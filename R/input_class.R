controlValueAndParams <- function(value, params, name, env){
  # have another variable name in env
  if(exists(name, envir = env)){
    # get value
    value_name <- get(name, envir = env)
    control <- function(value, name, env){
      # case of value / params of type name
      if(is.name(value)){
        # change name to new_name and assign current value
        new_name <- paste0(".tmp_mw_", name)
        assign(new_name, value_name, envir = env)
        # modify expr
        value <- eval(parse(text = paste0("substitute(", new_name, ")")))
        # case of value / params of type call
      } else if(is.call(value)){
        # change name to new_name and assign current value
        new_name <- paste0(".tmp_mw_", name)
        assign(new_name, value_name, envir = env)

        # modify expr
        char_call <- paste0(deparse(value), collapse = "\n")

        m <- gregexpr(paste0("((_.)[[:punct:]]|[[:space:]]|^){1}(",
                             name,
                             ")((_.)[[:punct:]]|[[:space:]]|$){1}"), char_call)

        if(m[[1]][1] != -1){
          matches_values <- unlist(regmatches(char_call, m))
          mlength <- attr(m[[1]], "match.length")
          mstart <- m[[1]][1:length(mlength)]
          if(mstart[1] != 1){
            final_value <- substring(char_call, 1, mstart[1]-1)
          } else {
            final_value <- ""
          }
          for(i in 1:length(mlength)){
            tmp <- matches_values[i]
            if(nchar(tmp) == (nchar(name) + 2)){
              final_value <- paste0(final_value, substring(char_call, mstart[i], mstart[i]), new_name,
                                    substring(char_call, mstart[i] + mlength[i] - 1, mstart[i] + mlength[i] - 1))
            } else if(nchar(tmp) == nchar(name)){
              final_value <- paste0(final_value, new_name)
            } else if(nchar(tmp) > (nchar(name) + 2)){
              final_value <- paste0(final_value, substring(char_call, mstart[i], mstart[i] + mlength[i] - 1))
            } else {
              if(substring(tmp, 1, nchar(name)) == name){
                final_value <- paste0(final_value, new_name,
                                      substring(char_call, mstart[i] + mlength[i] - 1, mstart[i] + mlength[i] - 1))
              } else {
                final_value <- paste0(final_value, substring(char_call, mstart[i], mstart[i]), new_name)
              }
            }
            if(i != length(mlength)){
              if((mstart[i] + mlength[i]) != mstart[i+1]){
                final_value <- paste0(final_value, substring(char_call, mstart[i] + mlength[i], mstart[i+1] - 1))
              }
            } else if((mstart[i] + mlength[i] - 1) != nchar(char_call)){
              final_value <- paste0(final_value, substring(char_call, mstart[i] + mlength[i], nchar(char_call)))
            }
          }
        } else {
          final_value <- char_call
        }
        value <- eval(parse(text = paste0("substitute(", final_value, ")")))
      } else {
        value
      }
      return(value)
    }

    # control value
    value <- control(value, name, env)

    # control params
    params <- lapply(params, function(x){control(x, name, env)})
  }

  return(list(value = value, params = params))
}

emptyField <- function(x) inherits(x, "uninitializedField")

evalParams <- function(params, env) {
  lapply(params, function(x) {
    tryCatch(eval(x, envir = env), silent = TRUE, error = function(e) {
      if(mwDebugMode()) message(e$message)
      NULL
    })
  })
}

evalValue <- function(value, env) {
  tryCatch(eval(value, envir = env), silent = TRUE, error = function(e) {
    if(mwDebugMode()) message(e$message);
    NULL
  })
}


# Private reference class representing an input.
Input <- setRefClass(
  "Input",
  fields = c("type", "name", "idFunc", "label", "value", "display", "params", "env",
             "validFunc", "htmlFunc", "htmlUpdateFunc",
             "lastParams", "changedParams", "valueHasChanged",
             "revDeps", "displayRevDeps", "value_expr"),

  methods = list(
    init = function(name, env) {
      "Set environment and default values"
      name <<- name
      env <<- env
      valueHasChanged <<- FALSE
      changedParams <<- list()
      revDeps <<- character()
      displayRevDeps <<- character()
      if (emptyField(label) || is.null(label)) label <<- name
      if (emptyField(idFunc)) {
        idFunc <<- function(oid, name) paste(oid, name, sep = "_")
      }

      ctrl_vp <- controlValueAndParams(value, params, name, env)
      value <<- ctrl_vp$value
      params <<- ctrl_vp$params

      if(is.call(value) | is.name(value)){
        assign(name, evalValue(value, parent.frame()), envir = env)
        value_expr <<- value
      } else {
        assign(name, value, envir = env)
        value_expr <<- NULL
      }

      lastParams <<- NULL
    },

    getID = function() {
      "Get the id of the input for the UI"
      gsub("[^a-zA-Z0-9]", "_", idFunc(get(".output", envir = env), name))
    },

    setValue = function(newValue, reactive = FALSE) {
      "Modify value of the input. If newValue is invalid, it sets a valid value"
      catIfDebug("Set value of ", getID())
      if(reactive & type == "sharedValue"){
        params$dynamic <<- FALSE
      }
      if (!emptyField(validFunc)) value <<- validFunc(evalValue(newValue, env), getParams())
      assign(name, value, envir = env)
      valueHasChanged <<- FALSE
      value
    },

    updateValue = function() {
      "Update value after a change in environment"
      catIfDebug("Update value of ", getID())
      oldValue <- value

      if (!emptyField(validFunc)){
        if(is.call(value_expr) | is.name(value_expr)){
          tmp_value <- evalValue(value_expr, env)
          if(is.null(tmp_value) & !is.call(oldValue) & !is.name(oldValue)) tmp_value <- oldValue
          value <<- validFunc(tmp_value, getParams())
        } else {
          tmp_value <- evalValue(value, env)
          if(is.null(tmp_value) & !is.call(oldValue) & !is.name(oldValue)) tmp_value <- oldValue
          value <<- validFunc(tmp_value, getParams())
        }
      }
      if (!identical(value, oldValue)) {
        valueHasChanged <<- TRUE
        assign(name, value, envir = env)
      }
      value
    },

    getParams = function() {
      "Get parameter values"
      oldParams <- lastParams
      lastParams <<- evalParams(params, env)

      for (n in names(lastParams)) {
        if (!is.null(oldParams[[n]]) &&
            !identical(lastParams[[n]], oldParams[[n]])) {
          changedParams[[n]] <<- lastParams[[n]]
        }
      }
      lastParams
    },

    getHTML = function(ns = NULL) {
      "Get the input HTML"
      if (emptyField(htmlFunc)) return(NULL)
      id <- getID()
      if (!is.null(ns)) id <- ns(id)
      shiny::conditionalPanel(
        condition = sprintf("input['%s_visible']", id),
        tags$div(
          style="display:none;",
          shiny::checkboxInput(paste0(id, "_visible"), "", value = evalValue(display, env))
        ),
        htmlFunc(id, label, value, lastParams, ns)
      )
    },

    updateHTML = function(session) {
      "Update the input HTML."
      if (emptyField(htmlUpdateFunc)) return()
      if (valueHasChanged || length(changedParams) > 0) {
        catIfDebug("Update HTML of ", getID(), "\n")
        htmlParams <- changedParams
        if (valueHasChanged) htmlParams$value <- value
        else if(length(changedParams) > 0){
          htmlParams$value <- validFunc(value, getParams())
        }
        htmlParams$session <- session
        htmlParams$inputId <- getID()
        do.call(htmlUpdateFunc, htmlParams)
        valueHasChanged <<- FALSE
        changedParams <<- list()
      }
    },

    show = function() {
      "print method"
      cat("input of class", type, "\n")
      if (type == "group") {
        for (n in names(value)) {
          cat("$", n, ": ", sep = "")
          value[[n]]$show()
        }
      }
    }
  )
)
