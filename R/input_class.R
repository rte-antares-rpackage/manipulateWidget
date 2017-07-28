emptyField <- function(x) inherits(x, "uninitializedField")

evalParams <- function(params, env) {
  lapply(params, function(x) {
    tryCatch(eval(x, envir = env), silent = TRUE, error = function(e) {NULL})
  })
}

# Private reference class representing an input.
Input <- setRefClass(
  "Input",
  fields = c("type", "name", "idFunc", "label", "value", "display", "params", "env",
             "validFunc", "htmlFunc", "htmlUpdateFunc",
             "lastParams", "changedParams", "valueHasChanged"),

  methods = list(
    init = function(name, env) {
      "Set environment and default values"
      name <<- name
      env <<- env
      valueHasChanged <<- FALSE
      changedParams <<- list()
      if (emptyField(label) || is.null(label)) label <<- name
      if (emptyField(idFunc)) {
        idFunc <<- function(oid, name) paste(oid, name, sep = "_")
      }
      assign(name, value, envir = env)
      lastParams <<- NULL
    },

    getID = function() {
      "Get the id of the input for the UI"
      gsub("[^a-zA-Z0-9]", "_", idFunc(get(".output", envir = env), name))
    },

    setValue = function(newValue) {
      "Modify value of the input. If newValue is invalid, it sets a valid value"
      if (!emptyField(validFunc)) value <<- validFunc(newValue, getParams())
      assign(name, value, envir = env)
      value
    },

    updateValue = function() {
      "Update value after a change in environment"
      oldValue <- value
      if (!emptyField(validFunc)) value <<- validFunc(value, getParams())
      if (!isTRUE(all.equal(value, oldValue))) {
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
            !isTRUE(all.equal(lastParams[[n]], oldParams[[n]]))) {
          changedParams[[n]] <<- lastParams[[n]]
        }
      }

      lastParams
    },

    getHTML = function() {
      "Get the input HTML"
      if (emptyField(htmlFunc)) return(NULL)

      id <- getID()
      shiny::conditionalPanel(
        condition = sprintf("input.%s_visible", id),
        tags$div(
          style="display:none;",
          shiny::checkboxInput(paste0(id, "_visible"), "", value = TRUE)
        ),
        htmlFunc(getID(), label, value, lastParams)
      )
    },

    updateHTML = function(session) {
      "Update the input HTML."
      if (emptyField(htmlUpdateFunc)) return()
      if (valueHasChanged || length(changedParams) > 0) {
        htmlParams <- changedParams
        if (valueHasChanged) htmlParams$value <- value
        htmlParams$session <- session
        htmlParams$inputId <- getID()
        do.call(htmlUpdateFunc, htmlParams)
        valueHasChanged <<- FALSE
        changedParams <<- list()
        catIfDebug("Update HTML of ", getID(), "\n")
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
