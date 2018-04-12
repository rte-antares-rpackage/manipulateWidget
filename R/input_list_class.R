extractVarsFromExpr <- function(expr) {
  f <- function() {}
  body(f) <- expr
  codetools::findGlobals(f, merge = FALSE)$variables
}

# Private reference class used to update value and params of a set of inputs
# when the value of an input changes.
InputList <- setRefClass(
  "InputList",
  fields = c("inputs", "session", "names", "chartIds", "initialized"),
  methods = list(
    initialize = function(inputs, session = NULL) {
      "args:
       - inputs: list of initialized inputs
       - session: shiny session"
      inputList <- flattenInputs(unname(inputs))
      inputs <<- inputList
      names(inputs) <<- sapply(inputList, function(x) {x$getID()})
      names <<- sapply(inputList, function(x) x$name)
      chartIds <<- sapply(inputList, function(x) get(".id", envir = x$env))
      session <<- session
      initialized <<- FALSE

      # Set dependencies
      setDeps()
    },

    setDeps = function() {
      # Reset all deps
      for (input in inputs) {
        inputId <- input$getID()
        inputs[[inputId]]$revDeps <<- character(0)
        inputs[[inputId]]$displayRevDeps <<- character(0)
      }

      for (input in inputs) {
        inputId <- input$getID()
        deps <- getDeps(input)
        for (d in deps$params) {
          inputs[[d]]$revDeps <<- union(.self$inputs[[d]]$revDeps, inputId)
        }
        for (d in deps$display) {
          inputs[[d]]$displayRevDeps <<- union(.self$inputs[[d]]$displayRevDeps, inputId)
        }
      }
    },

    init = function() {
      if (!initialized) {
        update(forceDeps = TRUE)
        initialized <<- TRUE
      }
      return(.self)
    },

    isShared = function(name) {
      idx <- which(names == name)
      if (length(idx) == 0) stop("cannot find input ", name)
      any(chartIds[idx] == 0)
    },

    isVisible = function(name, chartId = 1, inputId = NULL) {
      i <- getInput(name, chartId, inputId)
      eval(i$display, envir = i$env)
    },

    updateHTMLVisibility = function(name, chartId = 1, inputId = NULL) {
      if (!is.null(session)) {
        input <- getInput(name, chartId, inputId)
        catIfDebug("Update visibility of", input$getID())
        shiny::updateCheckboxInput(
          session,
          paste0(input$getID(), "_visible"),
          value = eval(input$display, envir = input$env)
        )
      }
    },

    getDeps = function(input) {
      deps <- lapply(input$params, extractVarsFromExpr)
      deps <- do.call(c, deps)

      displayDeps <- extractVarsFromExpr(input$display)

      list(
        params = names(inputs)[names %in% deps],
        display = names(inputs)[names %in% displayDeps]
      )
    },

    getInput = function(name, chartId = 1, inputId = NULL) {
      if (!is.null(inputId)) {
        if (!inputId %in% names(inputs)) stop("cannot find input with id ", inputId)
        return(inputs[[inputId]])
      }
      idx <- which(names == name & chartIds %in% c(0, chartId))
      if (length(idx) == 0) stop("cannot find input with name ", name)
      inputs[[idx]]
    },

    addInput = function(input) {
      inputs[[input$getID()]] <<- input
      names <<- append(names, input$name)
      chartIds <<- append(chartIds, get(".id", envir = input$env))

      # Reset dependencies
      setDeps()
      if (initialized) update(forceDeps = TRUE)
    },

    removeInput = function(name, chartId = 0, inputId = NULL) {
      if (!is.null(inputId)) {
        if (!inputId %in% names(inputs)) stop("cannot find input with id ", inputId)
        idx <- which(names(inputs) == inputId)
      } else {
        idx <- which(names == name & chartIds == chartId)
      }

      if (length(idx) == 0) stop("cannot find input with name ", name)
      inputs[[idx]] <<- NULL
      names <<- names[-idx]
      chartIds <<- chartIds[-idx]

      setDeps()

      TRUE
    },

    getValue = function(name, chartId = 1, inputId = NULL) {
      getInput(name, chartId, inputId)$value
    },

    getValues = function(chartId = 1) {
      idx <- which(chartIds %in% c(0, chartId))
      res <- lapply(names[idx], function(n) getValue(n, chartId))
      names(res) <- names[idx]
      res
    },

    setValue = function(name, value, chartId = 1, inputId = NULL, reactive = FALSE) {
      input <- getInput(name, chartId, inputId)
      oldValue <- input$value
      res <- input$setValue(value, reactive = reactive)
      if (!identical(oldValue, res)) updateRevDeps(input)
      res
    },

    updateRevDeps = function(input, force = FALSE) {
      if (!initialized && !force) return()
      for (inputId in input$revDeps) {
        revDepInput <- getInput(inputId = inputId)
        if(!identical(revDepInput$value, revDepInput$updateValue())) {
          updateRevDeps(revDepInput)
        }
      }
      for (inputId in input$displayRevDeps) {
        updateHTMLVisibility(inputId = inputId)
      }
      updateHTML()
    },

    update = function(forceDeps = FALSE) {
      "Update all inputs"
      for (input in inputs) {
        if (!identical(input$value, input$updateValue())) updateRevDeps(input, force = forceDeps)
      }
      updateHTML()
    },

    updateHTML = function() {
      if (!is.null(session)) {
        for (input in inputs) {
          input$updateHTML(session)
        }
      }
    }
  )
)
