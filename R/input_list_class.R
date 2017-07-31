# Private reference class used to update value and params of a set of inputs
# when the value of an input changes.
InputList <- setRefClass(
  "InputList",
  fields = c("inputs", "session", "names", "chartIds"),
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

      # Set dependencies
      for (input in inputList) {
        inputId <- input$getID()
        revdeps <- getRevDeps(input)
        for (d in revdeps) {
          inputs[[d]]$deps <<- c(inputList[[d]]$deps, inputId)
        }
      }

      update()
    },

    isShared = function(name) {
      idx <- which(names == name)
      if (length(idx) == 0) stop("cannot find input ", name)
      any(chartIds[idx] == 0)
    },

    isVisible = function(name, chartId = 1) {
      i <- getInput(name, chartId)
      eval(i$display, envir = i$env)
    },

    getRevDeps = function(input) {
      deps <- c()
      for (p in input$params) {
        f <- function() {}
        body(f) <- p
        deps <- union(deps, codetools::findGlobals(f, merge = FALSE)$variables)
      }
      names(inputs)[names %in% deps]
    },

    getInput = function(name, chartId = 1, inputId = NULL) {
      if (!is.null(inputId)) {
        if (!inputId %in% names(inputs)) stop("cannot find input with id", inputId)
        return(inputs[[inputId]])
      }
      idx <- which(names == name & chartIds %in% c(0, chartId))
      if (length(idx) == 0) stop("cannot find input with name", name)
      inputs[[idx]]
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

    setValue = function(name, value, chartId = 1, inputId = NULL) {
      input <- getInput(name, chartId, inputId)
      res <- input$setValue(value)
      updateDeps(input)
      res
    },

    updateDeps = function(input) {
      for (inputId in input$deps) {
        depInput <- getInput(inputId = inputId)
        if(!isTRUE(all.equal(depInput$value, depInput$updateValue()))) {
          updateDeps(depInput)
        }
      }
      updateHTML()
    },

    update = function() {
      "Update all inputs"
      n <- 0
      while(TRUE) {
        n <- n + 1
        valueHasChanged <- sapply(inputs, function(x) {
          #if (x$type == "group") return(FALSE)
          !isTRUE(all.equal(x$value, x$updateValue()))
        })
        if (all(!valueHasChanged) | n > 10) break
      }
      updateHTML()
    },

    updateHTML = function() {
      if (!is.null(session)) {
        for (input in inputs) {
          shiny::updateCheckboxInput(
            session,
            paste0(input$getID(), "_visible"),
            value = eval(input$display, envir = input$env)
          )
          input$updateHTML(session)
        }
      }
    }
  )
)
