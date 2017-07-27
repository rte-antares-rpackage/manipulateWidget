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

    getInput = function(name, chartId = 1) {
      idx <- which(names == name & chartIds %in% c(0, chartId))
      if (length(idx) == 0) stop("cannot find input ", name)
      inputs[[idx]]
    },

    getValue = function(name, chartId = 1) {
      getInput(name, chartId)$value
    },

    getValues = function(chartId = 1) {
      idx <- which(chartIds %in% c(0, chartId))
      res <- lapply(names[idx], function(n) getValue(n, chartId))
      names(res) <- names[idx]
      res
    },

    setValue = function(name, value, chartId = 1) {
      res <- getInput(name, chartId)$setValue(value)
      update()
      res
    },

    setValueById = function(inputId, value) {
      "Change the value of an input and update the other inputs
       args:
       - inputId: id of the input to update
       - value: new value for the input"
      res <- inputs[[inputId]]$setValue(value)
      update()
      res
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

      if (!is.null(session)) {
        for (input in inputs) input$updateHTML(session)
      }
    }
  )
)
