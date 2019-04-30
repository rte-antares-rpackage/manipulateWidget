extractVarsFromExpr <- function(expr) {
  f <- function() {}
  body(f) <- expr
  codetools::findGlobals(f, merge = FALSE)$variables
}

# Private reference class used to update value and params of a set of inputs
# when the value of an input changes.
InputList <- setRefClass(
  "InputList",
  fields = c("session", "initialized", "inputTable"),
  methods = list(
    initialize = function(inputs, session = NULL, flatten = TRUE) {
      "args:
       - inputs: list of initialized inputs
       - session: shiny session"
      if (flatten) inputList <- flattenInputs(unname(inputs))
      else inputList <- inputs

      inputTable <<- data.frame(
        row.names = sapply(inputList, function(x) {x$getID()}),
        name = sapply(inputList, function(x) x$name),
        chartId = sapply(inputList, function(x) get(".id", envir = x$env)),
        type = sapply(inputList, function(x) x$type),
        input = I(inputList)
      )

      session <<- session
      initialized <<- FALSE

      # Set dependencies
      setDeps()
    },

    setDeps = function() {
      # Reset all deps
      for (id in row.names(inputTable)) {
        .self[id]$resetDeps()
      }

      for (input in inputTable$input) {
        inputId <- input$getID()
        deps <- getDeps(input)
        for (d in deps$params) {
          .self[d]$addDeps(newRevDeps = inputId)
        }
        for (d in deps$display) {
          .self[d]$addDeps(newDisplayRevDeps = inputId)
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
      idx <- which(inputTable$name == name)
      if (length(idx) == 0) stop("cannot find input ", name)
      any(inputTable$chartId[idx] == 0)
    },

    shared = function() {
      inputTable$name[inputTable$chartId == 0]
    },

    unshared = function() {
      unique(inputTable$name[inputTable$chartId != 0])
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
      chartId <- get(".id", input$env)

      deps <- lapply(input$params, extractVarsFromExpr)
      deps <- do.call(c, deps)

      displayDeps <- extractVarsFromExpr(input$display)

      list(
        params = row.names(inputTable)[inputTable$name %in% deps & inputTable$chartId %in% c(0, chartId)],
        display = row.names(inputTable)[inputTable$name %in% displayDeps & inputTable$chartId %in% c(0, chartId)]
      )
    },

    getInput = function(name, chartId = 1, inputId = NULL) {
      if (!is.null(inputId)) {
        if (!inputId %in% row.names(inputTable)) {
          stop("cannot find input with id ", inputId)
        }
        return(.self[inputId])
      }
      idx <- which(inputTable$name == name & inputTable$chartId %in% c(0, chartId))
      if (length(idx) == 0) stop("cannot find input with name ", name)
      .self[idx]
    },

    addInputs = function(x) {
      newInputs <- data.frame(
        row.names = sapply(x, function(i) i$getID()),
        name = sapply(x, function(i) i$name),
        chartId = sapply(x, function(i) get(".id", envir = i$env)),
        type = sapply(x, function(i) i$type),
        input = I(x)
      )

      inputTable <<- rbind(inputTable, newInputs)

      # Reset dependencies
      setDeps()
      if (initialized) update(forceDeps = TRUE)
    },

    removeInput = function(name, chartId = 0, inputId = NULL) {
      if (!is.null(inputId)) {
        if (!inputId %in% row.names(inputTable)) stop("cannot find input with id ", inputId)
        idx <- which(row.names(inputTable) == inputId)
      } else {
        idx <- which(inputTable$name == name & inputTable$chartId == chartId)
      }

      if (length(idx) == 0) stop("cannot find input with name ", name)
      if (length(idx) > 1) stop("Something wrong with input", name)
      inputTable <<- inputTable[-idx,]

      setDeps()

      TRUE
    },

    getValue = function(name, chartId = 1, inputId = NULL) {
      getInput(name, chartId, inputId)$value
    },

    getValues = function(chartId = 1) {
      idx <- which(inputTable$chartId %in% c(0, chartId) & inputTable$type != "group")
      res <- lapply(inputTable$input[idx], function(i) i$value)
      names(res) <- inputTable$name[idx]
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
      for (input in inputTable$input) {
        if (!identical(input$value, input$updateValue())) updateRevDeps(input, force = forceDeps)
      }
      updateHTML()
    },

    updateHTML = function() {
      if (!is.null(session)) {
        for (input in inputTable$input) {
          input$updateHTML(session)
        }
      }
    },

    show = function() {
      print(inputTable)
    }
  )
)

`[.InputList` <- function(x, i, j, ...) {
  if (missing(j) & !missing(i)) {
    if (length(i) == 1) return (x[i, "input"][[1]])
    else return(x[i, "input"])
  }
  x$inputTable[i, j, ...]
}
