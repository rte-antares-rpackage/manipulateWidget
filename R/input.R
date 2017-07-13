
Input <- setRefClass(
  "Input",
  fields = c("type", "name", "idFunc", "label", "value", "display", "params", "env",
             "validFunc", "htmlFunc", "htmlUpdateFunc",
             "lastParams", "changedParams", "valueHasChanged"),

  methods = list(
    init = function() {
      valueHasChanged <<- FALSE
      changedParams <<- list()
      if (emptyField(label) || is.null(label)) label <<- name
      assign(name, value, envir = env)
      lastParams <<- evalParams(params, env)
    },

    getID = function() {
      idFunc(get(".output", envir = env), name)
    },

    setValue = function(newValue) {
      if (!emptyField(validFunc)) value <<- validFunc(newValue, getParams())
      assign(name, value, envir = env)
      value
    },

    updateValue = function() {
      oldValue <- value
      if (!emptyField(validFunc)) value <<- validFunc(value, getParams())
      if (oldValue != value) {
        valueHasChanged <<- TRUE
        assign(name, value, envir = env)
      }
      value
    },

    getParams = function() {
      oldParams <- lastParams
      lastParams <<- evalParams(params, env)

      if(emptyField(changedParams)) changedParams <<- list()
      for (n in names(lastParams)) {
        if (lastParams[[n]] != oldParams[[n]]) changedParams[[n]] <<- lastParams[[n]]
      }

      lastParams
    },

    getHTML = function() {
      if (emptyField(htmlFunc)) return(NULL)
      else htmlFunc(getID(), label, value, lastParams)
    },

    updateHTML = function(session) {
      if (emptyField(htmlFunc)) return()
      if (valueHasChanged || length(changedParams) > 0) {
        htmlFunc(session, getID(), label, value, lastParams)
        valueHasChanged <<- FALSE
        changedParams <<- list()
      }
    }
  )
)

mwNumeric2 <- function(min, max, value, label = NULL, ..., .display = TRUE) {
  params <- prepareParams(min = min, max = max, value = value, label = label, ...)
  Input(
    type = "numeric",
    idFunc = function(oid, name) paste(oid, name, sep = "_"),
    value = value,
    params = params
  )
}

env <- new.env()
assign(".output", "output1", env)

a = Input(
  type = "numeric",
  name = "x",
  idFunc = function(oid, name) paste(oid, name, sep = "_"),
  label = "x",
  value = 5,
  params = list(max = expression(10), min = expression(0)),
  validFunc = function(x, params) {
    min(max(params$min, x), params$max)
  },
  env = env
)
a$init()

b = Input(
  type = "numeric",
  name = "y",
  idFunc = function(oid, name) paste(oid, name, sep = "_"),
  value = 1,
  params = list(max = expression(x), min = expression(0)),
  validFunc = function(x, params) {
    min(max(params$min, x), params$max)
  },
  env = env
)
b$init()

InputList <- setRefClass(
  "InputList",
  fields = c("inputs", "ids", "shiny"),
  methods = list(
    initialize = function(..., shinyMode = TRUE) {
      inputs <<- list(...)
      names(inputs) <<- sapply(inputs, function(x) {x$getID()})
      shiny <<- shinyMode
    },

    setValue = function(inputId, newVal) {
      inputs[[inputId]]$setValue(newVal)
      update()
    },

    update = function(session = NULL) {
      n <- 0
      while(TRUE) {
        n <- n + 1
        valueHasChanged <- sapply(inputs, function(x) {
          x$value != x$updateValue()
        })
        if (all(!valueHasChanged) | n > 10) break
      }

      if (!is.null(session)) {
        for (input in inputs) input$updateHTML(session)
      }
    }
  )
)

myInputs <- InputList(a, b)

emptyField <- function(x) inherits(x, "uninitializedField")
