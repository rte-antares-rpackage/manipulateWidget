# Private reference class used to update value and params of a set of inputs
# when the value of an input changes.
InputList <- setRefClass(
  "InputList",
  fields = c("inputs", "session"),
  methods = list(
    initialize = function(inputs, session = NULL) {
      "args:
       - inputs: list of initialized inputs
       - session: shiny session"
      inputs <<- inputs
      names(inputs) <<- sapply(inputs, function(x) {x$getID()})
      session <<- session
      update()
    },

    setValue = function(inputId, newVal) {
      "Change the value of an input and update the other inputs
       args:
       - inputId: id of the input to update
       - newVal: new value for the input"
      inputs[[inputId]]$setValue(newVal)
      update()
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
