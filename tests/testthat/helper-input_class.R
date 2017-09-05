test_input <- function(input, values = NULL, expectedValues = NULL, name = "myInput") {
  describe(paste("input", input$type), {
    it ("is correctly initialized", {
      env <- initEnv(parent.frame(), 1)
      input$init(name, env)

      expect_initialized(input)
      expect_equal(input$env, env)
      expect_equal(input$label, name)
      if(!"call" %in% class(input$value)){
        expect_equal(input$value, get(name, envir = env))
      } else {
        expect_equal(evalValue(input$value, parent.frame()), get(name, envir = env))
      }
      expect_is(input$params, "list")
    })

    it ("sets valid values", {
      for (i in seq_along(values)) {
        input$setValue(values[[i]])
        expect_equal(input$value, expectedValues[[i]])
        expect_equal(get(name, envir = input$env), expectedValues[[i]])
      }
    })
  })
}

expect_initialized <- function(input) {
  expect_is(input, "Input")
  expect(!emptyField(input$name) & !emptyField(input$env), "Input unitialized")
}
