test_input <- function(input, values = NULL, expectedValues = NULL, name = "myInput") {
  describe(paste("input", input$type), {
    it ("is correctly initialized", {
      env <- initEnv(parent.frame(), 1)
      input$init(name, env)

      expect_equal(input$env, env)
      expect_equal(input$label, name)
      expect_equal(input$value, get(name, envir = env))
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
