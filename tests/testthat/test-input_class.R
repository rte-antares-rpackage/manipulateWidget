context("Input class")

describe("Input", {
  inputTPL <- Input(
    type = "test",
    value = 0,
    params = list(
      min = expression(0),
      max = expression(10)
    ),
    display = expression(TRUE),
    validFunc = function(x, params) {
      min(max(params$min, x), params$max)
    },
    htmlFunc = htmlFuncFactory(shiny::numericInput)
  )

  # Basic check
  test_input(inputTPL$copy(), c(5, -20, 20), c(5, 0, 10))

  it("correctly updates value when environment changes", {
    myInput <- inputTPL$copy()
    myInput$params$min <- expression(minx)

    env <- initEnv(parent.frame(), 1)
    assign("minx", 0, envir = env)
    myInput$init("x", env)
    expect_equal(myInput$value, 0)

    assign("minx", 5, envir = env)
    expect_equal(myInput$updateValue(), 5)
    expect_equal(myInput$value, 5)
    expect_equal(get("x", envir = env), 5)
  })

  it("returns a valid ID (in a JS point of view)", {
    myInput <- inputTPL$copy()
    env <- initEnv(parent.frame(), 1)
    myInput$init("invalid.name", env)

    expect_equal(myInput$getID(), "output_1_invalid_name")
  })

})
