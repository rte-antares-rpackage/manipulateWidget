context("Controller class")

describe("Controller", {
  it("can be created with the result of initInputs()", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")))
    expr <- expression(paste(a, b))
    controller <- Controller(expr, inputs)
    expect_is(controller$charts, "list")
    expect_length(controller$charts, 1)
    expect_equal(controller$charts[[1]], "a b")
  })

  it("can create multiple charts in comparison mode", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")), compare = "b",
                         ncharts = 3)
    expr <- expression(paste(a, b))
    controller <- Controller(expr, inputs)
    expect_is(controller$charts, "list")
    expect_length(controller$charts, 3)
    for (o in controller$charts) expect_equal(o, "a b")
  })

  it ("does not update charts if values do not change", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")))
    expr <- expression(print("chart updated"))
    expect_output(controller <- Controller(expr, inputs), "chart updated")
    # Update a with different value
    expect_output(controller$setValue("a", "b"), "chart updated")
    # Update a with same value
    expect_silent(controller$setValue("a", "b"))
  })
})
