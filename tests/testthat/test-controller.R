context("MWController class")

describe("MWController", {
  it("can be created with the result of initInputs()", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")))
    expr <- expression(paste(a, b))
    controller <- MWController(expr, inputs)
    controller$updateCharts()
    expect_is(controller$charts, "list")
    expect_length(controller$charts, 1)
    expect_equal(controller$charts[[1]], "a b")
  })

  it("creates multiple charts in comparison mode", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")), compare = "b",
                         ncharts = 3)
    expr <- expression(paste(a, b))
    controller <- MWController(expr, inputs)
    controller$updateCharts()
    expect_is(controller$charts, "list")
    expect_length(controller$charts, 3)
    for (o in controller$charts) expect_equal(o, "a b")
  })

  it ("does not update charts if values do not change", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")))
    expr <- expression(print("chart updated"))
    controller <- MWController(expr, inputs)
    expect_output(controller$updateCharts(), "chart updated")
    # Update a with different value
    expect_output(controller$setValue("a", "b"), "chart updated")
    # Update a with same value
    expect_silent(controller$setValue("a", "b"))
  })

  it("creates a copy that is completely autonomous", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")))
    expr <- expression(paste(a, b))
    controller1 <- MWController(expr, inputs)
    controller2 <- controller1$clone()

    controller1$setValue("a", "test")
    expect_equal(controller1$getValue("a"), "test")
    expect_equal(controller2$getValue("a"), "a")
  })

  it("accesses parameters of a given input", {
    inputs <- initInputs(list(a = mwSelect(c("a", "b", "c")), b = mwText("b")))
    expr <- expression(paste(a, b))
    controller <- MWController(expr, inputs)
    expect_equal(controller$getParams("a")$choices, c("a", "b", "c"))
  })

  it("generates server and ui functions", {
    inputs <- initInputs(list(a = mwSelect(c("a", "b", "c")), b = mwText("b")))
    expr <- expression(paste(a, b))
    controller <- MWController(expr, inputs)
    ui <- controller$getModuleUI()
    server <- controller$getModuleServer()
    expect_is(ui, "function")
    expect_is(server, "function")
    expect_equal(names(formals(server)), c("input", "output", "session", "..."))
  })
})
