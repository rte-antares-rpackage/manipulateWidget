context("MWController class")

describe("MWController", {
  it("can be created with the result of initInputs()", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")))
    expr <- expression(paste(a, b))
    controller <- MWController(expr, inputs)$init()
    controller$updateCharts()
    expect_is(controller$charts, "list")
    expect_length(controller$charts, 1)
    expect_equal(controller$charts[[1]]$widgets[[1]], "a b")
  })

  it("creates multiple charts in comparison mode", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")), compare = "b",
                         ncharts = 3)
    expr <- expression(paste(a, b))
    controller <- MWController(expr, inputs)$init()
    controller$updateCharts()
    expect_is(controller$charts, "list")
    expect_length(controller$charts, 3)
    for (o in controller$charts) expect_equal(o$widgets[[1]], "a b")
  })

  it ("does not update charts if values do not change", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")))
    expr <- expression(print("chart updated"))
    expect_output(controller <- MWController(expr, inputs)$init(), "chart updated")
    expect_output(controller$updateCharts(), "chart updated")
    # Update a with different value
    expect_output(controller$setValue("a", "b"), "chart updated")
    # Update a with same value
    expect_silent(controller$setValue("a", "b"))
  })

  it("creates a copy that is completely autonomous", {
    inputs <- initInputs(list(a = mwText("a"), b = mwText("b")))
    expr <- expression(paste(a, b))
    controller1 <- MWController(expr, inputs)$init()
    controller2 <- controller1$clone()

    controller1$setValue("a", "test")
    expect_equal(controller1$getValue("a"), "test")
    expect_equal(controller2$getValue("a"), "a")
    expect_true(controller2$initialized)
    expect_true(controller2$inputList$initialized)
  })

  it("accesses parameters of a given input", {
    inputs <- initInputs(list(a = mwSelect(c("a", "b", "c")), b = mwText("b")))
    expr <- expression(paste(a, b))
    controller <- MWController(expr, inputs)$init()
    expect_equal(controller$getParams("a")$choices, c("a", "b", "c"))
  })

  it("generates server and ui functions", {
    inputs <- initInputs(list(a = mwSelect(c("a", "b", "c")), b = mwText("b")))
    expr <- expression(paste(a, b))
    controller <- MWController(expr, inputs)$init()
    ui <- controller$getModuleUI()
    server <- controller$getModuleServer()
    expect_is(ui, "function")
    expect_is(server, "function")
    expect_equal(names(formals(server)), c("input", "output", "session", "..."))
  })

  it("does not update values or create charts until it is initialized", {
    inputs <- initInputs(list(x = mwSlider(0, 10, 5), y = mwSlider(x, 10, 0)))
    expr <- expression(paste(x, y))
    controller <- MWController(expr, inputs)
    expect_length(controller$charts, 0)
    expect_equal(controller$getValue("y"), 0)
    controller$setValue("x", 3)
    expect_length(controller$charts, 0)
    expect_equal(controller$getValue("y"), 0)
    controller$init()
    expect_length(controller$charts, 1)
    expect_equal(controller$charts[[1]]$widgets[[1]], "3 3")
    expect_equal(controller$getValue("y"), 3)
  })
})

describe("summary.MWController", {
  it("prints information about controller", {
    controller <- manipulateWidget(
      d$value,
      a = mwSelect(c("a", "b", "c")),
      b = mwSelect(c("a", "b", "c"), "b"),
      c = mwSelect(c("a", "b", "c"), c("a", "b"), multiple = TRUE),
      d = mwSharedValue(data.frame(value = 1)),
      .runApp = FALSE
    )
    expect_output(summary(controller), "List of inputs")
    # Indicates NULL values
    expect_output(summary(controller), "NULL")
    # paste values if multiple values
    expect_output(summary(controller), "a, b")
    # for complicated objects, indicates the class of object
    controller$init()
    expect_output(summary(controller), "data.frame")
  })
})
