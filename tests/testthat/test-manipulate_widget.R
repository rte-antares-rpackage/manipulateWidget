context("manipulateWidget")

describe("manipulateWidget", {
  it("returns an uninitialized MWController in a non interactive situation", {
    c <- manipulateWidget(
      paste(a, b),
      a = mwSelect(c("a", "b", "c")),
      b = mwText("test"),
      .compare = "a", .runApp = FALSE
    )
    expect_true(!c$initialized)
  })

  it("creates two charts when .compare is a character vector", {
    c <- manipulateWidget(
      paste(a, b),
      a = mwSelect(c("a", "b", "c")),
      b = mwText("test"),
      .compare = "a", .runApp = FALSE
    )
    c$init()
    expect_equal(c$ncharts, 2)
    expect_equal(c$getValue("a", 1), "a")
    expect_equal(c$getValue("a", 2), "a")
  })

  it("creates two charts when .compare is a named list with null values", {
    c <- manipulateWidget(
      paste(a, b),
      a = mwSelect(c("a", "b", "c")),
      b = mwText("test"),
      .compare = list(a = NULL), .runApp = FALSE
    )
    c$init()
    expect_equal(c$ncharts, 2)
    expect_equal(c$getValue("a", 1), "a")
    expect_equal(c$getValue("a", 2), "a")
  })

  it("sets different values when .compare is a named list with non null values", {
    c <- manipulateWidget(
      paste(a, b),
      a = mwSelect(c("a", "b", "c")),
      b = mwText("test"),
      .compare = list(a = list("a", "b")), .runApp = FALSE
    )
    c$init()
    expect_equal(c$ncharts, 2)
    expect_equal(c$getValue("a", 1), "a")
    expect_equal(c$getValue("a", 2), "b")
    expect_equal(c$charts[[1]]$widgets[[1]], "a test")
    expect_equal(c$charts[[2]]$widgets[[1]], "b test")
  })

  it ("creates more than two charts", {
    c <- manipulateWidget(
      paste(a, b),
      a = mwSelect(c("a", "b", "c")),
      b = mwText("test"),
      .compare = list(a = list("a", "b", "c")),
      .compareOpts = compareOptions(ncharts = 3), .runApp = FALSE
    )
    c$init()
    expect_equal(c$ncharts, 3)
    expect_equal(c$getValue("a", 1), "a")
    expect_equal(c$getValue("a", 2), "b")
    expect_equal(c$getValue("a", 2), "b")
    expect_equal(c$charts[[1]]$widgets[[1]], "a test")
    expect_equal(c$charts[[2]]$widgets[[1]], "b test")
    expect_equal(c$charts[[3]]$widgets[[1]], "c test")
  })

  it ("updates dynamic inputs", {
    c <- manipulateWidget(
      x + y,
      x = mwSlider(0, 10, 5),
      y = mwSlider(0, x, 4), .runApp = FALSE
    )
    c$init()
    expect_equal(c$getParams("y")$max, 5)
    c$setValue("x", 3)
    expect_equal(c$getParams("y")$max, 3)
    expect_equal(c$getValue("y"), 3)
  })

  it ("conditionally shows/hides inputs", {
    c <- manipulateWidget(
      x + y,
      x = mwSlider(0, 10, 0),
      y = mwSlider(0, 10, 0, .display = x < 5), .runApp = FALSE
    )
    c$init()
    expect_true(c$isVisible("y"))
    c$setValue("x", 6)
    expect_true(!c$isVisible("y"))
  })

  it ("shares values between inputs and outputs", {
    c <- manipulateWidget(
      x2 + y,
      x = mwSlider(0, 10, 5),
      x2 = mwSharedValue(x * 2),
      y = mwSlider(0, x2, 0), .runApp = FALSE
    )
    c$init()
    expect_equal(c$getParams("y")$max, 10)
    expect_equal(c$charts[[1]]$widgets[[1]], 10)
    c$setValue("x", 8)
    expect_equal(c$getValue("x2"), 16)
    expect_equal(c$getParams("y")$max, 16)
    expect_equal(c$charts[[1]]$widgets[[1]], 16)

  })

  it ("modifies a sharedInput when it is not dynamic", {
    c <- manipulateWidget(
      x2 + y,
      x = mwSlider(0, 10, 5),
      x2 = mwSharedValue(1),
      x3 = mwSharedValue(x + x2),
      y = mwSlider(0, x2, 0), .runApp = FALSE
    )
    c$init()
    expect_equal(c$getParams("y")$max, 1)
    expect_equal(c$charts[[1]]$widgets[[1]], 1)
    c$setValue("x2", 8)
    expect_equal(c$getValue("x2"), 8)
    expect_equal(c$getValue("x3"), 13)
    expect_equal(c$getParams("y")$max, 8)
    expect_equal(c$charts[[1]]$widgets[[1]], 8)
    c$setValue("x3", 10) # Dynamic shared input. Should not have any effect
    expect_equal(c$getValue("x3"), 13)
  })
})
