context("manipulateWidget")

describe("manipulateWidget", {
  it("creates two charts when .compare is a character vector", {
    c <- manipulateWidget(
      paste(a, b),
      a = mwSelect(c("a", "b", "c")),
      b = mwText("test"),
      .compare = "a"
    )
    expect_equal(c$ncharts, 2)
    expect_equal(c$getValue("a", 1), "a")
    expect_equal(c$getValue("a", 2), "a")
  })

  it("creates two charts when .compare is a named list with null values", {
    c <- manipulateWidget(
      paste(a, b),
      a = mwSelect(c("a", "b", "c")),
      b = mwText("test"),
      .compare = list(a = NULL)
    )
    expect_equal(c$ncharts, 2)
    expect_equal(c$getValue("a", 1), "a")
    expect_equal(c$getValue("a", 2), "a")
  })

  it("sets different values when .compare is a named list with non null values", {
    c <- manipulateWidget(
      paste(a, b),
      a = mwSelect(c("a", "b", "c")),
      b = mwText("test"),
      .compare = list(a = list("a", "b"))
    )
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
      .compareOpts = compareOptions(ncharts = 3)
    )
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
      y = mwSlider(0, x, 4)
    )
    expect_equal(c$getParams("y")$max, 5)
    c$setValue("x", 3)
    expect_equal(c$getParams("y")$max, 3)
    expect_equal(c$getValue("y"), 3)
  })

  it ("conditionally shows/hides inputs", {
    c <- manipulateWidget(
      x + y,
      x = mwSlider(0, 10, 0),
      y = mwSlider(0, 10, 0, .display = x < 5)
    )
    expect_true(c$isVisible("y"))
    c$setValue("x", 6)
    expect_true(!c$isVisible("y"))
  })
})
