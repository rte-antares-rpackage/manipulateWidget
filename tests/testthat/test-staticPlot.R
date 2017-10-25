context("Static plot & image")

describe("Static plot & image", {
  it("returns a combineWidget with both static plot and image", {
    c <- combineWidgets(
      staticPlot(hist(iris$Sepal.Length, breaks = 20), height = 300),
      staticImage(system.file("img/logo.png", package = "manipulateWidget"))
    )

    expect_is(c, "combineWidgets")
    expect_length(c$widgets, 2)
  })
})
