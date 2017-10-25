context("Static plot & image")

describe("Static plot & image", {
  it("returns a combineWidget with both static plot and image", {

    tmp_png <- tempfile(fileext = ".png")
    png(file = tmp_png, bg = "transparent")
    plot(1:10)
    dev.off()

    c <- combineWidgets(
      staticPlot(hist(iris$Sepal.Length, breaks = 20), height = 300),
      staticImage(tmp_png)
    )

    expect_is(c, "combineWidgets")
    expect_length(c$widgets, 2)
  })
})
