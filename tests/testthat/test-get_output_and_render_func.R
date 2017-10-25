context("getOutputAndRenderFunc")

describe("getOutputAndRenderFunc", {
  if(require("leaflet")){
    it ("returns output and render functions of a widget", {
      widget <- leaflet()
      res <- getOutputAndRenderFunc(widget)
      expect_named(res, c("outputFunc", "renderFunc", "useCombineWidgets"))
      expect_equal(res$outputFunc, leaflet::leafletOutput)
      expect_equal(res$renderFunc, leaflet::renderLeaflet)
      expect_equal(res$useCombineWidgets, FALSE)
    })

    it ("returns combineWidgets output and render functions if x is not an htmlwidget", {
      res <- getOutputAndRenderFunc("test")
      expect_named(res, c("outputFunc", "renderFunc", "useCombineWidgets"))
      expect_equal(res$outputFunc, combineWidgetsOutput)
      expect_equal(res$renderFunc, renderCombineWidgets)
      expect_equal(res$useCombineWidgets, TRUE)
    })
  }
})
