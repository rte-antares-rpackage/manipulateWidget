context("mwServer")

describe("onDone", {

  controls <- preprocessControls(
    list(x1 = mwText("value1"), x2 = mwText("value2")),
    env = parent.frame()
  )
  expr <- expression(combineWidgets(paste(x1, x2)))

  it ("stops the shiny gadget and returns a htmlwidget", {
    with_mock(
      `shiny::stopApp` = function(x) {
        print("Stop gadget")
        x
      },
      expect_output(res <- onDone(expr, controls), "Stop gadget"),
      expect_is(res, "htmlwidget"),
      expect_equal(length(res$widgets), 1),
      expect_equal(res$widgets[[1]], "value1 value2")
    )
  })

  it ("returns a combined widget if comparison", {
    compare <- list(x2 = list("a", "b", "c"), .n = 3)
    controls <- preprocessControls(
      list(x1 = mwText("value1"), x2 = mwText("value2")),
      compare = compare,
      env = parent.frame()
    )

    with_mock(
      `shiny::stopApp` = function(x) {
        print("Stop gadget")
        x
      },
      {
        expect_output(res <- onDone(expr, controls), "Stop gadget")
        expect_is(res, "combineWidgets")
        expect_equal(length(res$widgets), 3)
        for (i in 1:3) {
          expect_equal(res$widgets[[i]]$widgets[[1]], paste("value1", compare$x2[[i]]))
        }
      }
    )
  })

})
