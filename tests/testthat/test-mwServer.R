context("mwServer")

controlsSpec <- list(x1 = mwText("value1"), x2 = mwSelect(1:3))
expr <- expression(combineWidgets(paste(x1, x2)))
compare <- list(x2 = list(1, 2, 3), .n = 3)

# showHideControls #############################################################

describe("showHideControls", {
  visible <- list(x1_visible = TRUE, x2_visible = TRUE)
  display <- expression(list(x1 = x2 == 1, x2 = FALSE))
  controls <- preprocessControls(controlsSpec, env = parent.frame())
  controlsCompare <- preprocessControls(controlsSpec, compare, env = parent.frame())

  it("changes visibility of inputs", {

    with_mock(
      `shiny::updateCheckboxInput` = function(session, inputId, value) {
        visible[[inputId]] <<- value
      },
      {
        it ("Initial visibility", {
          showHideControls(display, controls$inputs, NULL, controls$env$ind[[1]])
          expect_true(visible$x1_visible)
          expect_false(visible$x2_visible)
        })

        it ("visibility after input update", {
          assign("x2", 2, envir = controls$env$ind[[1]])
          showHideControls(display, controls$inputs, NULL, controls$env$ind[[1]])
          expect_false(visible$x1_visible)
          expect_false(visible$x2_visible)
        })
      }
    )
  })
})

# onDone #######################################################################

describe("onDone", {
  controls <- preprocessControls(controlsSpec, env = parent.frame())
  controlsCompare <- preprocessControls(controlsSpec, compare, env = parent.frame())

  it ("stops the shiny gadget and returns a htmlwidget", {
    with_mock(
      `shiny::stopApp` = function(x) {
        print("Stop gadget")
        x
      },
      expect_output(res <- onDone(expr, controls), "Stop gadget"),
      expect_is(res, "htmlwidget"),
      expect_equal(length(res$widgets), 1),
      expect_equal(res$widgets[[1]], "value1 1")
    )
  })

  it ("returns a combined widget if comparison", {
    with_mock(
      `shiny::stopApp` = function(x) {
        print("Stop gadget")
        x
      },
      {
        expr <- expression(paste(x1, x2))
        expect_output(res <- onDone(expr, controlsCompare), "Stop gadget")
        expect_is(res, "combineWidgets")
        expect_equal(length(res$widgets), 3)
        for (i in 1:3) {
          expect_equal(res$widgets[[i]], paste("value1", compare$x2[[i]]))
        }
      }
    )
  })

})
