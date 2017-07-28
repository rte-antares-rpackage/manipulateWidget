context("onDone")

describe("onDone", {
  it ("stops the shiny gadget and returns a htmlwidget", {
    with_mock(
      `shiny::stopApp` = function(x) {
        print("Stop gadget")
        x
      },
      {
        inputs <- initInputs(list(x1 = mwText("value1"), x2 = mwSelect(1:3)))
        expr <- expression(combineWidgets(paste(x1, x2)))
        controller <- Controller(expr, inputs)

        expect_output(res <- onDone(controller), "Stop gadget")
        expect_is(res, "htmlwidget")
        expect_equal(length(res$widgets), 1)
        expect_equal(res$widgets[[1]], "value1 1")
      }
    )
  })
#
#   it ("returns a combined widget if comparison", {
#     with_mock(
#       `shiny::stopApp` = function(x) {
#         print("Stop gadget")
#         x
#       },
#       {
#         inputs <- initInputs(list(x1 = mwText("value1"), x2 = mwSelect(1:3)),
#                              compare = list(x2 = list(1, 2, 3)))
#         expr <- expression(paste(x1, x2))
#         controller <- Controller(expr, inputs)
#         expect_output(res <- onDone(controller), "Stop gadget")
#         expect_is(res, "combineWidgets")
#         expect_equal(length(res$widgets), 3)
#         for (i in 1:3) {
#           expect_equal(res$widgets[[i]], paste("value1", compare$x2[[i]]))
#         }
#       }
#     )
#   })

})
