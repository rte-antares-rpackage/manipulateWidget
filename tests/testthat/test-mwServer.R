# context("mwServer")
#
# # showHideControls #############################################################
#
# describe("showHideControls", {
#   visible <- list(x1_visible = TRUE, x2_visible = TRUE)
#   controlsSpec <- list(x1 = mwText("value1", .display = x2 == 1),
#                        x2 = mwSelect(1:3, .display = FALSE))
#   controls <- preprocessControls(controlsSpec, env = parent.frame(), ncharts = 1)
#
#   it("changes visibility of inputs", {
#
#     with_mock(
#       `shiny::updateCheckboxInput` = function(session, inputId, value) {
#         visible[[inputId]] <<- value
#       },
#       {
#         it ("Initial visibility", {
#           showHideControls(controls$desc, NULL, controls$env$ind[[1]])
#           expect_true(visible$x1_visible)
#           expect_false(visible$x2_visible)
#         })
#
#         it ("visibility after input update", {
#           assign("x2", 2, envir = controls$env$ind[[1]])
#           showHideControls(controls$desc, NULL, controls$env$ind[[1]])
#           expect_false(visible$x1_visible)
#           expect_false(visible$x2_visible)
#         })
#       }
#     )
#   })
# })
#
# # updateControls ###############################################################
#
# describe("updateControls", {
#   controlsSpec <- list(x1 = mwNumeric(0, min = x2), x2 = mwSelect(0:3))
#   controls <- preprocessControls(controlsSpec, env = parent.frame(), ncharts = 1)
#   desc <- controls$desc
#   env <- controls$env$ind[[1]]
#
#   with_mock(
#     `manipulateWidget:::getUpdateInputFun` = function(type) {
#       function(...) print(paste("update", type))
#     },
#     {
#       it ("updates control parameters", {
#         assign("x2", 1L, envir = env)
#         expect_output(desc <<- updateControls(desc, NULL, env),
#                       "update numeric")
#         expect_equal(desc$currentParams[[2]]$min, 1)
#       })
#       it ("does nothing if parameters are not modified", {
#         expect_silent(desc <<- updateControls(desc, NULL, env))
#         expect_equal(desc$currentParams[[2]]$min, 1)
#       })
#     }
#   )
# })
#
# # onDone #######################################################################
#
# describe("onDone", {
#   controls <- preprocessControls(controlsSpec, env = parent.frame(), ncharts = 1)
#   controlsCompare <- preprocessControls(controlsSpec, compare, env = parent.frame(), ncharts = 3)
#
#   it ("stops the shiny gadget and returns a htmlwidget", {
#     with_mock(
#       `shiny::stopApp` = function(x) {
#         print("Stop gadget")
#         x
#       },
#       expect_output(res <- onDone(expr, controls), "Stop gadget"),
#       expect_is(res, "htmlwidget"),
#       expect_equal(length(res$widgets), 1),
#       expect_equal(res$widgets[[1]], "value1 1")
#     )
#   })
#
#   it ("returns a combined widget if comparison", {
#     with_mock(
#       `shiny::stopApp` = function(x) {
#         print("Stop gadget")
#         x
#       },
#       {
#         expr <- expression(paste(x1, x2))
#         expect_output(res <- onDone(expr, controlsCompare), "Stop gadget")
#         expect_is(res, "combineWidgets")
#         expect_equal(length(res$widgets), 3)
#         for (i in 1:3) {
#           expect_equal(res$widgets[[i]], paste("value1", compare$x2[[i]]))
#         }
#       }
#     )
#   })
#
# })
