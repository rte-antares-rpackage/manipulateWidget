context("initInputs")

# Helper function that checks the structure of the object returned by initInputs.
# It returns the said object for further testing
test_structure <- function(inputs, compare = NULL, ncharts = 1) {
  res <- initInputs(inputs, compare = compare, ncharts = ncharts)

  inputList <- filterAndInitInputs(inputs, c(), TRUE, initEnv(parent.frame(), 1))
  inputList <- flattenInputs(inputList)

  expect_is(res, "Model")
  expect_named(res$getRefClass()$fields(), c("envs", "inputs", "inputList", "ncharts"))
  expect_is(res$envs, "list")
  expect_named(res$envs, c("shared", "ind"))
  expect_is(res$envs$ind, "list")
  expect_length(res$envs$ind, ncharts)

  expect_is(res$inputs, "list")
  expect_named(res$inputs, c("shared", "ind"))
  expect_is(res$inputs$ind, "list")
  expect_length(res$inputs$ind, ncharts)

  expect_is(res$inputList, "InputList")
  expectedLength <- length(inputList) + length(compare) * (ncharts - 1)
  # inexact when one tries to compare grouped inputs
  expect_length(res$inputList$inputs, expectedLength)

  sharedInputs <- setdiff(names(inputList), names(compare))

  if (length(sharedInputs) == 0) expected_names <- c()
  else expected_names <- paste0("shared_", sharedInputs)

  if (length(compare) > 0) {
    for (i in seq_len(ncharts)) {
      expected_names <- append(
        expected_names,
        paste0("output_", i, "_", names(compare))
      )
    }
  }

  expect_true(all(expected_names %in% names(res$inputList$inputs)))

  res
}

describe("initInputs", {
  it("generates correct structure", {
    test_structure(list(a = mwText(), b = mwText()))
  })

  it("handles grouped inputs", {
    test_structure(list(grp = mwGroup(a = mwText(), b = mwText())))
  })

  it("still works if ncharts > 1", {
    test_structure(list(grp = mwGroup(a = mwText(), b = mwText())), ncharts = 2)
  })

  it("prepares inputs for comparison", {
    test_structure(list(a = mwText(), b = mwText()), ncharts = 2,
                   compare = list(a = NULL))
  })

  it("prepares inputs for comparison with different initial values", {
    res <- test_structure(list(a = mwText(), b = mwText()), ncharts = 2,
                          compare = list(a = c("a", "b")))

  })

  it("throws errors if inputs are not inputs or not named", {
    expect_error(initInputs(list(mwText())), "All arguments need to be named.")
    expect_error(initInputs(list(a = 1)), "All arguments need to be Input objects.")
  })
})

describe("Model Class", {
  it ("shares an input", {
    model <- test_structure(list(x = mwSlider(0, 10, 5), y = mwSlider(x, 10, 0)),
                            ncharts = 2, compare = list(x = list(5, 0), y = NULL))

    model$inputList$init()
    model$shareInput("x")

    expect_length(model$inputs$shared, 1)
    expect_named(model$inputs$shared, "x")

    for (i in 1:2) {
      expect_length(model$inputs$ind[[i]], 1)
      expect_named(model$inputs$ind[[i]], c("y"))
    }

    expect_equal(model$envs$shared$x, 5)
    for (i in 1:2) {
      expect_null(model$envs$ind[[i]]$x)
    }
  })

  it ("unshares an input", {
    model <- test_structure(list(a = mwText(), b = mwText("test")), ncharts = 2,
                            compare = list(a = NULL))

    model$unshareInput("b")
    expect_length(model$inputs$shared, 0)

    for (i in 1:2) {
      expect_length(model$inputs$ind[[i]], 2)
      expect_named(model$inputs$ind[[i]], c("a", "b"))
    }

    expect_null(model$envs$shared$b)
    for (i in 1:2) {
      expect_equal(model$envs$ind[[i]]$b, "test")
    }

    model$inputList$setValue("b", "test2", chartId = 1)
    expect_equal(model$envs$ind[[1]]$b, "test2")
    expect_equal(model$envs$ind[[2]]$b, "test")

  })

  it ("ads a chart", {

  })

  it ("ads many charts", {

  })

  it ("removes many charts", {

  })
})
