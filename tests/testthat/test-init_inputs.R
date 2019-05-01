context("initInputs")

# Helper function that checks the structure of the object returned by initInputs.
# It returns the said object for further testing
test_structure <- function(inputs, compare = NULL, ncharts = 1) {
  res <- initInputs(inputs, compare = compare, ncharts = ncharts)

  #initAllInputs(inputs, initEnv(parent.frame(), 1))
  inputList <- lapply(unname(inputs), function(input) input$getInputs())
  inputList <- do.call(c, inputList)

  expect_is(res, "Model")
  expect_named(res$getRefClass()$fields(), c("envs", "inputList", "ncharts", "hierarchy"))
  expect_is(res$envs, "list")
  expect_named(res$envs, c("shared", "ind"))
  expect_is(res$envs$ind, "list")
  expect_length(res$envs$ind, ncharts)

  expect_is(res$inputList, "InputList")
  expectedLength <- length(inputList) + length(compare) * (ncharts - 1)
  # inexact when one tries to compare grouped inputs
  expect_equal(nrow(res$inputList$inputTable), expectedLength)

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

  expect_true(all(expected_names %in% row.names(res$inputList$inputTable)))

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
    newInput <- model$shareInput("x")
    expect_equal(newInput, "shared_x")

    expect_silent(model$inputList$getInput("x", 0))
    expect_error(model$inputList$getInput("y", 0), "cannot find input")

    for (i in 1:2) {
      expect_silent(model$inputList$getInput("y", i))
    }

    expect_equal(model$envs$shared$x, 5)
    for (i in 1:2) {
      expect_null(model$envs$ind[[i]]$x)
    }
  })

  it ("unshares an input", {
    model <- test_structure(list(a = mwText(), b = mwText("test")), ncharts = 2,
                            compare = list(a = NULL))

    newInputs <- model$unshareInput("b")
    expect_equal(newInputs, c("output_1_b", "output_2_b"))

    expect_error(model$inputList$getInput("b", 0), "cannot find input")

    for (i in 1:2) {
      expect_silent(model$inputList$getInput("a", i))
      expect_silent(model$inputList$getInput("b", i))
    }

    for (i in 1:2) {
      expect_equal(model$envs$ind[[i]]$b, "test")
    }

    model$inputList$setValue("b", "test2", chartId = 1)
    expect_equal(model$envs$ind[[1]]$b, "test2")
    expect_equal(model$envs$ind[[2]]$b, "test")

  })

  it ("shares a group of inputs", {
    model <- test_structure(list(grp = mwGroup(a = mwText(), b = mwText())),
                            ncharts = 2, compare = list(grp = NULL, a = NULL, b = NULL))

    model$inputList$init()
    newInput <- model$shareInput("grp")
    expect_equal(sort(newInput), c("shared_a", "shared_b", "shared_grp"))

    expect_silent(model$inputList$getInput("grp", 0))
    expect_silent(model$inputList$getInput("a", 0))
    expect_silent(model$inputList$getInput("b", 0))

    expect_named(model$inputList$getInput("grp", 0)$value, c("a", "b"))

    for (i in 1:2) {
      expect_error(model$inputList$getInput(inputId = sprintf("output_%s_grp",i)), "cannot find input")
      expect_error(model$inputList$getInput(inputId = sprintf("output_%s_grp",i)), "cannot find input")
      expect_error(model$inputList$getInput(inputId = sprintf("output_%s_grp",i)), "cannot find input")
    }

    # Check environments
    expect_true(exists("a", envir = model$envs$shared))
    expect_true(exists("b", envir = model$envs$shared))
    for (i in 1:2){
      expect_false(exists("a", envir = model$envs$ind[[i]], inherits = FALSE))
      expect_false(exists("b", envir = model$envs$ind[[i]], inherits = FALSE))
    }
  })

  it ("unshares a group of inputs", {
    model <- test_structure(list(grp = mwGroup(a = mwText(), b = mwText("test"))),
                            ncharts = 2)
    newInputs <- model$unshareInput("grp")
    expect_equal(
      sort(newInputs),
      c("output_1_a", "output_1_b", "output_1_grp", "output_2_a", "output_2_b", "output_2_grp")
    )
    expect_error(model$inputList$getInput("a", 0), "cannot find input")
    expect_error(model$inputList$getInput("b", 0), "cannot find input")
    expect_error(model$inputList$getInput("grp", 0), "cannot find input")

    for (i in 1:2) {
      expect_silent(model$inputList$getInput("a", i))
      expect_silent(model$inputList$getInput("b", i))
      expect_silent(model$inputList$getInput("grp", i))
    }

    expect_null(model$envs$shared$a)
    expect_null(model$envs$shared$b)
    for (i in 1:2) {
      expect_equal(model$envs$ind[[i]]$b, "test")
    }

    model$inputList$setValue("b", "test2", chartId = 1)
    expect_equal(model$envs$ind[[1]]$b, "test2")
    expect_equal(model$envs$ind[[2]]$b, "test")
  })

  it ("ads a chart", {
    model <- test_structure(list(a = mwText("test"), b = mwText()), ncharts = 1,
                           compare = list(a = NULL))
    model$addChart()

    expect_equal(model$ncharts, 2)
    expect_length(model$envs$ind, 2)

    for (i in 1:2) {
      expect_equal(model$envs$ind[[i]]$a, "test")
      expect_null(model$envs$ind[[i]]$b)
    }

    model$inputList$setValue("a", "test2", chartId = 1)
    expect_equal(model$envs$ind[[1]]$a, "test2")
    expect_equal(model$envs$ind[[2]]$a, "test")

    model$inputList$setValue("b", "test3", chartId = 0)
    expect_equal(get("b", envir = model$envs$ind[[1]]), "test3")
    expect_equal(get("b", envir = model$envs$ind[[2]]), "test3")
  })

  it ("removes a chart", {
    model <- test_structure(list(a = mwText("test"), b = mwText()), ncharts = 2,
                            compare = list(a = NULL))
    model$removeChart()

    expect_equal(model$ncharts, 1)
    expect_length(model$envs$ind, 1)
    expect_length(model$inputList$inputTable$input, 2)
    expect_equal(row.names(model$inputList$inputTable), c("shared_b", "output_1_a"), ignore.order = TRUE)
  })

  it ("does not remove last chart", {
    model <- test_structure(list(a = mwText("test"), b = mwText()), ncharts = 1,
                            compare = list(a = NULL))

    expect_error(model$removeChart(), "at least one chart")
  })

  it ("ads many charts", {
    model <- test_structure(list(a = mwText("test"), b = mwText()), ncharts = 2,
                            compare = list(a = NULL))
    model$setChartNumber(4)
    expect_equal(model$ncharts, 4)
    expect_length(model$envs$ind, 4)
  })

  it ("removes many charts", {
    model <- test_structure(list(a = mwText("test"), b = mwText()), ncharts = 4,
                            compare = list(a = NULL))
    model$setChartNumber(2)
    expect_equal(model$ncharts, 2)
    expect_length(model$envs$ind, 2)
  })
})
