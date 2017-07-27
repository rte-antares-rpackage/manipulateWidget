context("InputList class")

describe("InputList", {
  it ("correctly updates values when an input value changes", {
    inputs <- list(x = mwSlider(0, 10, 5), y = mwSlider(x, 10, 0))
    inputs <- filterAndInitInputs(inputs, c(), TRUE, initEnv(parent.frame(), 1))
    inputList <- InputList(inputs)

    expect_equal(inputList$inputs$output_1_y$value, 5)

    inputList$setValueById("output_1_x", 7)
    expect_equal(inputList$inputs$output_1_y$value, 7)
  })

  it ("gets and updates an input by name and chartId", {
    inputs <- list(x = mwSlider(0, 10, 5), y = mwSlider(0, 10, 0))
    inputs2 <- list(x = mwSlider(0, 10, 6), y = mwSlider(0, 10, 1))
    inputs <- c(
      filterAndInitInputs(list(shared = mwText("test")), c(), TRUE,
                          initEnv(parent.frame(), 0)),
      filterAndInitInputs(inputs, c(), TRUE, initEnv(parent.frame(), 1)),
      filterAndInitInputs(inputs2, c(), TRUE, initEnv(parent.frame(), 2))
    )
    inputList <- InputList(inputs)
    # Get Input
    # Individual inputs
    expect_equal(inputList$getInput("x", 1)$value, 5)
    expect_equal(inputList$getInput("x", 2)$value, 6)
    # Shared inputs
    expect_equal(inputList$getInput("shared", 1)$value, "test")
    expect_equal(inputList$getInput("shared", 2)$value, "test")

    # Get input value
    # Individual inputs
    expect_equal(inputList$getValue("x", 1), 5)
    expect_equal(inputList$getValue("x", 2), 6)
    # Shared inputs
    expect_equal(inputList$getValue("shared", 1), "test")
    expect_equal(inputList$getValue("shared", 2), "test")

    # Update input value
    # Individual inputs
    expect_equal(inputList$setValue("x", 4, 1), 4)
    expect_equal(inputList$setValue("x", 5, 2), 5)
    expect_equal(inputList$getValue("x", 1), 4)
    expect_equal(inputList$getValue("x", 2), 5)
    # Shared inputs
    expect_equal(inputList$setValue("shared", "test1", 1), "test1")
    expect_equal(inputList$getValue("shared", 1), "test1")
    expect_equal(inputList$setValue("shared", "test2", 1), "test2")
    expect_equal(inputList$getValue("shared", 2), "test2")
  })
})
