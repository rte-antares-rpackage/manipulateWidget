context("InputList class")

describe("InputList", {
  it ("correctly updates values when an input value changes", {
    inputs <- list(x = mwSlider(0, 10, 5), y = mwSlider(x, 10, 0))
    inputs <- filterAndInitInputs(inputs, c(), TRUE, initEnv(parent.frame(), 1))
    inputList <- InputList(inputs)

    expect_equal(inputList$inputs$output_1_y$value, 5)

    inputList$setValue("output_1_x", 7)
    expect_equal(inputList$inputs$output_1_y$value, 7)
  })
})
