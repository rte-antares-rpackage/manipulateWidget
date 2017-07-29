context("Input utils")

describe("filterAndInitInputs", {

  it ("returns a filtered list of initialized inputs", {
    inputs <- list(a = mwText(), b = mwText(), c = mwText())

    # Keep inputs
    filteredInputs <- filterAndInitInputs(inputs, c("a", "b"))
    expect_is(filteredInputs, "list")
    expect_length(filteredInputs, 2)
    expect_equal(names(filteredInputs), c("a", "b"))
    for (i in filteredInputs) {
      expect_is(i, "Input")
      expect_initialized(i)
    }

    # Drop inputs
    filteredInputs <- filterAndInitInputs(inputs, c("a", "b"), drop = TRUE)
    expect_is(filteredInputs, "list")
    expect_length(filteredInputs, 1)
    expect_equal(names(filteredInputs), c("c"))
    for (i in filteredInputs) {
      expect_is(i, "Input")
      expect_initialized(i)
    }
  })

  it ("filters grouped inputs", {
    inputs <- list(grp = mwGroup(a = mwText(), b = mwText()), c = mwText())

    # Keep inputs
    filteredInputs <- filterAndInitInputs(inputs, c("a", "c"))
    expect_is(filteredInputs, "list")
    expect_length(filteredInputs, 2)
    expect_equal(names(filteredInputs), c("grp", "c"))
    for (i in filteredInputs) {
      expect_initialized(i)
    }
    expect_is(filteredInputs$grp$value, "list")
    expect_length(filteredInputs$grp$value, 1)
    expect_equal(names(filteredInputs$grp$value), "a")
    expect_initialized(filteredInputs$grp$value$a)

    # Drop inputs
    filteredInputs <- filterAndInitInputs(inputs, c("a", "c"), drop = TRUE)
    expect_is(filteredInputs, "list")
    expect_length(filteredInputs, 1)
    expect_equal(names(filteredInputs), c("grp"))
    for (i in filteredInputs) {
      expect_is(i, "Input")
      expect_initialized(i)
    }
    expect_is(filteredInputs$grp$value, "list")
    expect_length(filteredInputs$grp$value, 1)
    expect_equal(names(filteredInputs$grp$value), "b")
    expect_initialized(filteredInputs$grp$value$b)
  })

  it ("removes empty groups", {
    inputs <- list(grp = mwGroup(a = mwText(), b = mwText()), c = mwText())
    filteredInputs <- filterAndInitInputs(inputs, c("c"))
    expect_is(filteredInputs, "list")
    expect_length(filteredInputs, 1)
    expect_equal(names(filteredInputs), c("c"))
  })

  it ("selects/removes a whole group", {
    inputs <- list(grp = mwGroup(a = mwText(), b = mwText()), c = mwText())
    filteredInputs <- filterAndInitInputs(inputs, c("grp"))
    expect_is(filteredInputs, "list")
    expect_length(filteredInputs, 1)
    expect_equal(names(filteredInputs), c("grp"))
    expect_is(filteredInputs$grp$value, "list")
    expect_length(filteredInputs$grp$value, 2)
    expect_equal(names(filteredInputs$grp$value), c("a", "b"))
    expect_initialized(filteredInputs$grp$value$a)
    expect_initialized(filteredInputs$grp$value$b)

    filteredInputs <- filterAndInitInputs(inputs, c("grp"), TRUE)
    expect_is(filteredInputs, "list")
    expect_length(filteredInputs, 1)
    expect_equal(names(filteredInputs), c("c"))
  })

  it ("updates initial value of an input", {
    inputs <- list(a = mwText(), b = mwText(), c = mwText())
    filteredInputs <- filterAndInitInputs(inputs, "a", newValues = list(a = "test"))
    expect_equal(filteredInputs$a$value, "test")
    expect_equal(filteredInputs$a$env$a, "test")
  })
})

describe("flattenInputs", {
  it ("flattens grouped inputs", {
    inputs <- list(grp = mwGroup(a = mwText(), b = mwText()), c = mwText())
    inputs <- filterAndInitInputs(inputs, c(), TRUE)
    inputList <- flattenInputs(inputs)
    expect_is(inputList, "list")
    expect_length(inputList, 4)
    expect_true(all(c("a", "b", "c", "grp") %in% names(inputList)))
    for (i in inputList) expect_initialized(i)
  })

  it("returns a list that can be used to create an InputList object", {
    inputs <- list(grp = mwGroup(a = mwText(), b = mwText()), c = mwText())
    inputs <- filterAndInitInputs(inputs, c(), TRUE, env = initEnv(parent.frame(), 1))
    inputList <- flattenInputs(inputs)
    expect_silent(InputList(inputs = inputList))
  })
})
