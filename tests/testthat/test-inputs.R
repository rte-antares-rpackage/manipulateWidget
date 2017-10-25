context("Shiny inputs")

# Slider
test_input(mwSlider(0, 10, 0), c(5, -20, 20), c(5, 0, 10))
# Slider with two values
test_input(
  mwSlider(0, 10, 0),
  list(c(5, 7), c(-20, 20), c(-20, 5), c(5, 20)),
  list(c(5, 7), c(0, 10), c(0, 5), c(5, 10))
)

# Text
test_input(mwText(), list("1", 1, NULL), list("1", "1", ""))

# Numeric
test_input(mwNumeric(0), list(5, -20, 20, NULL, "a"), list(5, -20, 20, NULL, NULL))
test_input(mwNumeric(0, min = 0, max = 10), c(5, -20, 20), c(5, 0, 10))

# Password
test_input(mwPassword(), list("1", 1, NULL), list("1", "1", ""))

# Select
test_input(mwSelect(1:4), list(1, 2, 5, NULL), list(1, 2, 1, 1))
test_input(
  mwSelect(1:4, multiple = TRUE),
  list(1, 5, NULL, 3:5),
  list(1, integer(0), integer(0), 3:4)
)
# Select where choices have distinct label and values
test_input(
  mwSelect(list(a = 1, b = 2)),
  list(1, 2, 5, NULL),
  list(1, 2, 1, 1)
)
test_input(
  mwSelect(list(a = 1, b = 2), multiple = TRUE),
  list(1, 2, 5, NULL, 1:3),
  list(1, 2, integer(0), integer(0), 1:2)
)

# Checkbox
test_input(
  mwCheckbox(),
  list(TRUE, FALSE, NULL, NA, "test"),
  list(TRUE, FALSE, FALSE, FALSE, FALSE)
)

# Radio buttons
test_input(mwRadio(1:4), list(1, 2, 5, NULL), list(1, 2, 1, 1))
test_input(
  mwRadio(list(a = 1, b = 2)),
  list(1, 2, 5, NULL),
  list(1, 2, 1, 1)
)

# Date picker
test_input(
  mwDate(),
  list(Sys.Date(), "2017-01-01", NULL),
  list(Sys.Date(), as.Date("2017-01-01"), Sys.Date())
)
# Date with min and max dates
test_input(
  mwDate(min = "2017-01-01", max = "2017-12-31"),
  list("2017-06-01", "2016-06-01", "2018-06-01"),
  list(as.Date("2017-06-01"), as.Date("2017-01-01"), as.Date("2017-12-31"))
)


# Date range
defaultRange <- c(Sys.Date(), Sys.Date())
test_input(
  mwDateRange(),
  list(defaultRange, as.character(defaultRange), NULL),
  list(defaultRange, defaultRange, defaultRange)
)
# Date range with min and max dates
test_input(
  mwDateRange(min = "2017-01-01", max = "2017-12-31"),
  list(c("2016-01-01", "2018-01-01")),
  list(as.Date(c("2017-01-01", "2017-12-31")))
)

# Checkbox group
test_input(
  mwCheckboxGroup(1:4),
  list(1, 5, NULL, 3:5),
  list(1, integer(0), integer(0), 3:4)
)
test_input(
  mwCheckboxGroup(list(a = 1, b = 2)),
  list(1, 2, 5, NULL, 1:3),
  list(1, 2, integer(0), integer(0), 1:2)
)

# Groups of input
test_input(mwGroup(a = mwText(), b = mwText()))
test_that("mwGroup throws an error if an argument is not named", {
  expect_error(mwGroup(mwText()), "All arguments need to be named.")
})
test_that("mwGroup throws an error if an argument is not an input", {
  expect_error(mwGroup(a = 1), "All arguments need to be Input objects.")
})

