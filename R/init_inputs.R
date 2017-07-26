#' Private function that initialize an environment for a given chart.
#'
#' @param parentEnv an environment to be used as the enclosure of the environment
#'   created.
#' @param id index of the chart
#'
#' @return Environment
#' @noRd
initEnv <- function(parentEnv, id) {
  res <- new.env(parent = parentEnv)
  res$.initial <- TRUE
  res$.session <- NULL
  res$.id <- id
  res$.output <- paste0("output_", id)
  res
}

#' Private function that initializes environments and inputs
#'
#' @param inputs list of uninitialized inputs
#' @param env parent environement
#' @param compare character vector with the name of the inputs to compare
#' @param ncharts number of charts that will be created
#'
#' @return A list with the following elements:
#' - envs: list with elements
#'    - shared: shared environment
#'    - ind: list of individual environments. Length is equal to ncharts
#' - inputs: list with elements:
#'    - shared: shared inputs (initialized)
#'    -ind: list of individual inputs (initialized) for each chart. Length is
#'          equal to ncharts
#' - inputList: same as inputs but flattened to facilitate looping.
#' @noRd
initInputs <- function(inputs, env = parent.frame(), compare = NULL, ncharts = 1) {
  if (is.null(names(inputs))) stop("All arguments need to be named.")
  for (i in inputs) if (!inherits(i, "Input")) stop("All arguments need to be Input objects.")

  sharedEnv <- initEnv(env, 1)
  indEnvs <- lapply(seq_len(ncharts), function(i) initEnv(sharedEnv, i))

  sharedInputs <- filterAndInitInputs(inputs, compare, drop = TRUE, sharedEnv)
  indInputs <- lapply(seq_len(ncharts), function(i) {
    filterAndInitInputs(inputs, compare, env = indEnvs[[i]])
  })

  inputList <- InputList(list(sharedInputs, indInputs))

  list(
    envs = list(
      shared = sharedEnv,
      ind = indEnvs
    ),
    inputs = list(
      shared = sharedInputs,
      ind = indInputs
    ),
    inputList = inputList
  )
}
