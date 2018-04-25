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
  if (id == 0) res$.output <- "shared"
  else res$.output <- paste0("output_", id)
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
#' - ncharts: number of charts
#' @noRd
initInputs <- function(inputs, env = parent.frame(), compare = NULL, ncharts = 1) {
  res <- Model()
  res$init(inputs = inputs, env = env, compare = compare, ncharts = ncharts)
  res
}

Model <- setRefClass(
  "Model",
  fields = c("envs", "inputs", "inputList", "ncharts"),
  methods = list(
    initialize = function() {},

    init = function(inputs, env = parent.frame(), compare = NULL, ncharts = 1) {
      if (is.null(names(inputs))) stop("All arguments need to be named.")
      for (i in inputs) if (!inherits(i, "Input")) stop("All arguments need to be Input objects.")

      sharedEnv <- initEnv(env, 0)
      indEnvs <- lapply(seq_len(ncharts), function(i) initEnv(sharedEnv, i))

      sharedInputs <- filterAndInitInputs(inputs, names(compare), drop = TRUE, sharedEnv)
      indInputs <- lapply(seq_len(ncharts), function(i) {
        newValues <- list()
        for (n in names(compare)) {
          if(!is.null(compare[[n]])) newValues[[n]] <- compare[[n]][[i]]
        }
        filterAndInitInputs(inputs, names(compare), env = indEnvs[[i]], newValues = newValues)
      })

      inputList <<- InputList(list(sharedInputs, indInputs))
      envs <<- list(
        shared = sharedEnv,
        ind = indEnvs
      )
      inputs <<- list(
        shared = sharedInputs,
        ind = indInputs
      )
      ncharts <<- ncharts
    },

    shareInput = function(name) {
      if (name %in% names(inputs$shared)) {
        return(character())
      }
      value <- get(name, envir = envs$ind[[1]])
      newInput <- inputs$ind[[1]][[name]]$copy()

      assign(name, value, envir = envs$shared)
      newInput$env <- envs$shared
      newInput <- list(newInput)
      names(newInput) <- name
      inputs$shared <<- append(inputs$shared, newInput)
      inputList$addInputs(newInput)

      for (i in seq_len(ncharts)) {
        rm(list = name, envir = envs$ind[[i]])
        inputs$ind[[i]][[name]] <<- NULL
        inputList$removeInput(name, chartId = i)
      }

      newInput[[1]]$getID()
    },

    unshareInput = function(name) {
      if (name %in% names(inputs$ind[[1]])) return(character())
      value <- get(name, envir = envs$shared)
      oldInput <- inputs$shared[[name]]

      newInputIds <- character()

      for (i in seq_len(ncharts)) {
        assign(name, value, envir = envs$ind[[i]])
        newInput <- oldInput$copy()
        newInput$env <- envs$ind[[i]]
        newInput <- list(newInput)
        names(newInput) <- name
        inputs$ind[[i]] <<- append(inputs$ind[[i]], newInput)
        inputList$addInputs(newInput)
        newInputIds <- append(newInputIds, newInput[[1]]$getID())
      }

      rm(list = name, envir = envs$shared)
      inputs$shared[[name]] <<- NULL
      inputList$removeInput(name, chartId = 0)

      newInputIds
    },

    addChart = function() {
      ncharts <<- ncharts + 1
      # Copy environment of last chart
      envs$ind <<- append(envs$ind, cloneEnv(envs$ind[[ncharts - 1]], envs$shared))
      assign(".id", ncharts, envir = envs$ind[[ncharts]])
      assign(".output", paste0("output_", ncharts), envir = envs$ind[[ncharts]])

      # Copy inputs
      newInputs <- lapply(inputs$ind[[ncharts - 1]], function(input) {
        newInput <- input$copy()
        newInput$env <- envs$ind[[ncharts]]
        newInput
      })

      names(newInputs) <- names(inputs$ind[[ncharts - 1]])
      inputs$ind[[ncharts]] <<- newInputs

      inputList$addInputs(newInputs)
    },

    removeChart = function() {
      if (ncharts == 1) stop("Need at least one chart.")

      for (n in names(inputs$ind[[ncharts]])) {
        inputList$removeInput(n, chartId = ncharts)
      }
      envs$ind[[ncharts]] <<- NULL
      inputs$ind[[ncharts]] <<- NULL

      ncharts <<- ncharts - 1
    },

    setChartNumber = function(n) {
      if (n < 1) stop("Need at least one chart.")
      while (n != ncharts) {
        if (n > ncharts) {
          addChart()
        } else {
          removeChart()
        }
      }
    }
  )
)
