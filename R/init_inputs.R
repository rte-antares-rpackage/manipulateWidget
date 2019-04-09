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
      newInput <- inputs$ind[[1]][[name]]$clone(envs$shared)
      inputs$shared <<- append(inputs$shared, structure(list(newInput), .Names = name))
      inputList$addInputs(newInput$getInputs())

      for (i in seq_len(ncharts)) {
        innerInputs <- names(inputs$ind[[i]][[name]]$getInputs())
        for (n in innerInputs) {
          inputList$removeInput(n, chartId = i)
        }
        inputs$ind[[i]][[name]]$destroy()
        inputs$ind[[i]][[name]] <<- NULL
      }

      unname(sapply(newInput$getInputs(), function(i) i$getID()))
    },

    unshareInput = function(name) {
      if (name %in% names(inputs$ind[[1]])) return(character())

      oldInput <- inputs$shared[[name]]

      newInputIds <- character()

      for (i in seq_len(ncharts)) {
        newInput <- oldInput$clone(envs$ind[[i]])
        inputs$ind[[i]] <<- append(inputs$ind[[i]], structure(list(newInput), .Names = name))
        inputList$addInputs(newInput$getInputs())
        newInputIds <- append(
          newInputIds,
          unname(sapply(newInput$getInputs(), function(i) i$getID()))
        )
      }
      innerInputs <- names(oldInput$getInputs())

      for (n in innerInputs) {
        inputList$removeInput(n, chartId = 0)
      }
      oldInput$destroy()
      inputs$shared[[name]] <<- NULL

      newInputIds
    },

    getShareable = function() {
      union(names(inputs$shared), names(inputs$ind[[1]]))
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
        inputsToRemove <- names(inputs$ind[[ncharts]][[n]]$getInputs())
        for (k in inputsToRemove) {
          inputList$removeInput(k, chartId = ncharts)
        }
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
