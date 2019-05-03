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
  fields = c("envs", "inputList", "ncharts", "hierarchy"),
  methods = list(
    initialize = function() {},

    init = function(inputs, env = parent.frame(), compare = NULL, ncharts = 1) {
      if (is.null(names(inputs))) stop("All arguments need to be named.")
      for (i in inputs) if (!inherits(i, "Input")) stop("All arguments need to be Input objects.")

      ncharts <<- ncharts

      # Initialize environments
      sharedEnv <- initEnv(env, 0)
      indEnvs <- lapply(seq_len(ncharts), function(i) initEnv(sharedEnv, i))
      envs <<- list(
        shared = sharedEnv,
        ind = indEnvs
      )

      # Get the hierarchy of inputs (used for html generation)
      getHierarchyRecursive <- function(inputs) {
        res <- sapply(names(inputs), function(n) {
          if (inputs[[n]]$type == "group") {
            getHierarchyRecursive(inputs[[n]]$value)
          } else {
            n
          }
        }, USE.NAMES = TRUE, simplify = FALSE)
      }

      hierarchy <<- getHierarchyRecursive(inputs)

      # Init inputs
      lapply(names(inputs), function(n) {inputs[[n]]$init(n, sharedEnv)})
      inputList <<- InputList(inputs)

      # If compare is not null, unshare inputs and set initial values
      lapply(names(compare) , function(n) {
        newInputIds <- unshareInput(n)
        if (!is.null(compare[[n]])) {
          for (i in seq_len(ncharts)) {
            inputList$setValue(inputId = newInputIds[i], value = compare[[n]][[i]])
          }
        }
      })
    },

    shareInput = function(name) {
      if (name %in% inputList$shared()) {
        return(character())
      }
      oldInput <- inputList$getInput(name, 1)

      if(!is.null(oldInput$group)) {
        return(shareInput(oldInput$group))
      }

      catIfDebug("Share variable", name)
      newInputIds <- character()

      for (dep in unname(do.call(c, inputList$getDeps(oldInput)))) {
        newInputIds <- append(newInputIds, shareInput(inputList$getInput(inputId = dep)$name))
      }

      newInput <- oldInput$clone(envs$shared)

      for (i in seq_len(ncharts)) {
        inputList$getInput(name, i)$destroy()
        inputList$removeInput(name, chartId = i)
      }

      append(newInputIds, inputList$addInputs(list(name = newInput)))
    },

    unshareInput = function(name) {
      if (name %in% inputList$unshared()) return(character())

      oldInput <- inputList$getInput(name, 0)

      if(!is.null(oldInput$group)) {
        return(unshareInput(oldInput$group))
      }

      catIfDebug("Unshare variable", name)
      newInputIds <- character()

      for (id in c(oldInput$revDeps,oldInput$displayRevDeps)) {
        newInputIds <- append(newInputIds, unshareInput(inputList$getInput(inputId = id)$name))
      }
      inputList$removeInput(name, chartId = 0)


      for (i in seq_len(ncharts)) {
        newInput <- oldInput$clone(envs$ind[[i]])

        newInputIds <- append(
          newInputIds,
          inputList$addInputs(list(name = newInput))
        )
      }

      oldInput$destroy()

      newInputIds
    },

    getInputsForChart = function(chartId) {
      if (chartId == 0) {
        inputNames <- intersect(names(hierarchy), inputList$shared())
      } else {
        inputNames <- intersect(names(hierarchy), inputList$unshared())
      }
      sapply(inputNames, function(n) {
        inputList$getInput(n, chartId)
      }, simplify = FALSE, USE.NAMES = TRUE)
    },

    getShareable = function() {
      names(hierarchy)
    },

    addChart = function() {
      ncharts <<- ncharts + 1
      # Copy environment of last chart
      envs$ind <<- append(envs$ind, cloneEnv(envs$ind[[ncharts - 1]], envs$shared))
      assign(".id", ncharts, envir = envs$ind[[ncharts]])
      assign(".output", paste0("output_", ncharts), envir = envs$ind[[ncharts]])

      # Get the list of inputs to clone
      toClone <- inputList$inputTable$chartId == ncharts - 1 &
        inputList$inputTable$name %in% names(hierarchy)
      inputsToClone <- inputList[toClone]

      # Copy inputs
      newInputs <- lapply(inputsToClone, function(input) {
        input$clone(envs$ind[[ncharts]])
      })

      allNewInputs <- lapply(unname(newInputs), function(input) {
        input$getInputs()
      })

      allNewInputs <- do.call(c, allNewInputs)

      inputList$addInputs(allNewInputs)
    },

    removeChart = function() {
      if (ncharts == 1) stop("Need at least one chart.")

      for (n in inputList$unshared()) {
        inputList$removeInput(n, chartId = ncharts)
      }

      envs$ind[[ncharts]] <<- NULL
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
    },

    clone = function() {
      newSharedEnv <- cloneEnv(envs$shared)
      newEnvs <- lapply(envs$ind, cloneEnv, parentEnv = newSharedEnv)

      newInputList <- InputList(list())

      newInputs <- list()
      for (n in names(hierarchy)) {
        if(inputList$isShared(n)) {
          newInputs <- append(newInputs, inputList$getInput(n, 0)$clone(newSharedEnv))
        } else {
          for (i in seq_len(ncharts)) {
            newInputs <- append(newInputs, inputList$getInput(n, i)$clone(newEnvs[[i]]))
          }
        }
      }
      newInputList$addInputs(newInputs)

      res <- Model()
      res$envs <- list(shared = newSharedEnv, ind = newEnvs)
      res$inputList <- newInputList
      res$hierarchy <- hierarchy
      res$ncharts <- ncharts

      res
    }
  )
)
