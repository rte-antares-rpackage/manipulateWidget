preprocessControls <- function(controls, compare = NULL, update = NULL, env) {
  # Initialize object returned by the function
  res <- list(
    inputs = data.frame(),
    env = list(
      shared = new.env(parent = env),
      ind = list()
    ),
    controls = list(
      shared = list(),
      ind = list()
    )
  )

  res$env$shared$.initial <- TRUE
  res$env$shared$.session <- NULL

  # Number of modules to create
  nmod <- 1
  if (!is.null(compare)) {
    if (!is.null(compare$.n)) {
      nmod <- compare$.n
    } else {
      nmod <- 2
    }
  }

  # Init environments and control list for each module
  for (i in seq_len(nmod)) {
    res$env$ind[[i]] <- new.env(parent = res$env$shared)
    res$env$ind[[i]]$.id <- i
    res$controls$ind[[i]] <- list()
  }

  # Quit function here if there is not any control
  if (length(controls) == 0) return(res)

  # controls description #######################################################

  controlsDesc <- getControlDesc(controls)
  controlsDesc$inputId <- controlsDesc$name

  controlsDescShared <- subset(controlsDesc, !name %in% names(compare))
  tmp <- list()
  for (i in seq_len(nrow(controlsDescShared))) {
    assign(controlsDescShared$name[i], controlsDescShared$initValue[[i]],
           envir = res$env$shared)
    tmp[[i]] <- res$env$shared
  }
  controlsDescShared$env <- tmp

  controlsDescInd <- subset(controlsDesc, name %in% names(compare))

  if (nrow(controlsDescInd) > 0) {
    controlsDescInd <- lapply(seq_len(nmod), function(i) {
      out <- controlsDescInd
      out$inputId <- paste0(out$inputId, i)

      tmp <- list()
      for (j in seq_len(nrow(out))) {
        if (out$name[j] %in% names(compare) && !is.null(compare[[out$name[j]]])) {
          value <- compare[[out$name[j]]][[i]]
        } else {
          value <- out$initValue[[j]]
        }
        assign(out$name[j], value, envir = res$env$ind[[i]])
        tmp[[j]] <- res$env$ind[[i]]
      }
      out$env <- tmp
      out
    })
    controlsDescInd <- do.call(rbind, controlsDescInd)
  }

  res$inputs <- rbind(controlsDescShared, controlsDescInd)

  # Correct initial values #####################################################

  # First check of initial values
  # Process the update parameter
  # Second check of initial values

  res
}

comparisonControls <- function(controls, compare, updateInputs = NULL, env) {
  if (length(controls) == 0) return(list(common = list(), ind1 = list(), ind2 = list()))
  common <- filterControls(controls, names(compare), drop = TRUE)
  ind <- filterControls(controls, names(compare))
  ind2 <- ind

  # extract the initial values of the individual parameters of each chart
  controlsDesc <- getControlDesc(controls)
  initValues <- controlsDesc$initValue
  names(initValues) <- controlsDesc$name

  initValues1 <- lapply(compare, function(x) {if(is.null(x)) x else x[[1]]})
  initValues1 <- mergeList(initValues, initValues1)
  initValues2 <- lapply(compare, function(x) {if(is.null(x)) x else x[[2]]})
  initValues2 <- mergeList(initValues, initValues2)

  # Reset initial values of input controls
  env1 <- list2env(initValues1, parent = env)
  env1$.id <- 1
  env1$.initial <- TRUE
  env1$.session <- FALSE
  newParams1 <- eval(updateInputs, env1)
  env2 <- list2env(initValues2, parent = env)
  env2$.id <- 2
  env2$.initial <- TRUE
  env2$.session <- FALSE
  newParams2 <- eval(updateInputs, env2)

  ind <- resetInitValues(ind, initValues1, newParams1)
  ind2 <- resetInitValues(ind2, initValues2, newParams2)
  common <- resetInitValues(common, NULL, newParams1)
  # Add a "2" at the end of the names of the inputs of the second chart
  ind2 <- addSuffixToControls(ind2, "2")

  list(common = common, ind = ind, ind2 = ind2)
}
