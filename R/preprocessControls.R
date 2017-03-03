#' Private function that transforms the list of inputs as it is expressed by the
#' user in a convenient object.
#'
#' @param controls list of controls
#' @param compare list describing how comparison should be done
#' @param update non evaluated list
#' @param env environment
#'
#' @return
#' An object with the following structure:
#' -nmod: number of modules (1 for no comparison, 2 for a single comparison, etc.)
#' - inputs: A data.frame containing a description of all inputs that will be
#'    created in the UI. It contains the following columns:
#'    - name: parameter name
#'    - initValue: initial value of the parameter
#'    - type: type of input
#'    - level: level in the UI (1 = root element)
#'    - multiple: only for select input. Are multiple values allowed?
#'    - params: parameters for the input
#'    - inputId: Id of the input in the UI
#'    - mod: module index. For convenience, shared controls are in module 0
#'    - env: environment
#' - env:
#'    - shared: environment containing the value of shared inputs
#'    - ind: list of environments, one for each module
#' - controls:
#'    - shared: list of shared inputs
#'    - ind: list of list of individual inputs (one for each module)
#'
#' @noRd
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

  res$nmod <- nmod

  # Init environments and control list for each module
  for (i in seq_len(nmod)) {
    res$env$ind[[i]] <- new.env(parent = res$env$shared)
    res$env$ind[[i]]$.id <- i
    res$env$ind[[i]]$.initial <- TRUE
    res$env$ind[[i]]$.session <- NULL
    res$env$ind[[i]]$.output <- paste0("output", i)
    res$controls$ind[[i]] <- list()
  }

  # Quit function here if there is not any control
  if (length(controls) == 0) return(res)

  # controls description #######################################################

  controlsDesc <- getControlDesc(controls)
  controlsDesc$inputId <- controlsDesc$name
  controlsDesc$mod <- 0

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
      out$mod <- i

      tmp <- list()
      for (j in seq_len(nrow(out))) {
        if (out$name[j] %in% names(compare) && !is.null(compare[[out$name[j]]])) {
          value <- compare[[out$name[j]]][[i]]
          out$initValue[[j]] <- value
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
  res$inputs$initValue <- getInitValue(res$inputs)
  for (i in seq_len(nrow(res$inputs))) {
    res$inputs$params[[i]]$value <- res$inputs$initValue[[i]]
    assign(res$inputs$name[i], res$inputs$initValue[[i]], envir = res$inputs$env[[i]])
  }

  # Process the update parameter
  for (i in seq_len(nmod)) {
    updatedParams <- eval(update, envir = res$env$ind[[i]])
    for (n in names(updatedParams)) {
      j <- which(res$inputs$name == n & res$inputs$mod %in% c(0, i))
      if (length(j) == 1) {
        res$inputs$params[[j]] <- mergeList(res$inputs$params[[j]], updatedParams[[n]])
      }
    }
  }

  # Second check of initial values
  res$inputs$initValue <- getInitValue(res$inputs)
  for (i in seq_len(nrow(res$inputs))) {
    res$inputs$params[[i]]$value <- res$inputs$initValue[[i]]
    assign(res$inputs$name[i], res$inputs$initValue[[i]], envir = res$inputs$env[[i]])
  }

  # List of controls for UI ####################################################

  res$controls$shared <- filterControls(controls, names(compare), drop = TRUE)
  res$controls$shared <- setValueAndParams(res$controls$shared, res$inputs)

  for (i in seq_len(nmod)) {
    res$controls$ind[[i]] <- filterControls(controls, names(compare))
    res$controls$ind[[i]] <- addSuffixToControls(res$controls$ind[[i]], i)
    res$controls$ind[[i]] <- setValueAndParams(res$controls$ind[[i]], res$inputs)
  }

  res
}

getInitValue <- function(desc) {
  type <- desc$type
  value <- desc$initValue
  params <- desc$params
  lapply(seq_along(type), function(i) {
    v <- value[[i]]
    p <- params[[i]]

    if (type[i] == "slider") {
      v[v < p$min] <- p$min
      v[v > p$max] <- p$max
    } else if (type[i] %in% c("text", "password")) {
      if (is.null(v) || is.na(v)) {
        v <- ""
      } else {
        v <- as.character(v)
      }
    } else if (type[i] == "numeric") {
      if (length(v) == 0 || !is.numeric(v)) {
        v <- NA_real_
      }
      if (!is.na(v)) {
        if (!is.null(p$min) && v < p$min) {
          v <- p$min
        }
        if (!is.null(p$max) && v > p$max) {
          v <- p$max
        }
      }
    } else if (type[i] == "select") {
      if (is.null(v) || !all(v %in% p$choices)) {
        if (is.null(p$multiple) || !p$multiple) {
          v <- p$choices[[1]]
        } else {
          v <- intersect(v, p$choices)
        }
      }
    } else if (type[i] == "checkbox") {
      if (is.null(v) || !is.logical(v)) {
        v <- FALSE
      }
    } else if (type[i] == "radio") {
      if (is.null(v) || !all(v %in% p$choices)) {
        v <- p$choices[[1]]
      }
    } else if (type[i] == "date") {

    } else if (type[i] == "dateRange") {

    } else if (type[i] == "checkboxGroup") {
      if (is.null(v) || !all(v %in% p$choices)) {
        v <- intersect(v, p$choices)
      }
    }

    v
  })
}

setValueAndParams <- function(controls, desc) {
  name <- desc$inputId
  initValue <- desc$initValue
  params <- desc$params

  setValueAndParamsIter <- function(x) {
    for (n in names(x)) {
      if (is.list(x[[n]])) {
        x[[n]] <- setValueAndParamsIter(x[[n]])
      } else {
        i <- which(name == n)
        attr(x[[n]], "params") <- params[[i]]
        attr(x[[n]], "params")$value <- initValue[[i]]
      }
    }

    x
  }

  setValueAndParamsIter(controls)
}
