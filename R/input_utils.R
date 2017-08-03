#' Private function that creates a filtered list of initialised inputs.
#'
#' @param inputs list of uninitialized inputs
#' @param names names of inputs to keep or drop
#' @param drop should inputs that appear in argument "names" be dropped or keepped?
#' @param env environment used to initilize parameters
#'
#' @return a list of inputs
#' @noRd
filterAndInitInputs <- function(inputs, names, drop = FALSE,
                                env = parent.frame(), newValues = list()) {
  res <- list()
  for (n in names(inputs)) {
    i <- inputs[[n]]$copy()
    if (n %in% names(newValues)) i$value <- newValues[[n]]
    if (inputs[[n]]$type == "group") {
      if (drop) {
        if (n %in% names) next # Remove the whole group
        else {
          i$value <- filterAndInitInputs(inputs[[n]]$value, names, drop, env)
          if (length(i$value) == 0) next
        }
      } else {
        if (n %in% names) {
          # Keep the whole group
          i$value <- filterAndInitInputs(inputs[[n]]$value, names(i$value), drop, env)
        } else {
          i$value <- filterAndInitInputs(inputs[[n]]$value, names, drop, env)
          if (length(i$value) == 0) next
        }
      }
    } else {
      if (!drop && ! n %in% names) next
      if (drop && n %in% names) next
    }
    i$init(n, env)
    res[[n]] <- i
  }
  res
}

#' Private function that flattens a list of inputs
#'
#' @param inputs list of initialized inputs
#'
#' @return
#' List of initialized inputs. The difference with the input is that
#' inputs that belong to groups are placed in top of the list, so it is easier
#' to iterate over all the inputs. Specifically, the result of this function
#' can be used to create in InputList object.
#' @noRd
flattenInputs <- function(inputs) {
  res <- list()
  if (is.null(names(inputs))) names(inputs) <- as.character(seq_along(inputs))
  for (n in names(inputs)) {
    if (is.list(inputs[[n]])) {
      res <- append(res, flattenInputs(inputs[[n]]))
      next
    }
    if (inputs[[n]]$type == "group") {
      res <- append(res, flattenInputs(inputs[[n]]$value))
    }
    res[[n]] <- inputs[[n]]
  }
  res
}
