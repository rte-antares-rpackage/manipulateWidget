# Internal function that extracts the name, initial value and type of input
# controls defined by the user.
# This function is required because of the fact that the user can group controls
# and even create nested groups so it is a bit hard to know what are the
# available controls.
#
# Returns a data.frame with columns "name", "initValue", "type" and "level".
# "level" is equal to 1 if the input is not contained in a group, 2 if it is
# contained in a group, 3 if it is contained in a group contained in a group,
# etc.
getControlDesc <- function(controls) {
 if (length(controls) == 0) return(data.frame())
 inputNames <- c()
 initValues <- list()
 types <- c()
 groupLevel <- c()
 multiple <- c()
 choices <- list()

 getControlDescRecursive <- function(x, name = "", level = 0) {
   if (is.function(x)) {
     value <- list(attr(x, "params")$value)
     inputNames <<- append(inputNames, name)
     initValues <<- append(initValues, value)
     types <<- append(types, attr(x, "type"))
     groupLevel <<- append(groupLevel, level)
     m <- if (is.null(attr(x, "params")$multiple)) NA else attr(x, "params")$multiple
     multiple <<- append(multiple, m)
     choices <<- append(choices, list(attr(x, "params")$choices))
   } else if (length(x) == 0) {
     return()
   } else mapply(getControlDescRecursive, x=x, name = names(x), level = level + 1)
 }
 getControlDescRecursive(controls)

 data.frame(
   name = inputNames,
   initValue = I(initValues),
   type = types,
   level = groupLevel,
   multiple = multiple,
   choices = I(choices),
   stringsAsFactors = FALSE
 )
}

# Internal function that filters a list of controls given a vector of names.
# If drop = TRUE, controls whose name is in "names" are removed, else they
# are kept and all other controls are removed.
filterControls <- function(controls, names, drop = FALSE) {
  if (length(controls) == 0) return(controls)

  filterControlsRecursive <- function(x) {
    for (n in names(x)) {
      if (is.list(x[[n]])) {
        x[[n]] <- filterControlsRecursive(x[[n]])
        if (length(x[[n]]) == 0) x[[n]] <- NULL
      } else {
        if (!n %in% names & !drop) x[[n]] <- NULL
        if (n %in% names & drop) x[[n]] <- NULL
      }
    }
    return(x)
  }

  filterControlsRecursive(controls)
}

# Add a suffix to the name of each control without impacting the labels of the
# inputs.
addSuffixToControls <- function(controls, suffix) {
  if (length(controls) == 0) return(controls)
  addSuffixToControlsRecursive <- function(x) {
    for (n in names(x)) {
      if (is.list(x[[n]])) {
        x[[n]] <- addSuffixToControlsRecursive(x[[n]])
      }
      if (is.null(attr(x[[n]], "params"))) {
        attr(x[[n]], "params") <- list(label = n)
      }else if (is.null(attr(x[[n]], "params")$label)) {
        attr(x[[n]], "params")$label <- n
      }
    }
    names(x) <- paste0(names(x), suffix)
    return(x)
  }
  addSuffixToControlsRecursive(controls)
}

initValue <- function(x) {
  if (!is.null(attr(x, "params")$value) & initValueIsValid(x)) {
    return(x)
  }

  type <- attr(x, "type")
  params <- attr(x, "params")
  multiple <- params$multiple

  if (type == "radio" || (type == "select" && !multiple)) {
    attr(x, "params")$value <- attr(x, "params")$choices[1]
    return(x)
  }

  if (type == "checkboxGroup" || (type == "select" && multiple)) {
    attr(x, "params")$value <- intersect(params$value, params$choices)
    return(x)
  }

  if (type == "slider") {
    attr(x, "params")$value <- params$min
    return(newValue)
  }

  stop("Can not find initial value")
}

initValueIsValid <- function(x) {
  type <- attr(x, "type")
  params <- attr(x, "params")

  if (type %in%  c("radio", "select", "checkboxGroup")) {
    return(all(params$value %in% params$choices))
  }

  if (type == "slider") {
    return(all(params$value >= params$min & params$value <= params$min) )
  }

  TRUE
}

# Private function that resets the initial values of some controls
resetInitValues <- function(controls, values, newParams = NULL) {
  if (length(controls) == 0) return(controls)
  resetInitValuesRecursive <- function(x) {
    for (n in names(x)) {
      if (is.list(x[[n]])) {
        x[[n]] <- resetInitValuesRecursive(x[[n]])
      } else {
        # Update parameters if necessary
        if (n %in% names(newParams)) {
          for (p in names(newParams[[n]])) {
            attr(x[[n]], "params")[[p]] <- newParams[[n]][[p]]
          }
        }
        # Update value if necessary
        if (n %in% names(values)) {
          attr(x[[n]], "params")$value <- values[[n]]
        }
        # Check init value and reset it if necessary
        x[[n]] <- initValue(x[[n]])
      }
    }
    return(x)
  }
  resetInitValuesRecursive(controls)
}

# Private function that returns a list with three elements:
# - common: list of common controls
# - ind: list of individual controls for the first chart to compare
# - ind2: list of individual controls for the seconde chart to compare
comparisonControls <- function(controls, compare, updateInputs = NULL) {
  common <- filterControls(controls, names(compare), drop = TRUE)
  ind <- filterControls(controls, names(compare))
  ind2 <- ind

  # extract the initial values of the individual parameters of each chart
  initValues1 <- lapply(compare, function(x) {if(is.null(x)) x else x[[1]]})
  initValues2 <- lapply(compare, function(x) {if(is.null(x)) x else x[[2]]})

  # Reset initial values of input controls
  newParams1 <- eval(updateInputs, list2env(initValues1, parent = parent.frame()))
  newParams2 <- eval(updateInputs, list2env(initValues2, parent = parent.frame()))

  ind <- resetInitValues(ind, initValues1, newParams1)
  ind2 <- resetInitValues(ind2, initValues2, newParams2)
  common <- resetInitValues(common, NULL, newParams1)
  # Add a "2" at the end of the names of the inputs of the second chart
  ind2 <- addSuffixToControls(ind2, "2")

  list(common = common, ind = ind, ind2 = ind2)
}
