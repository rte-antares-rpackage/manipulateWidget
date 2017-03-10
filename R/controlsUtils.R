# Set or update the elements of a list given the elements of another list.
mergeList <- function(x, y) {
  for (n in names(y)) {
    x[[n]] <- y[[n]]
  }
  x
}

# Internal function that extracts the name, initial value and type of input
# controls defined by the user.
# This function is required because of the fact that the user can group controls
# and even create nested groups so it is a bit hard to know what are the
# available controls.
#
# Returns a data.frame with columns "name", "initValue", "type", "level",
# "multiple" and "params".
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
 params <- list()
 display <- list()

 getControlDescRecursive <- function(x, name = "", level = 0) {
   if (is.function(x)) {
     value <- list(attr(x, "params")$value)
     inputNames <<- append(inputNames, name)
     initValues <<- append(initValues, value)
     types <<- append(types, attr(x, "type"))
     groupLevel <<- append(groupLevel, level)
     m <- if (is.null(attr(x, "params")$multiple)) NA else attr(x, "params")$multiple
     multiple <<- append(multiple, m)

     # Label of the control
     if (is.null(attr(x, "params"))) {
       attr(x, "params") <- list(label = name)
     } else if (is.null(attr(x, "params")$label)) {
       attr(x, "params")$label <- name
     }
     params <<- append(params, list(attr(x, "params")))
     display <<- append(display, list(attr(x, "display")))
   } else if (length(x) == 0) {
     return()
   } else {
     if (".display" %in% names(x)) {
       display <<- append(display, list(x$.display))
       x$.display <- NULL
     } else {
       display <<- append(display, list(NULL))
     }
     inputNames <<- append(inputNames, name)
     initValues <<- append(initValues, list(NULL))
     types <<- append(types, "group")
     groupLevel <<- append(groupLevel, level)
     multiple <<- append(multiple, NA)
     params <<- append(params, list(NULL))
     mapply(getControlDescRecursive, x=x, name = names(x), level = level + 1)
   }
 }
 getControlDescRecursive(controls, ".root")

 res <- data.frame(
   name = inputNames,
   initValue = I(initValues),
   type = types,
   level = groupLevel,
   multiple = multiple,
   params = I(params),
   display = I(display),
   stringsAsFactors = FALSE
 )

 res <- res[res$type != "group",]
 res
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
