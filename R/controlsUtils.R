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
 inputNames <- c()
 initValues <- list()
 types <- c()
 groupLevel <- c()

 .getControlDesc <- function(x, name = "", level = 0) {
   if (is.function(x)) {
     value <- list(attr(x, "value"))
     inputNames <<- append(inputNames, name)
     initValues <<- append(initValues, value)
     types <<- append(types, attr(x, "type"))
     groupLevel <<- append(groupLevel, level)
   }
   else mapply(.getControlDesc, x=x, name = names(x), level = level + 1)
 }
 .getControlDesc(controls)

 data.frame(
   name = inputNames,
   initValue = I(initValues),
   type = types,
   level = groupLevel
 )
}

# Internal function that filters a list of controls given a vector of names
filterControls <- function(controls, names) {
  .filterControls <- function(x) {
    for (n in names(x)) {
      if (is.list(x[[n]])) {
        x[[n]] <- .filterControls(x[[n]])
        if (length(x[[n]]) == 0) x[[n]] <- NULL
      } else {
        if (!n %in% names) x[[n]] <- NULL
      }
    }
    return(x)
  }

  .filterControls(controls)
}
