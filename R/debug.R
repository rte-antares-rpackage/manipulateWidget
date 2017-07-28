mwDebug <- function() {
  options(mwDebug = TRUE)
}

mwUndebug <- function() {
  options(mwDebug = FALSE)
}

mwDebugMode <- function() {
  res <- getOption("mwDebug")
  if (is.null(res)) res <- FALSE
  res
}

catIfDebug <- function(...) {
  if (mwDebugMode()) cat(..., "\n")
}
