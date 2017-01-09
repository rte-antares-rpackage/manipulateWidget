#' Include a static image in a combinedWidgets
#'
#' @param file
#' path of the image to include.
#'
#' @return
#' HTML code needed to include the image in a combinedWidgets object.
#'
#' @export
#'
staticImage <- function(file) {
  data <- base64enc::base64encode(readBin(file, "raw", file.info(file)[1, "size"]))
  ext <- tools::file_ext(file)
  sprintf('<img src="data:image/%s;base64,%s" style="max-width:100%%;max-height:100%%">', ext, data)
}
