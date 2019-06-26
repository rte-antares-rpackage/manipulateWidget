#' Translate UI titles and labels
#'
#' Creates a list of translation strings that can be passed to function
#' \code{\link{manipulateWidget}} to translate some UI elements.
#'
#' @param settings Title of the settings panel.
#' @param chart Title of the chart panel.
#' @param compare Label of the checkbox that activate the comparison mode.
#' @param compareVars Label of the input containing the list of variables to compare.
#' @param ncol Label of the input that sets the number of columns.
#' @param ncharts Label of the input that sets the number of charts.
#'
#' @return
#' Named list of translation strings.
#'
#' @examples
#' translations <- mwTranslations(
#'   settings = "Parametres", chart = "Graphique", compare = "Comparaison",
#'   compareVars = "Variable de comparaison", ncharts = "Nb graph.", ncol = "Nb col."
#' )
#'
#' if (require(dygraphs)) {
#'   mydata <- data.frame(year = 2000+1:100, value = rnorm(100))
#'   manipulateWidget(dygraph(mydata[range[1]:range[2] - 2000, ], main = title),
#'                    range = mwSlider(2001, 2100, c(2001, 2100)),
#'                    title = mwText("Fictive time series"),
#'                    .translations = translations)
#' }
#'
#' @export
#'
mwTranslations <- function(settings = "Settings", chart = "Chart",
                         compare = "Compare",
                         compareVars = "Variables to compare",
                         ncol = "Nb Columns", ncharts = "Nb Charts") {
  list(
    settings = settings,
    chart = chart,
    compare = compare,
    compareVars = compareVars,
    ncol = ncol,
    ncharts = ncharts
  )
}
