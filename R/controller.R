#' Controller object of a manipulateWidget application
#'
#' @description
#' \code{MWController} is a reference class that is used to manage interaction
#' with data and update of the view created by manipulateWidget. Only users who
#' desire to create automatic tests for applications created with
#' \code{\link{manipulateWidget}} should care about this object.
#'
#' @section Testing a manipulateWidget application:
#' When \code{\link{manipulateWidget}} is used in a test script, it returns a
#' \code{MWController} object instead of starting a shiny gadget. This object has
#' methods to modify inputs values and check the state of the application. This
#' can be useful to automatically checks if your application behaves like desired.
#' Here is some sample code that uses package \code{testthat}:
#'
#' \preformatted{
#' library("testthat")
#'
#' controller <- manipulateWidget(
#'   x + y,
#'   x = mwSlider(0, 10, 5),
#'   y = mwSlider(0, x, 0),
#'   .compare = "y"
#' )
#'
#' test_that("Two charts are created", {
#'   expect_equal(controller$ncharts, 2)
#' })
#'
#' test_that("Parameter 'max' of 'y' is updated when 'x' changes", {
#'   expect_equal(controller$getParams("y", 1)$max, 5)
#'   expect_equal(controller$getParams("y", 2)$max, 5)
#'   controller$setValue("x", 3)
#'   expect_equal(controller$getParams("y", 1)$max, 3)
#'   expect_equal(controller$getParams("y", 2)$max, 3)
#' })
#'
#' }
#'
#' @field ncharts Number of charts in the application
#' @field nrow Number of rows.
#' @field ncol Number of columns.
#' @field autoUpdate Boolean indicating if charts should be automatically
#'   updated when a value changes
#'
#' @export
MWController <- setRefClass(
  "MWController",
  fields = c("inputList", "uiSpec", "envs", "session", "shinyOutput", "expr", "ncharts", "charts",
             "autoUpdate", "renderFunc", "outputFunc", "useCombineWidgets", "nrow", "ncol",
             "returnFunc", "initialized"),
  methods = list(

    initialize = function(expr, inputs, autoUpdate = TRUE, nrow = NULL,
                          ncol = NULL, returnFunc = function(widget, envs) {widget}) {
      expr <<- expr
      inputList <<- inputs$inputList
      uiSpec <<- inputs
      ncharts <<- inputs$ncharts
      envs <<- inputs$envs$ind
      autoUpdate <<- autoUpdate
      outputFunc <<- NULL
      renderFunc <<- NULL
      session <<- NULL
      shinyOutput <<- NULL
      useCombineWidgets <<- FALSE
      nrow <<- nrow
      ncol <<- ncol
      returnFunc <<- returnFunc
      charts <<- list()
      initialized <<- FALSE
    },

    init = function() {
      if (!initialized) {
        inputList$init()
        updateCharts()
        if (is.null(renderFunc) || is.null(outputFunc) || is.null(useCombineWidgets)) {
          outputAndRender <- getOutputAndRenderFunc(charts[[1]])
          renderFunc <<- outputAndRender$renderFunc
          outputFunc <<- outputAndRender$outputFunc
          useCombineWidgets <<- outputAndRender$useCombineWidgets
          if (useCombineWidgets) {
            charts <<- lapply(charts, combineWidgets)
          }
        }
      }

      return(.self)
    },

    setShinySession = function(output, session) {
      session <<- session
      shinyOutput <<- output
      inputList$session <<- session
      for (env in envs) {
        assign(".initial", FALSE, envir = env)
        assign(".session", session, envir = env)
      }
    },

    getValue = function(name, chartId = 1) {
      "Get the value of a variable for a given chart."
      inputList$getValue(name, chartId)
    },

    getValueById = function(id) {
      inputList$getValue(inputId = id)
    },

    setValue = function(name, value, chartId = 1) {
      "Update the value of a variable for a given chart."
      oldValue <- getValue(name, chartId)
      newValue <- inputList$setValue(name, value, chartId)
      if (autoUpdate && !isTRUE(all.equal(oldValue, newValue))) {
        if (inputList$isShared(name)) updateCharts()
        else updateChart(chartId)
      }
    },

    setValueById = function(id, value) {
      oldValue <- getValueById(id)
      newValue <- inputList$setValue(inputId = id, value = value)
      if (autoUpdate && !isTRUE(all.equal(oldValue, newValue))) {
        if (grepl("^shared_", id)) updateCharts()
        else {
          chartId <- get(".id", envir = inputList$inputs[[id]]$env)
          updateChart(chartId)
        }
      }
    },

    getValues = function(chartId = 1) {
      "Get all values for a given chart."
      inputList$getValues(chartId)
    },

    getParams = function(name, chartId = 1) {
      "Get parameters of an input for a given chart"
      inputList$getInput(name, chartId)$getParams()
    },

    isVisible = function(name, chartId = 1) {
      "Indicates if a given input is visible"
      inputList$isVisible(name, chartId = 1)
    },

    updateChart = function(chartId = 1) {
      catIfDebug("Update chart", chartId)
      charts[[chartId]] <<- eval(expr, envir = envs[[chartId]])
      if (useCombineWidgets) {
        charts[[chartId]] <<- combineWidgets(charts[[chartId]])
      }
      renderShinyOutput(chartId)
    },

    returnCharts = function() {
      "Return all charts."
      if (length(charts) == 1) {
        finalWidget <-  charts[[1]]
      } else {
        finalWidget <- combineWidgets(list = charts, nrow = nrow, ncol = ncol)
      }
      returnFunc(finalWidget, envs)
    },

    show = function() {
      print(returnCharts())
    },

    updateCharts = function() {
      "Update all charts."
      for (i in seq_len(ncharts)) updateChart(i)
    },

    renderShinyOutput = function(chartId) {
      if (!is.null(renderFunc) & !is.null(shinyOutput) &
          is(charts[[chartId]], "htmlwidget")) {
        outputId <- get(".output", envir = envs[[chartId]])
        shinyOutput[[outputId]] <<- renderFunc(charts[[chartId]])
      }
    },

    renderShinyOutputs = function() {
      for (i in seq_len(ncharts)) renderShinyOutput(i)
    },

    clone = function(env = parent.frame()) {
      # Clone environments
      newSharedEnv <- cloneEnv(parent.env(envs[[1]]))
      newEnvs <- lapply(envs, cloneEnv, parentEnv = newSharedEnv)

      newInputs <- lapply(seq_along(inputList$inputs), function(i) {
        x <- inputList$inputs[[i]]$copy()
        chartId <- inputList$chartIds[i]
        if (chartId == 0) x$env <- newSharedEnv
        else x$env <- newEnvs[[chartId]]
        x
      })

      res <- MWController(
        expr,
        list(
          inputList = InputList(newInputs, session),
          envs = list(
            shared = newSharedEnv,
            ind = newEnvs
          ),
          ncharts = ncharts
        ),
        autoUpdate
      )
      res$renderFunc <- renderFunc
      res$charts <- charts
      res$useCombineWidgets <- useCombineWidgets
      res
    },

    getModuleUI = function(gadget = TRUE, saveBtn = TRUE, addBorder = !gadget) {
      function(id, width = "100%", height = "400px") {
        ns <- shiny::NS(id)
        mwUI(ns, uiSpec, nrow, ncol, outputFunc,
             okBtn = gadget, updateBtn = !autoUpdate, saveBtn = saveBtn,
             areaBtns = length(uiSpec$inputs$ind) > 1, border = addBorder,
             width = width, height = height)
      }
    },

    getModuleServer = function() {
      function(input, output, session, ...) {
        controller <- .self$clone()
        controller$setShinySession(output, session)
        controller$renderShinyOutputs()

        message("Click on the 'OK' button to return to the R session.")

        lapply(names(controller$inputList$inputs), function(id) {
          observe(controller$setValueById(id, value = input[[id]]))
        })

        observeEvent(input$.update, controller$updateCharts())
        observeEvent(input$done, onDone(controller))

        output$save <- shiny::downloadHandler(
          filename = function() {
            paste('mpWidget-', Sys.Date(), '.html', sep='')
          },
          content = function(con) {
            htmlwidgets::saveWidget(widget = onDone(controller, stopApp = FALSE),
                                    file = con, selfcontained = TRUE)
          }
        )
      }
    }
  )
)

cloneEnv <- function(env, parentEnv = parent.env(env)) {
  res <- as.environment(as.list(env, all.names = TRUE))
  parent.env(res) <- parentEnv
  res
}

#' knit_print method for MWController object
#'
#' @param x MWController object
#' @param ... arguments passed to function knit_print
#'
#' @export
knit_print.MWController <- function(x, ...) {
  knitr::knit_print(x$returnCharts(), ...)
}
