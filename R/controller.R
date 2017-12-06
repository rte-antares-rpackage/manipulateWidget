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
#'   updated when a value changes. list with \code{value} and \code{initBtn} (not autoUpdate, if want first charts on init)
#'
#' @export
MWController <- setRefClass(
  "MWController",
  fields = c("inputList", "uiSpec", "envs", "session", "shinyOutput", "expr", "ncharts", "charts",
             "autoUpdate", "renderFunc", "outputFunc", "useCombineWidgets", "nrow", "ncol",
             "returnFunc", "initialized"),
  methods = list(

    initialize = function(expr, inputs, autoUpdate = list(value = TRUE, initBtn = FALSE), nrow = NULL,
                          ncol = NULL, returnFunc = function(widget, envs) {widget}) {
      expr <<- expr
      inputList <<- inputs$inputList
      uiSpec <<- inputs
      ncharts <<- inputs$ncharts
      envs <<- inputs$envs
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
      catIfDebug("Controller initialization")
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
        initialized <<- TRUE
      }

      invisible(.self)
    },

    clear = function(){
      rm(list = ls(envir = .self, all.names = TRUE), envir = .self, inherits = TRUE)
    },

    setShinySession = function(output, session) {
      catIfDebug("Set shiny session")
      session <<- session
      shinyOutput <<- output
      inputList$session <<- session
      for (env in envs$ind) {
        assign(".initial", FALSE, envir = env)
        assign(".session", session, envir = env)
      }
      # also on shared env
      assign(".initial", FALSE, envir = envs$shared)
      assign(".session", session, envir = envs$shared)
    },

    getValue = function(name, chartId = 1) {
      "Get the value of a variable for a given chart."
      inputList$getValue(name, chartId)
    },

    getValueById = function(id) {
      inputList$getValue(inputId = id)
    },

    setValue = function(name, value, chartId = 1, reactive = FALSE) {
      "Update the value of a variable for a given chart."
      oldValue <- getValue(name, chartId)
      newValue <- inputList$setValue(name, value, chartId, reactive = reactive)
      if (!initialized) return()
      if (autoUpdate$value && !identical(oldValue, newValue)) {
        if (inputList$isShared(name)) updateCharts()
        else updateChart(chartId)
      }
    },

    setValueById = function(id, value) {
      oldValue <- getValueById(id)
      newValue <- inputList$setValue(inputId = id, value = value)
      if (!initialized) return()
      if (autoUpdate$value && !identical(oldValue, newValue)) {
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
      if(!is.null(envs)){
        e <- new.env(parent = envs$ind[[chartId]]) # User can set values in expr without messing environments
        charts[[chartId]] <<- eval(expr, envir = e)
        if (useCombineWidgets) {
          charts[[chartId]] <<- combineWidgets(charts[[chartId]])
        }
        renderShinyOutput(chartId)
      }

    },

    returnCharts = function() {
      "Return all charts."
      if (length(charts) == 1) {
        finalWidget <-  charts[[1]]
      } else {
        finalWidget <- combineWidgets(list = charts, nrow = nrow, ncol = ncol)
      }
      returnFunc(finalWidget, envs$ind)
    },

    show = function() {
      if (!initialized) {
        message("Nothing to display because controller has not been initialized. Use 'ctrl$init()' where 'ctrl' is the variable created with manipulateWidget()")
      }
      print(returnCharts())
    },

    updateCharts = function() {
      "Update all charts."
      for (i in seq_len(ncharts)) updateChart(i)
    },

    renderShinyOutput = function(chartId) {
      if (!is.null(renderFunc) & !is.null(shinyOutput) &
          is(charts[[chartId]], "htmlwidget")) {
        catIfDebug("Render shiny output")
        outputId <- get(".output", envir = envs$ind[[chartId]])
        shinyOutput[[outputId]] <<- renderFunc(charts[[chartId]])
      }
    },

    renderShinyOutputs = function() {
      for (i in seq_len(ncharts)) renderShinyOutput(i)
    },

    clone = function(env = parent.frame()) {
      res <- MWController(
        expr,
        cloneUISpec(uiSpec, session),
        autoUpdate
      )
      res$charts <- charts
      res$nrow <- nrow
      res$ncol <- ncol
      res$outputFunc <- outputFunc
      res$renderFunc <- renderFunc
      res$useCombineWidgets <- useCombineWidgets
      res$initialized <- initialized
      res$inputList$initialized <- initialized

      res
    },

    getModuleUI = function(gadget = TRUE, saveBtn = TRUE, addBorder = !gadget) {
      function(ns, okBtn = gadget, width = "100%", height = "400px") {
        #ns <- shiny::NS(id)
        mwUI(ns, uiSpec, nrow, ncol, outputFunc,
             okBtn = okBtn, updateBtn = !autoUpdate$value, saveBtn = saveBtn,
             areaBtns = length(uiSpec$inputs$ind) > 1, border = addBorder,
             width = width, height = height)
      }
    },

    render = function(output, session) {
      if (initialized) return()
      ns <- session$ns
      tryCatch({
        init()
        setShinySession(output, session)
        output$ui <- renderUI(getModuleUI()(ns, height = "100%"))

        lapply(inputList$inputs, function(input) {
          # Update input visibility
          catIfDebug("Update visibility of", input$getID())
          shiny::updateCheckboxInput(
            session,
            paste0(input$getID(), "_visible"),
            value = eval(input$display, envir = input$env)
          )
          # Hack to fix https://github.com/rstudio/shiny/issues/1490
          if (input$type == "select" && identical(input$lastParams$multiple, TRUE)) {
            input$valueHasChanged <- TRUE
            input$updateHTML(session)
          }
        })
        if (autoUpdate$value) renderShinyOutputs()
      }, error = function(e) {catIfDebug("Initialization error"); print(e)})
    },

    getModuleServer = function() {
      function(input, output, session, ...) {

        controller <- .self$clone()

        reactiveValueList <- list(...)

        observe({
          for (n in names(reactiveValueList)) {
            controller$setValue(n, reactiveValueList[[n]](), reactive = TRUE)
          }
          controller$render(output, session)
        })

        lapply(names(controller$inputList$inputs), function(id) {
          if (controller$inputList$inputs[[id]]$type != "sharedValue") {
            # When shiny starts, this code is executed but input[[id]] is not defined yet.
            # The code is designed to skip this first useless update.
            e <- environment()
            e$shinyInitialisation <- TRUE
            observe({
              shinyValue <- input[[id]]
              if (e$shinyInitialisation) {
                assign("shinyInitialisation", FALSE, envir = e)
              } else {
                controller$setValueById(id, value = shinyValue)
                controller$render(output, session)
              }
            })
          }
        })

        observeEvent(input$.update, controller$updateCharts(), ignoreNULL = !autoUpdate$initBtn)
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

        return(controller)
      }
    }
  )
)

cloneEnv <- function(env, parentEnv = parent.env(env)) {
  res <- as.environment(as.list(env, all.names = TRUE))
  parent.env(res) <- parentEnv
  res
}

cloneUISpec <- function(uiSpec, session) {
  newSharedEnv <- cloneEnv(uiSpec$envs$shared)
  newEnvs <- lapply(uiSpec$envs$ind, cloneEnv, parentEnv = newSharedEnv)

  newInputs <- lapply(seq_along(uiSpec$inputList$inputs), function(i) {
    x <- uiSpec$inputList$inputs[[i]]$copy()
    chartId <- uiSpec$inputList$chartIds[i]
    if (chartId == 0) x$env <- newSharedEnv
    else x$env <- newEnvs[[chartId]]
    x
  })
  names(newInputs) <- names(uiSpec$inputList$inputs)

  newSpec <- replaceInputs(uiSpec$inputs, newInputs, c(list(newSharedEnv), newEnvs))

  list(
    envs = list(shared = newSharedEnv, ind = newEnvs),
    inputs = newSpec,
    inputList = InputList(newInputs, session),
    ncharts = uiSpec$ncharts
  )
}

replaceInputs <- function(inputs, newInputs, envs) {
  lapply(inputs, function(el) {
    if (is.list(el)) return(replaceInputs(el, newInputs, envs))
    else if (el$type == "group") {
      params <- replaceInputs(el$value, newInputs, envs)
      params$.display <- el$display
      newGroup <- do.call(mwGroup, params)
      env <- envs[[1 + get(".id", envir = el$env)]]
      newGroup$init(el$name, env)
      return(newGroup)
    }
    else return(newInputs[[el$getID()]])
  })
}



#' knit_print method for MWController object
#'
#' @param x MWController object
#' @param ... arguments passed to function knit_print
#'
#' @export
knit_print.MWController <- function(x, ...) {
  x$init()
  knitr::knit_print(x$returnCharts(), ...)
}

#' summary method for MWController object
#'
#' @param object MWController object
#' @param ... Not use
#'
#' @export
summary.MWController <- function(object, ...) {
  cat("Initialized         :", object$initialized, "\n")
  cat("Number of chart(s)  :", object$ncharts, "\n")
  cat("Number of row(s)    :", object$nrow, "\n")
  cat("Number of column(s) :", object$ncol, "\n")
  cat("\nList of inputs : \n\n")
  infos <- lapply(names(object$inputList$inputs), function(n){
    input <- object$inputList$inputs[[n]]
    if (is.atomic(input$value)) {
      if (is.null(input$value)) value <- "NULL"
      else if (length(input$value) == 0) value <- ""
      else value <- paste(input$value, collapse = ", ")
    } else {
      if(is.call(input$value) | is.name(input$value)){
        value <- evalValue(input$value, parent.frame())
        if (is.null(value)) value <- sprintf("<%s>", class(input$value[1]))
        else if (length(value) == 0) value <- ""
        else value <- paste(value, collapse = ", ")
      } else {
        value <- sprintf("<%s>", class(input$value[1]))
      }
    }

    chartId <- as.character(get(".id", envir = input$env))
    if (chartId == "0") chartId <- "shared"

    visible <- object$inputList$isVisible(inputId = n)

    data.frame(inputId = n, type = input$type, variable = input$name,
               chart = chartId, value = value, visible = visible,
               stringsAsFactors = FALSE)
  })
  infos$stringsAsFactors <- FALSE
  infos <- do.call(rbind, infos)
  print(infos)
}

