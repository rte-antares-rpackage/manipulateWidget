menuModuleUI <- function(id, okBtn = TRUE, saveBtn = TRUE, updateBtn = FALSE,
                         exportBtn = TRUE, exportType = "html2canvas") {
  ns <- NS(id)

  container <- tags$div(
      class="mw-menu",
      # Main Settings button
      tags$div(
        style = "padding:0;",
        class = "mw-btn mw-btn-settings",
        onclick = sprintf("select(this, '%s')", ns("mw-shared-inputs")),
        shiny::actionButton(ns(".settings"), "", icon = shiny::icon("gears"), class = "bt1 settings"),
        tags$div(class="right-arrow")
      ),
      uiOutput(ns("chart_btns"))
    )

  if (updateBtn) {
    updateBtn <- tags$div(
      class = "mw-btn mw-btn-update",
      shiny::actionButton(ns(".update"), "", icon = shiny::icon("refresh"), class = "bt1")
    )
    container <- tagAppendChild(container, updateBtn)
  }

  actionButtons <- tags$div(class = "action-buttons-container")

  if (saveBtn) {
    saveBtnInput <- shiny::downloadButton(ns("save"), label = "", class = "mw-btn mw-btn-save")
    actionButtons <- tagAppendChild(actionButtons, saveBtnInput)
  }

  if (exportBtn) {
    if(exportType %in% "html2canvas"){
      exportBtnInput <- shiny::actionButton(ns("export_html2canvas"), icon = icon("camera"), label = "",
                                            class = "mw-btn mw-btn-export",
                                            onclick = sprintf("saveAsPNG('%s')", "mw-chartarea"))
    } else {
      # exportBtnInput <- shiny::downloadButton(ns("export"), icon = icon("camera"), label = "",
      #                                         class = "mw-btn mw-btn-export")
      exportBtnInput <- tags$a(id = ns("export"),
                               class = paste("btn btn-default shiny-download-link",
                                             "mw-btn mw-btn-export"), href = "", target = "_blank", download = NA,
                               icon("camera"), "")
    }

    actionButtons <- tagAppendChild(actionButtons, exportBtnInput)
  }

  if (okBtn) {
    okBtnInput <- shiny::actionButton(ns("done"), "OK", class = "mw-btn mw-btn-ok")
    actionButtons <- tagAppendChild(actionButtons, okBtnInput)
  }

  tagAppendChild(container, actionButtons)
}

menuModuleServer <- function(input, output, session, ncharts, nrow, ncol,
                             displayIndBtns, ctrl) {
  ns <- session$ns

  chartId <- shiny::reactiveVal(-1)

  state <- reactive({
    list(
      chartId = chartId(),
      done = input$done,
      update = input$.update,
      save = input$save
    )
  })

  listeners <- character()

  # Eventually add listeners
  observe({
    req(ncharts())
    ids <- ns(paste0("mw-ind-inputs-", seq_len(ncharts())))

    lapply(seq_along(ids), function(i) {
      if (! ids[[i]] %in% listeners) {
        observeEvent(input[[ids[i]]], {
          if (chartId() == i) chartId(-1)
          else chartId(i)
        })
      }
    })

    listeners <<- union(listeners, ids)
  })

  # If user removes current chart, update chartId
  observeEvent(ncharts(), {
    if (chartId() > ncharts() | (chartId() == 1 & ncharts() == 1)) {
      chartId(-1)
    }
  })

  output$chart_btns <- renderUI({
    req(ncharts())
    if (ncharts() < 2 || !displayIndBtns()) ""
    else {
      ids <- ns(paste0("mw-ind-inputs-", seq_len(ncharts())))

      btns <- lapply(seq_len(ncharts()), function(i) {
        if (i == chartId()) active_class <- " active"
        else active_class <- ""

        tags$div(
          class = paste0("mw-btn mw-btn-area", active_class),
          style = "padding:0;",
          onclick = sprintf("select(this,'%s')", ids[i]),
          shiny::actionButton(
            ns(ids[i]), class = "bt1 area",
            .uiChartIcon(i, nrow(), ncol())
          ),
          tags$div(class="right-arrow")
        )
      })
      btns$class <- "mw-chart-selection"

      do.call(tags$div, btns)
    }
  })

  observeEvent(input$.settings, {
    if (chartId() == 0) chartId(-1)
    else chartId(0)
  })

  output$save <- shiny::downloadHandler(
    filename = function() {
      paste('mw-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
      htmlwidgets::saveWidget(widget = onDone(ctrl$clone(), stopApp = FALSE),
                              file = con, selfcontained = TRUE)
    }
  )

  output$export <- shiny::downloadHandler(
    filename = function() {
      paste('mw-', Sys.Date(), '.png', sep='')
    },
    content = function(con) {
      tmp_html <- tempfile(fileext=".html")
      htmlwidgets::saveWidget(widget = onDone(ctrl$clone(), stopApp = FALSE),
                              file = tmp_html, selfcontained = TRUE)
      webshot::webshot(url = tmp_html, file = con)
    }
  )

  return(state)
}


.uiChartIcon <- function(i, nrow, ncol) {
  WIDTH <- 42
  HEIGHT <- 28
  PAD <- 2
  i <- i - 1

  w <- (WIDTH - 2 * PAD) / ncol
  h <- (HEIGHT - 2 * PAD) / nrow

  chartIconStyle <- sprintf("width:%spx;height:%spx;left:%spx;top:%spx;",
                            w, h, w * (i%%ncol) + PAD, h * (i %/% ncol) + PAD)
  tags$div(
    class = "mw-icon-areachart",
    tags$div(class="mw-icon-chart", style=chartIconStyle)
  )
}
