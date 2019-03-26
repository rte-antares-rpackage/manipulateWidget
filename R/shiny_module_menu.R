menuModuleUI <- function(id, okBtn = TRUE, saveBtn = TRUE, updateBtn = TRUE) {
  ns <- NS(id)
  container <- tags$div(
    class="mw-menu",
    # Main Settings button
    tags$div(
      style = "padding:0;",
      class = "mw-btn mw-btn-settings",
      onclick = sprintf("select(this, '%s')", ns("mw-shared-inputs")),
      shiny::actionButton(ns(".settings"), "", icon = shiny::icon("gears"), class = "bt1",
                          style="padding:0;width:50px;height:50px;outline:0;border-radius:0;"),
      tags$div(class="right-arrow")
    ),
    uiOutput(ns("chart_btns"))
  )

  if (okBtn) {
    okBtnInput <- shiny::actionButton(ns("done"), "OK", class = "mw-btn mw-btn-ok")
    container <- tagAppendChild(container, okBtnInput)
  }

  if (updateBtn) {
    updateBtn <- tags$div(
      class = "mw-btn mw-btn-update",
      shiny::actionButton(ns(".update"), "", icon = shiny::icon("refresh"), class = "bt1")
    )
    container <- tagAppendChild(container, updateBtn)
  }

  if (saveBtn) {
    bottom_px <- ifelse(okBtn, "bottom: 80px;", "bottom: 30px;")
    saveBtnInput <- shiny::downloadButton(ns("save"), label = "", class = "mw-btn mw-btn-save",
                                          style = bottom_px)
    container <- tagAppendChild(container, saveBtnInput)
  }

  container
}

menuModuleServer <- function(input, output, session, ncharts, nrow, ncol, ns) {
  ns <- session$ns

  chartId <- shiny::reactiveVal(-1)

  listeners <- character()

  # Eventually add listeners
  observe({
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
    if (ncharts() < 2) ""
    else {
      ids <- ns(paste0("mw-ind-inputs-", seq_len(ncharts())))

      btns <- lapply(seq_len(ncharts()), function(i) {
        if (i == chartId()) active_class <- " active"
        else active_class <- ""

        tags$div(
          class = paste0("mw-btn mw-btn-area", active_class),
          style = "padding: 0px",
          onclick = sprintf("select(this,'%s')", ids[i]),
          shiny::actionButton(
            ns(ids[i]), class = "bt1",
            style="padding:0;width:50px;height:50px;outline:0;border-radius:0;border: none;background: none;padding: 10px 3.3px;",
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

  return(chartId)
}
