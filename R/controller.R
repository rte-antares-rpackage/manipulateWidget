Controller <- setRefClass(
  "Controller",
  fields = c("inputList", "envs", "session", "output", "expr", "ncharts", "charts",
             "autoUpdate", "renderFunc"),
  methods = list(

    initialize = function(expr, inputs, autoUpdate = TRUE) {
      expr <<- expr
      inputList <<- inputs$inputList
      ncharts <<- inputs$ncharts
      envs <<- inputs$envs$ind
      autoUpdate <<- autoUpdate
      renderFunc <<- NULL
      output <<- NULL
      charts <<- list()
      updateCharts()
    },

    setShinySession = function(session) {
      session <<- session
      inputList$session <<- session
    },

    getValue = function(name, chartId = 1) {
      inputList$getValue(name, chartId)
    },

    getValueById = function(id) {
      inputList$getValueById(id)
    },

    setValue = function(name, value, chartId = 1) {
      oldValue <- getValue(name, chartId)
      newValue <- inputList$setValue(name, value, chartId)
      if (autoUpdate && !isTRUE(all.equal(oldValue, newValue))) {
        if (inputList$isShared(name)) updateCharts()
        else updateChart(chartId)
      }
    },

    setValueById = function(id, value) {
      oldValue <- getValueById(id)
      newValue <- inputList$setValueById(id, value)
      if (autoUpdate && !isTRUE(all.equal(oldValue, newValue))) {
        if (grepl("^shared_", id)) updateCharts()
        else {
          chartId <- get(".id", envir = inputList$inputs[[id]]$env)
          updateChart(chartId)
        }
      }
    },

    getValues = function(chartId = 1) {
      inputList$getValues(chartId)
    },

    updateChart = function(chartId = 1) {
      charts[[chartId]] <<- eval(expr, envir = envs[[chartId]])
      renderShinyOutput(chartId)
    },

    updateCharts = function() {
      for (i in seq_len(ncharts)) updateChart(i)
    },

    renderShinyOutput = function(chartId) {
      if (!is.null(renderFunc) & !is.null(output)) {
        outputId <- get(".output", envir = envs[[chartId]])
        output[[outputId]] <<- renderFunc(charts[[chartId]])
      }
    },

    renderShinyOutputs = function() {
      for (i in seq_len(ncharts)) renderShinyOutput(i)
    }

  )
)
