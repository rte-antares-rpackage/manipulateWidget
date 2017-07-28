mwDebug <- TRUE

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
      session <<- NULL
      output <<- NULL
      charts <<- list()
      updateCharts()
    },

    setShinySession = function(output, session) {
      session <<- session
      output <<- output
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
      if (mwDebug) cat("update input", id, "- new value = ", value, "\n")
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
      if (mwDebug) cat("Update chart", chartId, "\n")
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

      res <- Controller(
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
      res
    }
  )
)

cloneEnv <- function(env, parentEnv = parent.env(env)) {
  res <- as.environment(as.list(env, all.names = TRUE))
  parent.env(res) <- parentEnv
  res
}