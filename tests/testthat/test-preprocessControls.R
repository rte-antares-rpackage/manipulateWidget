context("preprocessControls")

describe("preprocessControls", {

  controls <- list(
    x1 = mwText(value = "value1", label = "label1"),
    x2 = mwSelect(choices = 1:3, value = 2, label = "label2"),
    x3 = mwSelect(4:6, 1, multiple = TRUE, label = "label3")
  )

  controlsPrepro <- preprocessControls(controls, env = parent.frame())
  desc <- controlsPrepro$desc

  describe("Controls description", {

    it("is a data.frame", {
      expect_is(desc, "data.frame")
      expectedColumns <- c("name", "initValue", "type", "level", "multiple",
                           "params", "inputId", "mod", "env")
      expect_true(all(expectedColumns %in% names(desc)))
    })

    it ("reads correct values", {
      expect_equal(desc$name, names(controls))
      expect_equal(desc$type, c("text", "select", "select"))
      expect_equal(desc$initValue, list("value1", 2, integer()))
      expect_equal(desc$multiple, c(NA, FALSE, TRUE))
      expect_equal(desc$inputId, desc$name)
    })
  })

  describe("Environments", {
    it ("creates a shared environment", {
      expect_is(controlsPrepro$env$shared, "environment")
    })

    it ("defines shared variables in shared environment", {
      sharedEnv <- controlsPrepro$env$shared
      expect_true(exists("x1", envir = sharedEnv))
      expect_true(exists("x2", envir = sharedEnv))
      expect_true(exists("x3", envir = sharedEnv))
      expect_equal(get("x1", envir = sharedEnv), desc$initValue[[1]])
      expect_equal(get("x2", envir = sharedEnv), desc$initValue[[2]])
      expect_equal(get("x3", envir = sharedEnv), desc$initValue[[3]])
    })

    it ("creates an individual environment", {
      expect_equal(length(controlsPrepro$env$ind), 1)
      expect_is(controlsPrepro$env$ind[[1]], "environment")
    })

    it("can access shared variables from individual environment", {
      indEnv <- controlsPrepro$env$ind[[1]]
      expect_equal(get("x1", envir = indEnv), desc$initValue[[1]])
      expect_equal(get("x2", envir = indEnv), desc$initValue[[2]])
      expect_equal(get("x3", envir = indEnv), desc$initValue[[3]])
    })
  })

  describe("Control list", {
    sharedControls <- controlsPrepro$controls$shared
    indControls <- controlsPrepro$controls$ind

    it ("creates a list of shared and individual controls", {
      expect_equal(length(sharedControls), length(controls))
      expect_equal(names(sharedControls), desc$inputId)
      for (ctrl in sharedControls) {
        expect_is(ctrl, "function")
      }

      expect_equal(length(indControls), 1)
      expect_equal(length(indControls[[1]]), 0)
    })
  })

  describe("Comparison", {
    x3Values <- as.list(sample(4:6, 3, replace = TRUE))
    compare <- list(.n = 3, x2 = NULL, x3 = x3Values)
    controlsPrepro <- preprocessControls(controls, compare, env = parent.frame())
    desc <- controlsPrepro$desc
    envs <- controlsPrepro$env
    ctrlList <- controlsPrepro$controls

    it ("adds individual inputs in description", {
      expect_equal(nrow(desc), 7)
      expect_equal(desc$name, c("x1", "x2", "x3", "x2", "x3", "x2", "x3"))
      expect_equal(desc$inputId, c("x1", "x21", "x31", "x22", "x32", "x23", "x33"))
      expect_equal(desc$mod, c(0, 1, 1, 2, 2, 3, 3))
      expect_equal(desc$initValue, list("value1", 2, x3Values[[1]], 2, x3Values[[2]], 2, x3Values[[3]]))
    })

    it ("creates an individual environment for each module", {
      expect_equal(length(envs$ind), 3)
      for (i in 1:3) {
        expect_equal(get("x1", envir = envs$ind[[i]]), "value1")
        expect_equal(get("x3", envir = envs$ind[[i]]), x3Values[[i]])
      }
    })

    it ("creates a control list for each module", {
      expect_equal(names(ctrlList$shared), "x1")
      expect_equal(length(ctrlList$ind), 3)
      for (i in 1:3) {
        expect_equal(names(ctrlList$ind[[i]]), paste0(c("x2", "x3"), i))
      }
    })
  })

  describe("Update inputs", {
    controls <- list(
      x1 = mwText(value = "value1", label = "label1"),
      x2 = mwSelect(choices = 4:6, value = 2, label = "label2"),
      x3 = mwSelect(x2 * 1:3, 1, multiple = TRUE, label = "label3")
    )

    controlsPrepro <- preprocessControls(controls, env = parent.frame())
    desc <- controlsPrepro$desc
    envs <- controlsPrepro$env
    ctrlList <- controlsPrepro$controls

    it ("updates params in description and control list", {
      expect_equal(desc$currentParams[[2]]$choices, c(4:6))
      expect_equal(desc$currentParams[[3]]$choices, c(4, 8, 12))
      expect_equal(attr(ctrlList$shared$x2, "params")$choices, c(4:6))
      expect_equal(attr(ctrlList$shared$x3, "params")$choices, c(4, 8, 12))
    })

    it ("updates initial values if required", {
      expect_equal(desc$initValue, list("value1", 4, integer()))
    })

    it ("updates inputs of each module", {
      controls <- list(
        x1 = mwText(value = "value1", label = "label1"),
        x2 = mwSelect(choices = 1:3, label = "label2"),
        x3 = mwSelect(x2 * 1:3, 1, multiple = TRUE, label = "label3")
      )
      compare <- list(x2 = list(1, 2, 3), x3 = NULL, .n = 3)
      controlsPrepro <- preprocessControls(controls, compare, update, env = parent.frame())
      desc <- controlsPrepro$desc
      envs <- controlsPrepro$env
      ctrlList <- controlsPrepro$controls

      for (i in 1:3) {
        expect_equal(desc$currentParams[[2 + (i-1) * 2]]$choices, c(1:3))
        expect_equal(desc$currentParams[[3 + (i-1) * 2]]$choices, 1:3 * compare$x2[[i]])
        expect_equal(attr(ctrlList$ind[[i]]$x2, "params")$choices, c(1:3))
        expect_equal(attr(ctrlList$ind[[i]]$x3, "params")$choices, 1:3 * compare$x2[[i]])
      }
    })
  })

  describe("scope", {
    it ("can access parent environment", {
      parent <- new.env()
      assign("test", "testValue", envir = parent)

      controlsPrepro <- preprocessControls(controls, env = parent)
      expect_equal(get("test", envir = controlsPrepro$env$ind[[1]]), "testValue")
    })
  })

  describe("special variables", {

    it("can access .id variable", {
      controls <- list(x = mwNumeric(0, min = .id))
      controlsPrepro <- preprocessControls(controls, env = parent.frame())
      expect_equal(controlsPrepro$desc$currentParams[[1]]$min, 1)
      expect_equal(controlsPrepro$desc$initValue[[1]], 1)

      controlsPrepro <- preprocessControls(controls, compare = list(x = NULL), env = parent.frame())
      expect_equal(controlsPrepro$desc$currentParams[[1]]$min, 1)
      expect_equal(controlsPrepro$desc$currentParams[[2]]$min, 2)
      expect_equal(controlsPrepro$desc$initValue[[1]], 1)
      expect_equal(controlsPrepro$desc$initValue[[2]], 2)
    })
  })
})
