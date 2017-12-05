context("mwModuleUI function")

describe("mwModuleUI function", {

  it("Correct mwModuleUI", {
    # missing id
    expect_error(mwModuleUI())

    # default
    def_mw_ui <- mwModuleUI(id = "def")
    expect_is(def_mw_ui, "shiny.tag.list")
    expect_equal(def_mw_ui[[2]]$name, "div")
    expect_equal(def_mw_ui[[2]]$attribs$id, "def-ui")
    expect_true(grepl("border", def_mw_ui[[2]]$attribs$class))

    # parameters
    def_mw_ui <- mwModuleUI(id = "def", border = FALSE)
    expect_false(grepl("border", def_mw_ui[[2]]$attribs$class))

    def_mw_ui <- mwModuleUI(id = "def", height = "100%")
    expect_true(grepl("height:100%", def_mw_ui[[2]]$attribs$style))
  })
})
