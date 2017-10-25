context("Shiny Module")

describe("Shiny Module", {
  it("returns a well defined shiny module", {
    c <- manipulateWidget(
      paste(a, b),
      a = mwSelect(c("a", "b", "c")),
      b = mwText("test"),
      .compare = "a", .runApp = FALSE
    )

    # server
    f_server <- c$getModuleServer()
    expect_is(f_server, "function")
    expect_equal(names(formals(f_server)), c("input", "output", "session", "..."))

    f_ui <- c$getModuleUI()
    expect_is(f_ui, "function")
  })
})
