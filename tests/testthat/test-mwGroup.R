context("Group of inputs")
describe("mwGroup", {
  it("throws an error if an argument is not named", {
    expect_error(mwGroup(mwText()), "All arguments need to be named.")
  })

  it("throws an error if an argument is not an input", {
    expect_error(mwGroup(a = 1), "All arguments need to be Input objects.")
  })

  it("can be cloned", {
    env1 <- initEnv(parent.frame(), 1)
    env2 <- initEnv(parent.frame(), 2)

    a <- mwText()
    b <- mwText()
    inner_grp = mwGroup(a = a)
    grp <- mwGroup(inner_grp = inner_grp, b = b)
    a$init("a", env1)
    b$init("b", env1)
    inner_grp$init("inner_grp", env1)
    grp$init("grp", env1)

    grp2 <- grp$clone(env2)

    expect_equal(c("a", "b"), ls(envir = env2))
    grp2$value$b$setValue("test")
    expect_equal(grp2$value$b$value, "test")
    expect_equal(get("b", envir = env2), "test")
    expect_equal(get("b", envir = env1), "")

    grp2$value$inner_grp$value$a$setValue("test")
    expect_equal(grp2$value$inner_grp$value$a$value, "test")
    expect_equal(get("a", envir = env2), "test")
    expect_equal(get("a", envir = env1), "")
  })

  it("removes inner inputs from environment", {
    env1 <- initEnv(parent.frame(), 1)

    a <- mwText()
    b <- mwText()
    inner_grp = mwGroup(a = a)
    grp <- mwGroup(inner_grp = inner_grp, b = b)
    a$init("a", env1)
    b$init("b", env1)
    inner_grp$init("inner_grp", env1)
    grp$init("grp", env1)

    grp$destroy()
    expect_true(!"a" %in% ls(envir = env1))
    expect_true(!"b" %in% ls(envir = env1))
  })

  it("can return list of inner inputs", {
    env1 <- initEnv(parent.frame(), 1)

    a <- mwText()
    b <- mwText()
    inner_grp = mwGroup(a = a)
    grp <- mwGroup(inner_grp = inner_grp, b = b)
    a$init("a", env1)
    b$init("b", env1)
    inner_grp$init("inner_grp", env1)
    grp$init("grp", env1)

    inputs <- grp$getInputs()
    expect_equal(names(inputs), c("a", "b"))
    expect_identical(inputs$a, a)
    expect_identical(inputs$b, b)
  })
})
