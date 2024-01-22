test_that("model single univariates", {
  model <- readRDS("../../inst/test.data/model.rds")
  outf <- file.path("../../inst", "test.output", "cyl_univariate.pdf")
  pdf(outf, h = 7, w = 14)
  expect_silent(grid.arrange(univariate(model$train.a.dt, "cyl", "mpg", "model.pred", "weight")))
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model cyl univariate pdf not created in last 10 seconds.")
})

test_that("model single partials", {
  model <- readRDS("../../inst/test.data/model.rds")
  outf <- file.path("../../inst", "test.output", "cyl_partial.pdf")
  pdf(outf, h = 7, w = 14)
  expect_warning(grid.arrange(partial.plot(model, "cyl", model$train.a.dt)), "y2 values constant when attempting rebase.")
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model cyl partial pdf not created in last 10 seconds.")

  outf <- file.path("../../inst", "test.output", "disp_partial.pdf")
  pdf(outf, h = 7, w = 14)
  expect_silent(grid.arrange(partial.plot(model, "disp", model$train.a.dt)))
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model disp partial pdf not created in last 10 seconds.")
})

test_that("model all univariates", {
  model <- readRDS("../../inst/test.data/model.rds")
  outf <- file.path("../../inst", "test.output", "univariates.pdf")
  expect_warning(p.objs <- plot_model_univariates(model), "y2 values constant when attempting rebase.")
  pdf(outf, h = 7, w = 14)
  for (p.obj in p.objs) {
    expect_equal(length(p.obj), 5)
    expect_silent(grid.arrange(
      p.obj[[1]],
      p.obj[[2]],
      p.obj[[3]],
      p.obj[[4]],
      ncol = 2
    ))
    expect_silent(grid.arrange(
      p.obj[[5]],
      p.obj[[5]],
      p.obj[[5]],
      p.obj[[5]],
      ncol = 2, newpage = FALSE
    ))
  }
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model univariates pdf not created in last 10 seconds.")

  model$uvar <- c("disp")
  outf <- file.path("../../inst", "test.output", "disp_univariate.pdf")
  # p.objs <- plot_model_univariates(model)
  expect_silent(p.objs <- plot_model_univariates(model))
  expect_equal(length(p.objs[[1]]), 5)
  pdf(outf, h = 7, w = 14)
  expect_silent(grid.arrange(
    p.objs[[1]][[1]],
    p.objs[[1]][[2]],
    p.objs[[1]][[3]],
    p.objs[[1]][[4]],
    ncol = 2
  ))
  expect_silent(grid.arrange(
    p.objs[[1]][[5]],
    p.objs[[1]][[5]],
    p.objs[[1]][[5]],
    p.objs[[1]][[5]],
    ncol = 2, newpage = FALSE
  ))
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model disp univariate pdf not created in last 10 seconds.")
})
