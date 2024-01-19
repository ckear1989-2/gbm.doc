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
  pdf(outf, h = 7, w = 14)
  expect_warning(plot_model_univariates(model), "y2 values constant when attempting rebase.")
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model univariates pdf not created in last 10 seconds.")
})
