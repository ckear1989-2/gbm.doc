test_that("model param plot", {
  model <- readRDS("../../inst/test.data/model.rds")
  outf <- file.path("../../inst", "test.output", "model_param.pdf")
  expect_silent(param.p.objs <- plot_model_param(model))
  pdf(outf, h = 7, w = 14)
  expect_silent(grid.arrange(param.p.objs))
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model param pdf not created in last 10 seconds.")
})

test_that("model perf plot", {
  model <- readRDS("../../inst/test.data/model.rds")
  outf <- file.path("../../inst", "test.output", "model_perf.pdf")
  expect_silent(perf.p.obj <- plot_model_perf(model))
  pdf(outf, h = 7, w = 14)
  expect_silent(grid.arrange(perf.p.obj))
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model param pdf not created in last 10 seconds.")

  model <- readRDS("../../inst/test.data/model.no.cv.rds")
  outf <- file.path("../../inst", "test.output", "model_perf_no_cv.pdf")
  expect_warning(perf.p.obj <- plot_model_perf(model), "plotting model perf with no cv.")
  pdf(outf, h = 7, w = 14)
  expect_silent(grid.arrange(perf.p.obj))
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model param pdf not created in last 10 seconds.")
})

test_that("model variable importance plot", {
  model <- readRDS("../../inst/test.data/model.rds")
  outf <- file.path("../../inst", "test.output", "model_var_importance.pdf")
  expect_silent(vi.p.obj <- plot_var_importance(model))
  pdf(outf, h = 7, w = 14)
  expect_silent(grid.arrange(vi.p.obj))
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model variable importance pdf not created in last 10 seconds.")
})
