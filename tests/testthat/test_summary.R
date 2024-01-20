test_that("model param plot", {
  model <- readRDS("../../inst/test.data/model.rds")
  outf <- file.path("../../inst", "test.output", "model_param.pdf")
  expect_silent(param.p.objs <- plot_model_param(model))
  pdf(outf, h = 7, w = 14)
  expect_silent(grid.arrange(param.p.objs))
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model param pdf not created in last 10 seconds.")
})
