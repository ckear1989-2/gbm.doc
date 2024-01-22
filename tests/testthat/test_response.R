test_that("plot model response variables", {
  model <- readRDS("../../inst/test.data/model.rds")
  outf <- file.path("../../inst", "test.output", "response.pdf")
  pdf(outf, h = 7, w = 14)
  expect_silent(grid.arrange(plot_response_vars(model)))
  dev.off()
  expect(difftime(Sys.time(), file.info(outf)$mtime, units = "secs") < 10, "model response variables pdf not created in last 10 seconds.")
})
