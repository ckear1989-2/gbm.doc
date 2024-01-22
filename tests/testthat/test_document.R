test_that("all model plots", {
  model <- readRDS("../../inst/test.data/model.rds")
  expect_warning(gbm_doc(model), "y2 values constant when attempting rebase.")
  expect(difftime(Sys.time(), file.info(model$pdffile)$mtime, units = "secs") < 10, "model doc (all plots) pdf not created in last 10 seconds.")
})
