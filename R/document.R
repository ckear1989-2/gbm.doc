#' @export
gbm_doc <- function(model) {
  p1.obj <- plot_model_param(model)
  p2.obj <- plot_model_perf(model)
  p3.obj <- plot_var_importance(model)
  univ.p.objs <- plot_model_univariates(model)
  pdf(model$pdffile)
  grid.arrange(p1.obj)
  grid.arrange(p2.obj)
  grid.arrange(p3.obj)
  for (p in univ.p.objs) {
    stopifnot(length(p) == 5)
    grid.arrange(
      p[[1]], p[[2]], p[[3]], p[[4]],
      ncol = 2
    )
    grid.arrange(
      p[[5]], p[[5]], p[[5]], p[[5]],
      ncol = 2, newpage = FALSE
    )
  }
  dev.off()
  NULL
}
