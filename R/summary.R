#' @importFrom pretty.gtable pretty_gtable
#' @importFrom gbm3 iteration_error
#' @importFrom gridExtra arrangeGrob
#' @export
plot_model_param <- function(model) {
  params <- c(
    "train.fraction",
    "cv.folds",
    "n.trees",
    "shrinkage",
    "interaction.depth",
    "family",
    "train.error",
    "valid.error",
    "cv.error",
    "oobag.improve"
  )
  params <- gsub("\\.", "\n", params)
  if (model$cv_folds > 1) {
    best.trees <- gbm.perf(model, plot.it = FALSE, method = "cv")
    cv.error <- gbm3::iteration_error(model, "cv")[[best.trees]]
    if (is.null(cv.error)) {
      cv.error <- 0
    }
  } else {
    best.trees <- gbm.perf(model, plot.it = FALSE, method = "test")
    cv.error <- 0
  }
  vals <- c(
    round(model$params$train_fraction, 2),
    model$cv.folds,
    model$n.trees,
    round(model$params$shrinkage, 3),
    round(model$params$interaction_depth),
    model$distribution$name,
    round(model$train.error[[best.trees]], 4),
    round(model$valid.error[[best.trees]], 4),
    round(cv.error, 4),
    round(model$oobag.improve[[best.trees]], 4)
  )
  params.dt.0 <- data.frame(t(data.frame(parameter = params[1:5], value = vals[1:5])))
  p.obj.options <- list(
    fs = 10,
    rowcs = c("red1", "red3"),
    bg_fill = "red1",
    bg_color = "black",
    bg_alpha = 0.5,
    bg_linewidth = 0
  )
  p.obj.0 <- pretty.gtable::pretty_gtable(params.dt.0, p.obj.options)
  params.dt.1 <- data.frame(t(data.frame(parameter = params[6:10], value = vals[6:10])))
  p.obj.1 <- pretty.gtable::pretty_gtable(params.dt.1, p.obj.options)
  lay <- rbind(
    c(1, 3),
    c(2, 3)
  )
  arrangeGrob(p.obj.0, p.obj.1, layout_matrix = lay)
}
