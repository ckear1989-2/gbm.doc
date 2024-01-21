#' @importFrom pretty.gtable pretty_gtable
#' @importFrom gbm3 iteration_error
#' @importFrom gridExtra arrangeGrob
#' @export
plot_model_param <- function(model) {
  params <- c(
    "train.fraction",
    "cv.folds",
    "n.trees",
    "best.trees",
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
    model$cv_folds,
    model$params$num_trees,
    best.trees,
    round(model$params$shrinkage, 3),
    round(model$params$interaction_depth),
    model$distribution$name,
    round(model$train.error[[best.trees]], 4),
    round(model$valid.error[[best.trees]], 4),
    round(cv.error, 4),
    round(model$oobag.improve[[best.trees]], 4)
  )
  params.dt.0 <- data.frame(t(data.frame(parameter = params[1:6], value = vals[1:6])))
  p.obj.options <- list(
    fs = 10,
    rows = rownames(params.dt.0),
    rowcs = c("red1", "red3"),
    bg_fill = "red1",
    bg_color = "black",
    bg_alpha = 0.5,
    bg_linewidth = 0
  )
  p.obj.0 <- pretty.gtable::pretty_gtable(params.dt.0, p.obj.options)
  params.dt.1 <- data.frame(t(data.frame(parameter = params[7:11], value = vals[7:11])))
  p.obj.1 <- pretty.gtable::pretty_gtable(params.dt.1, p.obj.options)
  arrangeGrob(p.obj.0, p.obj.1, nrow = 2)
}

#' @importFrom gbm3 iteration_error
#' @import ggplot2
#' @export
plot_model_perf <- function(model) {
  trees <- cv_rs <- cv <- x1 <- y1 <- x2 <- y2 <- NULL
  train.error <- model$train.error
  if (model$cv_folds > 1) {
    best.trees <- gbm.perf(model, plot.it = FALSE, method = "cv")
    cv.error <- gbm3::iteration_error(model, "cv")
    if (is.null(cv.error)) {
      cv.error <- train.error
    }
  } else {
    warning("plotting model perf with no cv.")
    best.trees <- gbm.perf(model, plot.it = FALSE, method = "test")
    cv.error <- train.error
  }
  p.data <- data.table(
    train.a = model$train.error,
    train.b = model$valid.error,
    cv = cv.error
  )
  p.data[, trees := seq(p.data[, .N])]
  if (model$cv_folds > 1) {
    p.data[, cv_rs := rebase.y(c(p.data[, train.a], p.data[, train.b]), p.data[, cv])]
  } else {
    p.data[, cv_rs := 0]
  }
  sf <- 0.2
  plot.obj <- ggplot2::ggplot(p.data)
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x = trees, y = train.a), color = "red", linewidth = 2)
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x = trees, y = train.b), color = "blue", linewidth = 2)
  plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x = trees, y = cv_rs), color = "green", linewidth = 2)
  best.y <- min(p.data[, train.b])
  best.x <- p.data[train.b == best.y, trees]
  df <- data.frame(x1 = c(1, best.x), x2 = c(best.x, best.x), y1 = c(best.y, best.y), y2 = c(best.y, -Inf))
  plot.obj <- plot.obj + ggplot2::geom_segment(ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2), data = df, linetype = "dashed")
  plot.obj <- plot.obj + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    plot.title = ggplot2::element_text(hjust = 0.5),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle("mean deviance on train.a, train.b and cv")
  # TODO handle error when cv isn't used
  # cv.error is copy of train
  plot.obj <- plot.obj + ggplot2::scale_y_continuous(
    name = "train.a, train.b mean.deviance",
    sec.axis = ggplot2::sec_axis(~ rebase.y(p.data[, cv], .), name = "cv mean.deviance")
  )
  plot.obj
}

#' @import ggplot2
#' @import data.table
#' @export
plot_var_importance <- function(model) {
  sv <- rel_inf <- x <- var <- NULL
  best.trees <- gbm.perf(model, plot.it = FALSE, method = "test")
  p.dt <- data.table(summary(model, num_trees = best.trees, plot_it = FALSE))
  p.dt[, sv := -rel_inf]
  setkey(p.dt, sv)
  p.dt[, x := seq(p.dt[, .N])]
  plot.obj <- ggplot2::ggplot(p.dt)
  plot.obj <- plot.obj + ggplot2::geom_bar(ggplot2::aes(x = x, y = rel_inf), stat = "identity", color = "yellow", fill = "yellow", alpha = 0.3)
  plot.obj <- plot.obj + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    plot.title = ggplot2::element_text(hjust = 0.5),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle("modeled variables relative influence")
  plot.obj <- plot.obj + ggplot2::ylab("relative influence")
  plot.obj <- plot.obj + ggplot2::xlab("modeled variable")
  breaks <- p.dt[, x]
  labels <- p.dt[, var]
  if (length(breaks) != length(labels)) {
    print(p.dt)
    print(breaks)
    print(labels)
    stop("length breaks labels differ")
  }
  plot.obj <- plot.obj + ggplot2::scale_x_continuous(breaks = breaks, labels = labels)
  plot.obj
}
