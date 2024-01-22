#' @import ggplot2
plot.dist <- function(x, xlabel) {
  w <- (max(x) - min(x)) / 100
  g <- ggplot2::ggplot(data = NULL)
  g <- g + geom_histogram(ggplot2::aes(x = x), fill = "yellow", alpha = 0.3, binwidth = w)
  g <- g + xlab(xlabel) + ggplot2::ggtitle(paste("distribution of", xlabel))
  g
}

#' @importFrom gridExtra arrangeGrob
#' @export
plot_response_vars <- function(model) {
  y <- gbmp <- NULL
  train.dt <- copy(model$train.dt)
  test.dt <- copy(model$test.dt)
  setnames(train.dt, model$yvar, "y")
  setnames(test.dt, model$yvar, "y")
  setnames(train.dt, model$pvar, "p")
  setnames(test.dt, model$pvar, "p")
  p.obj <- arrangeGrob(
    plot.dist(c(train.dt[, y], test.dt[, y]), model$yvar),
    plot.dist(c(train.dt[, p], test.dt[, p]), "model prediction"),
    nrow = 2
  )
  setnames(train.dt, "y", model$yvar)
  setnames(test.dt, "y", model$yvar)
  setnames(train.dt, "p", model$pvar)
  setnames(test.dt, "p", model$pvar)
  p.obj
}
