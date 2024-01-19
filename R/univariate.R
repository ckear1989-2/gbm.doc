#' @import ggplot2
#' @import data.table
univariate <- function(a.dt, x, y, p, w) {
  act <- act_rs <- pred <- count <- weight <- xn <- pred_rs <- NULL
  if (is.null(a.dt)) strop("attempting to plot null data.table")
  setnames(a.dt, x, "x")
  setnames(a.dt, y, "y")
  setnames(a.dt, p, "p")
  setnames(a.dt, w, "w")
  summary.dt <- a.dt[, list(
    act = sum(y), pred = sum(p), count = .N, weight = sum(w)
  ), x][order(-weight)]
  setnames(a.dt, "x", x)
  setnames(a.dt, "y", y)
  setnames(a.dt, "p", p)
  setnames(a.dt, "w", w)
  summary.dt[, act := act / weight]
  summary.dt[, pred := pred / weight]
  if (is.numeric(summary.dt[, x])) setkey(summary.dt, x)
  summary.dt[, xn := seq(summary.dt[, .N])]
  row_count <- summary.dt[, .N]
  if (row_count > 100) {
    message <- paste("plotting top 100 of", summary.dt[, .N], "levels")
    summary.dt <- summary.dt[xn <= 100, ]
  }
  new_row_count <- summary.dt[, .N]
  max_weight <- max(summary.dt[!is.na(weight), weight])
  max_y <- max(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred]))
  min_y <- min(c(summary.dt[!is.na(act), act], summary.dt[!is.na(pred), pred]))
  range_y <- max_y - min_y
  summary.dt[, act_rs := rebase.y(c(summary.dt[, weight], 0), c(summary.dt[, act], summary.dt[, pred]), nreturn = new_row_count)]
  summary.dt[, pred_rs := rebase.y(c(summary.dt[, weight], 0), c(summary.dt[, pred], summary.dt[, act]), nreturn = new_row_count)]
  plot.obj <- ggplot2::ggplot(summary.dt)
  plot.obj <- plot.obj + ggplot2::geom_bar(ggplot2::aes(x = xn, y = weight), stat = "identity", fill = "yellow", color = "yellow", alpha = 0.3)
  if (row_count > 1) {
    plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x = xn, y = act_rs), stat = "identity", color = "red")
    plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x = xn, y = pred_rs), stat = "identity", color = "blue")
  }
  plot.obj <- plot.obj + ggplot2::geom_point(ggplot2::aes(x = xn, y = act_rs), stat = "identity", color = "red")
  plot.obj <- plot.obj + ggplot2::geom_point(ggplot2::aes(x = xn, y = pred_rs), stat = "identity", color = "blue")
  plot.obj <- plot.obj + ggplot2::scale_y_continuous(
    name = "weight",
    sec.axis = ggplot2::sec_axis(~ rebase.y(c(summary.dt[, act], summary.dt[, pred]), .), name = "act, pred")
  )
  breaks <- summary.dt[, xn]
  labels <- summary.dt[, x]
  if (row_count > 50) {
    breaks <- breaks[seq(1, row_count, 5)]
    labels <- labels[seq(1, row_count, 5)]
  }
  if (length(breaks) != length(labels)) {
    print(summary.dt)
    print(breaks)
    print(labels)
    stop("length breaks labels differ")
  }
  plot.obj <- plot.obj + ggplot2::scale_x_continuous(breaks = breaks, labels = labels)
  plot.obj <- plot.obj + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    plot.title = ggplot2::element_text(vjust = 0.5, hjust = 0.5),
    axis.title.y = ggplot2::element_text(vjust = 0.5, hjust = 0.5),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 2)
  )
  plot.obj <- plot.obj + ggplot2::ggtitle(paste(deparse(substitute(a.dt)), x))
  plot.obj <- plot.obj + ggplot2::xlab(x)
  if (row_count > 100) plot.obj <- plot.obj + annotate("text", x = 15, y = max(summary.dt[, weight]), size = 4, label = message)
  plot.obj
}

#' @import ggplot2
#' @import gbm3
#' @export
partial.plot <- function(model, x, a.dt, debug = FALSE) {
  weight <- y <- xn <- y_rs <- NULL
  if (!"data.table" %in% class(a.dt)) stop(paste("attempting to pass non-data.table object", class(a.dt)))
  if (x %in% model$variables$var_names) {
    x.dt <- data.table(plot(model, x, return_grid = TRUE))
    if (isTRUE(debug)) {
      print(x.dt)
    }
    setnames(x.dt, x, "x")
    setnames(a.dt, x, "x")
    summary.dt <- a.dt[, list(count = .N, weight = sum(weight)), x]
    setnames(a.dt, "x", x)
    setkey(summary.dt, x)
    setkey(x.dt, x)
    summary.dt <- merge(summary.dt, x.dt, all = TRUE)
    summary.dt[is.na(weight), weight := 0]
    if (is.numeric(summary.dt[, x])) {
      for (i in seq(summary.dt[, .N])) if (is.na(summary.dt[i, y])) summary.dt[i, y := summary.dt[i - 1, y]]
      summary.dt <- summary.dt[weight > 0, ]
    }
    row_count <- summary.dt[, .N]
    setkey(summary.dt, x)
    summary.dt[, xn := seq(row_count)]
    max_weight <- max(summary.dt[!is.na(weight), weight])
    max_y <- max(summary.dt[!is.na(y), y])
    min_y <- min(summary.dt[!is.na(y), y])
    range_y <- max_y - min_y
    summary.dt[, y_rs := rebase.y(c(summary.dt[, weight], 0), summary.dt[, y])]
    if (range_y == 0) summary.dt[, y_rs := max_weight / 2]
    plot.obj <- ggplot2::ggplot(summary.dt)
    plot.obj <- plot.obj + ggplot2::geom_bar(ggplot2::aes(x = xn, y = weight, group = 1), stat = "identity", fill = "yellow", color = "yellow", alpha = 0.3)
    if (is.numeric(summary.dt[, x])) plot.obj <- plot.obj + ggplot2::geom_line(ggplot2::aes(x = xn, y = y_rs, group = 1), stat = "identity", color = "green", linewidth = 2)
    if (is.factor(summary.dt[, x])) plot.obj <- plot.obj + ggplot2::geom_point(ggplot2::aes(x = xn, y = y_rs, group = 1), stat = "identity", color = "green", size = 8)
    if (range_y > 0) {
      plot.obj <- plot.obj + scale_y_continuous(
        name = "weight",
        sec.axis = ggplot2::sec_axis(~ rebase.y(summary.dt[, y], .), name = "partial")
      )
    } else {
      plot.obj <- plot.obj + scale_y_continuous(
        name = "weight",
        sec.axis = ggplot2::sec_axis(~ . / max_weight * max_y, name = "partial")
      )
    }
    breaks <- summary.dt[, xn]
    labels <- summary.dt[, x]
    if (row_count > 50) {
      breaks <- breaks[seq(1, row_count, 5)]
      labels <- labels[seq(1, row_count, 5)]
    }
    if (length(breaks) != length(labels)) {
      stop("length breaks labels differ")
    }
    plot.obj <- plot.obj + ggplot2::scale_x_continuous(breaks = breaks, labels = labels)
    plot.obj <- plot.obj + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.5),
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 2)
    )
    plot.obj <- plot.obj + ggplot2::ggtitle(x)
    plot.obj <- plot.obj + ggplot2::xlab(x)
  } else {
    plot.obj <- ggplot2::ggplot() +
      annotate("text", x = 4, y = 25, size = 4, label = paste("variable", x, "not modeled")) +
      ggplot2::theme_void() +
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 2))
  }
  plot.obj
}

#' @importFrom grid grid.rect gpar
#' @importFrom gridExtra grid.arrange
grid.square <- quote({
  grid.rect(x = 0.25, y = 0.25, width = 0.50, height = 0.50, gp = gpar(lwd = 5, col = "black", fill = NA))
  grid.rect(x = 0.25, y = 0.75, width = 0.50, height = 0.50, gp = gpar(lwd = 5, col = "black", fill = NA))
  grid.rect(x = 0.75, y = 0.25, width = 0.50, height = 0.50, gp = gpar(lwd = 5, col = "black", fill = NA))
  grid.rect(x = 0.75, y = 0.75, width = 0.50, height = 0.50, gp = gpar(lwd = 5, col = "black", fill = NA))
})

#' @importFrom gridExtra grid.arrange
#' @export
plot_model_univariates <- function(model) {
  for (x in model$uvar) {
    grid.arrange(
      univariate(model$train.a.dt, x, model$yvar, model$pvar, model$wvar),
      univariate(model$train.b.dt, x, model$yvar, model$pvar, model$wvar),
      univariate(model$test.dt, x, model$yvar, model$pvar, model$wvar),
      partial.plot(model, x, model$train.dt),
      ncol = 2
    )
    eval(grid.square)
  }
  NULL
}
