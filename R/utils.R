rebase.y <- function(y1, y2, nreturn = length(y2), verbose = FALSE) {
  if (length(unique(y1)) == 1) {
    warning("y1 values constant when attempting rebase.")
    new_y2 <- rep(mean(y1), nreturn)
  } else if (length(unique(y2)) == 1) {
    warning("y2 values constant when attempting rebase.")
    new_y2 <- rep(mean(y1), nreturn)
  } else {
    # stretch y2 to range of y1
    max_y1 <- max(y1)
    max_y2 <- max(y2)
    min_y1 <- min(y1)
    min_y2 <- min(y2)
    range_y2 <- max_y2 - min_y2
    range_y1 <- max_y1 - min_y1
    new_y2a <- y2 - min_y2 # max sure all are >= 0
    max_new_y2a <- max(new_y2a)
    new_y2b <- new_y2a / max_new_y2a # rescale to (0 1)
    new_y2c <- new_y2b * range_y1 # stretch to (0, range_y1)
    new_y2 <- new_y2c + min_y1 # shift to (min_y1, max_y1)
  }
  # logic checks
  if (isTRUE(verbose)) {
    print(all(new_y2a >= 0))
    print(all(c(min(new_y2b) == 0, max(new_y2b) == 1)))
    print(all(c(min(new_y2c) == 0, max(new_y2c) == range_y1)))
    print(all(c(min(new_y2) == min_y1, max(new_y2) == max_y1)))
  }
  new_y2[1:nreturn]
}
