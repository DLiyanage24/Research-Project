vase <- function(x, ..., names = NULL, bw = NULL, plot = TRUE) {
  
  all.x   <- c(x, list(...))
  centers <- seq_along(all.x)
  n       <- length(all.x)
  
  # sensible default labels
  if (is.null(names) || length(names) != n) {
    names <- as.character(seq_len(n))
  }
  
  # handle bandwidth: allow scalar or vector; NA => let density() choose
  if (is.null(bw)) {
    bw_vec <- rep(NA_real_, n)
  } else {
    bw_vec <- rep_len(bw, n)
  }
  
  if (plot) {
    xmin <- 0.5
    xmax <- n + 0.5
    ymin <- min(unlist(all.x), na.rm = TRUE)
    ymax <- max(unlist(all.x), na.rm = TRUE)
    
    plot(c(xmin, xmax), c(ymin, ymax), type = "n",
         main = "", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  }
  
  vase <- list()
  
  for (i in seq_len(n)) {
    
    xi <- all.x[[i]]
    
    lower   <- as.numeric(quantile(xi, 0.25, na.rm = TRUE))
    upper   <- as.numeric(quantile(xi, 0.75, na.rm = TRUE))
    Hspread <- upper - lower
    step    <- 1.5 * Hspread
    med     <- as.numeric(median(xi, na.rm = TRUE))
    
    # Tukey whiskers (guard against empty subsets)
    up_set <- xi[xi <= upper + step]
    lo_set <- xi[xi >= lower - step]
    TUex   <- if (length(up_set)) max(up_set, na.rm = TRUE) else upper
    Tlex   <- if (length(lo_set)) min(lo_set, na.rm = TRUE) else lower
    
    # density and body construction
    ds <- density(xi, bw = bw_vec[i], na.rm = TRUE)
    Xs <- ds$x
    Ys <- ds$y
    
    in_box <- (Xs < upper) & (Xs > lower)
    Xs <- c(lower, Xs[in_box], upper)
    Ys <- c(0,     Ys[in_box], 0)
    
    # scale width to 0.4
    if (max(Ys) > 0) Ys <- Ys / max(Ys) * 0.4
    
    mpos <- which.min(abs(Xs - med))[1]
    
    outliers <- (xi > upper + step) | (xi < lower - step)
    out_x    <- rep(centers[i], sum(outliers))
    out_y    <- xi[outliers]
    
    if (plot) {
      if (sum(outliers) > 0) {
        segments(centers[i], upper + step, centers[i], upper, col = "grey")
        segments(centers[i], lower - step, centers[i], lower, col = "grey")
        points(out_x, out_y, cex = 0.5)
      }
      
      # body
      lines(centers[i] + Ys, Xs)
      lines(centers[i] - Ys, Xs)
      
      # whiskers (Tukey)
      segments(centers[i], Tlex, centers[i], lower)
      segments(centers[i], TUex, centers[i], upper)
      
      # median segment
      segments((centers[i] - Ys)[mpos], Xs[mpos],
               (centers[i] + Ys)[mpos], Xs[mpos], col = gray(0.25))
    }
    
    # assemble return pieces
    attr(Xs, "names") <- NULL
    vase[[i]] <- list(
      body    = cbind(x = c(centers[i] + Ys, rev(centers[i] - Ys)),
                      y = c(Xs,               rev(Xs))),
      median  = cbind(x    = (centers[i] - Ys)[mpos],
                      y    = Xs[mpos],
                      xend = (centers[i] + Ys)[mpos],
                      yend = Xs[mpos]),
      whisker = cbind(x   = rep(centers[i], 2),
                      xend= rep(centers[i], 2),
                      y   = c(lower, upper),
                      yend= c(Tlex, TUex)),
      outlier = cbind(x = out_x, y = out_y)
    )
  }
  
  if (plot) {
    axis(1, at = centers, labels = names)
    axis(2)
    print(centers)
  }
  
  invisible(vase)
}
