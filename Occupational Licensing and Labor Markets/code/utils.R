# utils.R — shared helpers for replication

log_step <- function(step, msg) {
  cat(sprintf("\n[%d/9] %s\n", step, msg))
}

winsorize <- function(x, lo = 0.01, hi = 0.99) {
  q <- quantile(x, c(lo, hi), na.rm = TRUE)
  x[!is.na(x) & x < q[1]] <- q[1]
  x[!is.na(x) & x > q[2]] <- q[2]
  x
}

wt_summary <- function(dt, vars, labels, wt_col) {
  out <- data.frame(Variable = labels, Mean = NA_real_, SD = NA_real_,
                    stringsAsFactors = FALSE)
  for (i in seq_along(vars)) {
    x <- dt[[vars[i]]]
    w <- dt[[wt_col]]
    ok <- !is.na(x) & !is.na(w) & w > 0
    if (sum(ok) == 0) next
    xv <- x[ok]; wv <- w[ok]
    wm <- sum(xv * wv) / sum(wv)
    wsd <- sqrt(sum(wv * (xv - wm)^2) / sum(wv))
    out$Mean[i] <- wm
    out$SD[i] <- wsd
  }
  out$N <- sum(!is.na(dt[[wt_col]]) & dt[[wt_col]] > 0)
  out
}

wcor <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y) & !is.na(w) & w > 0
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  mx <- sum(x * w) / sum(w); my <- sum(y * w) / sum(w)
  cov_xy <- sum(w * (x - mx) * (y - my)) / sum(w)
  sd_x <- sqrt(sum(w * (x - mx)^2) / sum(w))
  sd_y <- sqrt(sum(w * (y - my)^2) / sum(w))
  cov_xy / (sd_x * sd_y)
}
