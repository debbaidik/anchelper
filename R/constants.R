#' Control chart constants
#' @param n integer vector of subgroup sizes (default 2:10)
#' @export
constants <- function(n = 2:10) {
  d2f <- function(m) integrate(function(q) 1 - ptukey(q, m, Inf), 0, Inf)$value
  ew2 <- function(m) integrate(function(q) 2 * q * (1 - ptukey(q, m, Inf)), 0, Inf)$value
  d2 <- sapply(n, d2f)
  D  <- sqrt(sapply(n, ew2) - d2^2)
  c2 <- sqrt(2 / n) * gamma(n / 2) / gamma((n - 1) / 2)   # notes: s has divisor n
  v  <- sqrt((n - 1) / n - c2^2)
  B1 <- c2 - 3 * v; B2 <- c2 + 3 * v
  D1 <- d2 - 3 * D; D2 <- d2 + 3 * D
  out <- data.frame(n = n, A = 3 / sqrt(n), A1 = 3 / (c2 * sqrt(n)), A2 = 3 / (d2 * sqrt(n)),
                    c2 = c2, B1 = pmax(0, B1), B2 = B2, B3 = pmax(0, B1 / c2), B4 = B2 / c2,
                    d2 = d2, D = D, D1 = pmax(0, D1), D2 = D2, D3 = pmax(0, D1 / d2), D4 = D2 / d2)
  cat("CONSTANTS (functions of subgroup size n; tabulated in notes). Negative LCL factors -> 0\n",
      "  A  : 3/sqrt(n)           Xbar limits mu' -/+ A*sigma'  (sigma known)\n",
      "  A1 : 3/(c2*sqrt(n))      Xbar limits xbb -/+ A1*sbar\n",
      "  A2 : 3/(d2*sqrt(n))      Xbar limits xbb -/+ A2*Rbar\n",
      "  c2 : E(s) = c2*sigma, s with divisor n: sqrt(2/n)*Gamma(n/2)/Gamma((n-1)/2); sigma_hat = sbar/c2\n",
      "       Var(s) = ((n-1)/n - c2^2)*sigma^2\n",
      "  B1, B2 : c2 -/+ 3*sqrt((n-1)/n - c2^2)   S-chart LCL/UCL = B1*sigma, B2*sigma\n",
      "  B3, B4 : B1/c2, B2/c2                    S-chart LCL/UCL = B3*sbar,  B4*sbar\n",
      "  d2 : E(R) = d2*sigma; sigma_hat = Rbar/d2\n",
      "  D  : Var(R) = D^2*sigma^2 (called d3 in standard tables)\n",
      "  D1, D2 : d2 -/+ 3D    R-chart LCL/UCL = D1*sigma, D2*sigma\n",
      "  D3, D4 : D1/d2, D2/d2 R-chart LCL/UCL = D3*Rbar,  D4*Rbar\n",
      "  Note: qcc/sd() use divisor n-1 (c4), so S-chart limits from qcc differ slightly from these.\n\n", sep = "")
  print(round(out, 4), row.names = FALSE)
  invisible(out)
}
