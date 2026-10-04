#' Control chart constants
#'
#' Constants are functions of subgroup size n (tabulated in the notes):
#' d2 = E(R/sigma), D = sd(R/sigma) (called d3 in standard tables),
#' D1 = d2-3D, D2 = d2+3D, D3 = D1/d2 (0 if negative), D4 = D2/d2,
#' c2 = E(s)/sigma (notes' c2; modern c4), A = 3/sqrt(n).
#' @param n integer vector of subgroup sizes (default 2:10)
#' @export
constants <- function(n = 2:10) {
  d2f <- function(m) integrate(function(q) 1 - ptukey(q, m, Inf), 0, Inf)$value
  ew2 <- function(m) integrate(function(q) 2 * q * (1 - ptukey(q, m, Inf)), 0, Inf)$value
  d2 <- sapply(n, d2f)
  D  <- sqrt(sapply(n, ew2) - d2^2)
  D1 <- d2 - 3 * D
  D2 <- d2 + 3 * D
  out <- data.frame(n = n, d2 = d2, D = D, D1 = D1, D2 = D2,
                    D3 = pmax(0, D1 / d2), D4 = D2 / d2,
                    c2 = sqrt(2 / (n - 1)) * gamma(n / 2) / gamma((n - 1) / 2),
                    A = 3 / sqrt(n))
  cat("CONSTANTS (functions of subgroup size n; tabulated in notes)\n",
      "  d2 : E(R) = d2*sigma, mean of the range of n obs from N(mu, sigma^2); sigma_hat = Rbar/d2\n",
      "  D  : Var(R) = D^2*sigma^2, so D = sd of range / sigma (called d3 in standard tables)\n",
      "  D1 : d2 - 3D, R-chart LCL factor on sigma:  LCL = D1*sigma\n",
      "  D2 : d2 + 3D, R-chart UCL factor on sigma:  UCL = D2*sigma\n",
      "  D3 : D1/d2, R-chart LCL factor on Rbar:     LCL = D3*Rbar  (negative -> 0)\n",
      "  D4 : D2/d2, R-chart UCL factor on Rbar:     UCL = D4*Rbar\n",
      "  c2 : E(s) = c2*sigma, mean of sample s / sigma; sigma_hat = sbar/c2 (modern name c4)\n",
      "  A  : 3/sqrt(n), Xbar limits mu0 -/+ A*sigma0 (value not stated in notes; 3-sigma multiplier)\n\n", sep = "")
  print(round(out, 4), row.names = FALSE)
  invisible(out)
}
