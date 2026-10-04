#' List LCL, CL, UCL formulas
#' @param chart optional: "R", "mean", "p", "np", "c", "u". NULL lists all.
#' @export
formulas <- function(chart = NULL) {
  f <- list(
    R = c(
      "R-CHART  (R-chart file)",
      "  sigma' known  : LCL = D1*sigma'   CL = d2*sigma'   UCL = D2*sigma'",
      "  sigma unknown : LCL = D3*Rbar     CL = Rbar        UCL = D4*Rbar   (sigma_hat = Rbar/d2)",
      "  D1 = d2 - 3D, D2 = d2 + 3D, D3 = D1/d2, D4 = D2/d2",
      "  Negative LCL -> 0",
      "",
      "(Xbar,R) control region (PDF 8, OC function)",
      "  Xbar: mu0 -/+ A*sigma0 ;  R: D1*sigma0, d2*sigma0, D2*sigma0   [A = 3/sqrt(n), not stated in notes]"),
    mean = c(
      "MEAN CHART, exponential example (R-chart file)",
      "  theta' known  : LCL = theta'(1 - 3/sqrt(n))  CL = theta'  UCL = theta'(1 + 3/sqrt(n))",
      "  theta unknown : LCL = xbb(1 - 3/sqrt(n))     CL = xbb     UCL = xbb(1 + 3/sqrt(n))",
      "  (theta_hat = xbb)"),
    p = c(
      "p-CHART  (Variable Subgroup Size file)   use p' if standard given, else estimate p:",
      "  p estimate: (a) variable-width uses pbar = (1/m)*sum(d_i/n_i)  [method of moments, as defined in notes]",
      "              (b),(c),(d) use pbar = sum(d_i)/sum(n_i) = sum(n_i*p_i)/sum(n_i)  [MLE, = MVUE]",
      "              MLE variance <= MME variance (AM >= HM); equal when n_i equal. Notes prefer MLE.",
      "  (a) variable-width : p -/+ 3*sqrt(p(1-p)/n_i)",
      "  (b) min/max bands  : p -/+ 3*sqrt(p(1-p)/n_min)  [outer],  p -/+ 3*sqrt(p(1-p)/n_max) [inner]",
      "  (c) average n      : p -/+ 3*sqrt(p(1-p)/nbar),  nbar = sum(n_i)/m",
      "  (d) standardized   : Z_i = (p_i - p)/sqrt(p(1-p)/n_i);  LCL = -3, CL = 0, UCL = +3",
      "  CL = p in (a)-(c). Negative LCL -> 0"),
    np = c(
      "np-CHART  (Variable Subgroup Size file)",
      "  (a) variable-width : n_i*p -/+ 3*sqrt(n_i*p*(1-p)),  CL = n_i*p",
      "  (b) min/max bands  : same with n_min (outer), n_max (inner)",
      "  (c) average n      : nbar*p -/+ 3*sqrt(nbar*p*(1-p)),  CL = nbar*p",
      "  (d) standardized   : Z_i = (d_i - n_i*p)/sqrt(n_i*p*(1-p));  LCL = -3, CL = 0, UCL = +3",
      "  p = p' if given, else pbar = sum(n_i*p_i)/sum(n_i) = sum(d_i)/sum(n_i)  [MLE]"),
    c = c(
      "c-CHART  (c/u-chart file)",
      "  subgroup size 1, lambda' known : lambda' -/+ 3*sqrt(lambda'),  CL = lambda'",
      "  subgroup size 1, unknown       : cbar -/+ 3*sqrt(cbar),        CL = cbar",
      "  n units, lambda' known         : n*lambda' -/+ 3*sqrt(n*lambda'),  CL = n*lambda'",
      "  n units, unknown               : n*lam_hat -/+ 3*sqrt(n*lam_hat),  CL = n*lam_hat,",
      "                                   lam_hat = sum(c_i0)/(m*n)",
      "  Negative LCL -> 0"),
    u = c(
      "u-CHART  (c/u-chart file)   u_i = c_i0/n_i",
      "  variable-width, lambda' known : lambda' -/+ 3*sqrt(lambda'/n_i)",
      "  variable-width, unknown       : ubar -/+ 3*sqrt(ubar/n_i)",
      "  lambda estimate: ubar = (1/m)*sum(u_i) [method of moments]  or  lam_hat = sum(c_i0)/sum(n_i) [MLE]",
      "                   notes: use the preferable one (MLE); both equal when n_i equal; MLE unbiasedness/variance left as HW",
      "  min/max bands                 : same with n_min (outer), n_max (inner)",
      "  average n                     : lam -/+ 3*sqrt(lam/nbar)",
      "  standardized                  : Z_i = (u_i - lam)/sqrt(lam/n_i);  LCL = -3, CL = 0, UCL = +3",
      "  CL = lambda' or lam_hat. Negative LCL -> 0")
  )
  if (!is.null(chart)) f <- f[intersect(chart, names(f))]
  for (x in f) cat(x, "", sep = "\n")
  invisible(f)
}
