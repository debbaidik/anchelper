#' When to use which chart
#' @export
when <- function() {
  cat("
WHICH CHART?
  Measurement, n small (R nearly as efficient as s) .... Xbar and R
  Measurement, n moderate/large (s more efficient) ..... Xbar and s
  Defective items, n constant .......................... p or np (equivalent)
  Defective items, n varies ............................ p  (np has moving CL, confusing)
  Defects per unit, n constant ......................... c
  Defects per unit, n varies ........................... u  (c not meaningful)

DEFECTIVE vs DEFECT
  Defective = item failing one or more specs (counts items).
  Defect = single non-conformity (counts flaws; a defective has >= 1 defect).
  c/u charts: Poisson assumption.

VARIABLE n (p, np, u): pick one approach
  (a) Variable-width limits : exact, one set per subgroup (criticism: different tolerance per subgroup)
  (b) Min/max bands         : outside outer band (n_min) -> out of control
                              all inside inner band (n_max) -> in control
                              between bands -> compute exact limits (a)
  (c) Average n             : approximate; use if sizes vary by no more than 2:1
  (d) Standardized Z        : exact, uniform basis, limits -3/0/+3; loses original scale

ESTIMATORS
  sigma_hat = Rbar/d2 or sbar/c2 (both unbiased); mu_hat = xbb (unbiased, Var = sigma^2/(m*n))
  Notes define s with divisor n: s^2 = (1/n)*sum(x - xbar)^2, so E(s^2) = (n-1)/n*sigma^2
  p  : MME  pbar = (1/m)*sum(d_i/n_i)   (used in notes' approach (a))
       MLE  pbar = sum(d_i)/sum(n_i)    (approaches (b),(c),(d); MVUE; preferred; equal if n constant)
  c  : size 1: lambda_hat = cbar ;  n units: lambda_hat = sum(c_i0)/(m*n)
  u  : MME  ubar = (1/m)*sum(u_i)  ;  MLE lambda_hat = sum(c_i0)/sum(n_i) (preferred when n varies)
  Mean chart (exponential): theta_hat = xbb, Var(theta_hat) = theta^2/(m*n)

PRACTICAL ORDER (X-S / X-R)
  1. Build S (or R) chart for variability.  2. Check it is in control.  3. Only then build Xbar chart.
  Xbar limits involve sigma, so if variability is out of control the Xbar chart is meaningless.
  Range is poor when there are outliers (and for large n); S preferred then.

JUDGING CONTROL (PDF 8)
  In control only if all points inside limits AND scattered randomly.
  Signals: point beyond limit; trend; shift (run of 7 on one side of CL); cluster; cycle.
  Check R/s chart (dispersion) alongside Xbar.
  Negative LCL -> 0.

PERFORMANCE (PDF 8)
  OC beta = P(point inside control region | theta);  ARL = 1/(1-beta);  ARL0 ~ 370.4;  ATS = ARL*h

DESIGN (p-chart)
  Positive LCL: n > 9(1-p)/p
  P(detect) = 0.5 when UCL (or LCL) sits at shifted p1: n = 9p(1-p)/(p1-p)^2

WORDING CUES
  'inspecting all units' / 100% inspection -> variable n (p or u)
  'number of defects per board/unit' -> c or u
  'non-conforming units' -> p
  'tensile strength', 'dimension', 'average value' -> Xbar with R or s
  Only summary values (xbb, Rbar) given -> compute limits directly; n must be given
")
  invisible(NULL)
}
