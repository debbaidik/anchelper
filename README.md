# anchelper

Quick-reference helpers for control charts in R.

## Installation

```r
# install.packages("remotes")
remotes::install_github("debbaidik/anchelper")
```

## Functions

| Function | Description |
|----------|-------------|
| `constants(n)` | Compute control chart constants (d2, D, D1–D4, c2, A) for subgroup sizes |
| `formulas(chart)` | Print LCL / CL / UCL formulas for R, mean, p, np, c, u charts |
| `when()` | Quick-reference guide for choosing the right chart |

## Usage

```r
library(anchelper)

constants()        # constants for n = 2:10
formulas("p")      # p-chart formulas
when()             # which chart to use?
```
