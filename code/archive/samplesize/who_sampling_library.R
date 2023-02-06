## this version is more accurate because it uses
## the actual finite population S^2 values (which we would never know in reality)
## so the only approximation here is the taylor-series expansion to linearize the estimator
approx.var <- function(p, n, d.bar, N, population) {
  fpc <- 1 - (n/N)
  tmp <- (1/n) * 
    fpc * 
    (1/d.bar)^2 * 
    (var(population$y.i) + var(population$d.i)*p^2 - 2*p*cov(population$y.i, population$d.i))
  return(tmp)
}

## this version is less accurate because 
## * it plugs in means to approximate S^2_y as S^2_y\approx d.bar * p * (1-p)
##   thus, this approximation assumes everyone has d.bar people in her network; thus,
##   it does not capture variance in the d_i
## * it assumes that cov(y_i, d_i) = 0
approx.var2 <- function(p, n, d.bar) {
  tmp <- (p / (n*d.bar))
  return(tmp)
}

num.unit.var <- function(population) {
  #return(d.bar * p * (1-p))
  return(var(population$y.i))
}

num.unit.var2 <- function(d.bar, p) {
  return(d.bar * p * (1-p))
}

denom.unit.var <- function(d.bar) {
  return(d.bar)
}