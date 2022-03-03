cal_u_t0 <- function(x) {
  # An example from Olver PDEs
  y = 0
  if (0 <= x && x <= 1/5) {
    y = -x;
  }
  else if (1/5 <= x && x <= 7/10) {
    y = x - 2/5;
  }
  else if (7/10 <= x && x <= 1) {
    y = 1 - x;
  }
  return(y)
}

integrand <- function(x, n) {
  return(sapply(x, cal_u_t0) * sin(n * pi * x))
}

cal_coeff <- function(n) {
  to_integrate = function(x) sapply(x, cal_u_t0) * sin(n * pi * x)
  iv = integrate(to_integrate, 0, 1, subdivisions=3000L)
  return(2 * iv$value)
}

cal_u <- function(x, t, n_terms) {
  s = 0
  for (n in 1:n_terms) {
    bn = bvec[n]
    term = bn * exp( - n^2 * pi^2 * t) * sin( n * pi * x)
    s = s + term
  }
  return(s)
}

bvec <- c()

precompute_bvec <- function(max_n_terms) {
  bvec <<- sapply(1:max_n_terms, cal_coeff)
}
