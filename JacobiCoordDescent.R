# Jacobi method: supports parallelization
# Newton for minimiser of univariate function

jacobi_coord_desc <- function(f, xi = NULL, d, max_iter = 1000){
  count = 1
  if (is.null(xi)){xi = rep(0, d)}
  while (count <= max_iter){
    xf = xi
    for (i in 1:d){
      xc = xi
      inter_f = function(x){
        xc[i] = x
        return(f(xc))
      }
      xf[i] = argmin(inter_f, x0 = 1)
    }
    count = count + 1
    xi = xf
  }
  return(xf)
}


argmin <- function(f, x0, der_h = 0.000001, tol = 0.0001){
  g <- function(x) {return((f(x + der_h) - f(x - der_h))/(2*der_h))}
  H <- function(x) {return((f(x + der_h) - 2*f(x) + f(x - der_h))/(der_h^2))}
  while (g(x0) > tol){
    x0 = x0 - g(x0)/H(x0)
  }
  return(x0)
}



# test case

f = function(a){
  x = a[1]
  y = a[2]
  return(x^2 + y^2 + 2*(x - y))
}


jacobi_coord_desc(f, c(2, 1), d = 2)
jacobi_coord_desc(f, d = 2)


