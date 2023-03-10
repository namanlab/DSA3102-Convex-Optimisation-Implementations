# Gradient descent with Armijo backtracking
# recommended: sigma between 0 and 0.5 (closed)
# beta between 0 and 1 (open)


grad_desc_ab <- function(f, g = NULL, x0 = NULL, d, tol = 0.00001, 
                         sigma = 0.25, beta = 0.75, t0 = 1){
  count = 1
  if (is.null(x0)){x0 = rep(0, d)}
  if (is.null(g)){g = approx_g(f, d)}
  val = -g(x0)
  while (sqrt(sum(val^2)) > tol){
    val = -g(x0)
    inter_f = function(t){f(x0 - t*val)}
    t = armijo(f, g, x0, val, sigma, beta, t0 )
    x0 = x0 + t*val
    count = count + 1
  }
  print(count)
  return(x0)
}


approx_g <- function(f, d, h = 0.000001){
  ret_f <- function(x){
    res = rep(0, d)
    for (i in 1:d){
      inter1 = x
      inter1[i] = inter1[i] + h
      inter2 = x
      inter2[i] = inter2[i] - h
      val <- (f(inter1) - f(inter2))/(2*h)
      res[i] = val
    }
    return(res)
  }
  return(ret_f)
}



armijo <- function(f, g, x0, v, s, b, t0){
  while (f(x0 + t0*v) > f(x0) + t0*s*sum(v*g(x0))){
    t0 = t0*b
  }
  return(t0)
}



# test case

f = function(a){
  x = a[1]
  y = a[2]
  return(x^2 + 2*x*y + y^2 + (y - 2)^4)
}
g = function(a){
  x = a[1]
  y = a[2]
  xp = 2*x + 2*y 
  yp = 2*x + 2*y + 4*(y - 2)^3
  return(c(xp, yp))
}


grad_desc_ab(f, g, c(1, 1), d = 2)
grad_desc_ab(f, g, d = 2)
grad_desc_ab(f, d = 2)


