# Gradient descent with fixed step size

grad_desc <- function(f, g = NULL, x0 = NULL, d, t = 0.01, tol = 0.00001, a = 0){
  count = 1
  if (is.null(x0)){x0 = rep(0, d)}
  if (is.null(g)){g = approx_g(f, d)}
  while (sqrt(sum(g(x0)^2)) > tol){
    x0 = x0 - t*g(x0)
    t = t*count^(-a)
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


grad_desc(f, g, c(1, 1), d = 2)
grad_desc(f, g, d = 2)
grad_desc(f, d = 2)


grad_desc(f, g, c(1, 1), d = 2, t = 0.1)
grad_desc(f, g, d = 2, t = 0.1)
grad_desc(f, d = 2, t = 0.1)
