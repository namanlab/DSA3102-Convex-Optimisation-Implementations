# Newton method

# g and H are gradient and hessian of f
newton_method <- function(f, g = NULL, H = NULL, x0 = NULL, d, tol = 0.00001){
  count = 0
  if (is.null(x0)){x0 = rep(0, d)}
  if (is.null(g)){g = approx_g(f, d)}
  if (is.null(H)){H = approx_H(f, d)}
  while (sqrt(sum(g(x0)^2)) > tol){
    x0 = x0 - (solve(H(x0)) %*% g(x0))
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


approx_H <- function(f, d, h = 0.000001){
  g <- approx_g(f, d, h)
  ret_f <- function(x){
    fin_res = NULL
    for (j in 1:d){
      res = rep(0, d)
      for (i in 1:d){
        inter1 = x
        inter1[i] = inter1[i] + h
        inter2 = x
        inter2[i] = inter2[i] - h
        val <- (g(inter1)[j] - g(inter2)[j])/(2*h)
        res[i] = val
      }
      fin_res = c(fin_res, res)
    }
    return(matrix(fin_res, nrow = d, byrow = T))
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
H = function(a){
  x = a[1]
  y = a[2]
  xxp = 2
  xyp = 2
  yyp = 2 + 12*(y - 2)^2
  return(matrix(c(xxp, xyp, xyp, yyp), nrow = 2, byrow = T))
}

newton_method(f, g, H, c(1, 1), d = 2)
newton_method(f, g, H, d = 2)
newton_method(f, d = 2)
