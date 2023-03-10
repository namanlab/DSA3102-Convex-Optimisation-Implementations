# Gradient descent with line search

grad_desc_ls <- function(f, g = NULL, x0 = NULL, d, tol = 0.00001, 
                      minimizer = "newton", tnewton_x0 = 0.1, tbisect_a = -1000,
                      tbisect_b = 1000, tgolden_a = -1000,
                      tgolden_b = 1000){
  count = 1
  if (is.null(x0)){x0 = rep(0, d)}
  if (is.null(g)){g = approx_g(f, d)}
  val = g(x0)
  while (sqrt(sum(val^2)) > tol){
    val = g(x0)
    inter_f = function(t){f(x0 - t*val)}
    t = argmin(minimizer, inter_f, tnewton_x0, tbisect_a,
               tbisect_b, tgolden_a, tgolden_b)
    x0 = x0 - t*val
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



argmin <- function(minimizer, f, x0, ab, bb, ag, bg, 
                   der_h = 0.000001, tol = 0.0001){
  g <- function(x) {return((f(x + der_h) - f(x - der_h))/(2*der_h))}
  H <- function(x) {return((f(x + der_h) - 2*f(x) + f(x - der_h))/(der_h^2))}
  if (minimizer == "newton"){
    while (g(x0) > tol){
      x0 = x0 - g(x0)/H(x0)
    }
    return(x0)
  } else if (minimizer == "bisection"){
    cb = 0.5*(ab + bb)
    val_f = g(cb)
    while (val_f > tol){
      if (val_f*g(ab)<= 0){bb = cb}
      else {ab = cb}
      cb = 0.5*(ab + bb)
      val_f = g(cb)
    }
    return(cb)
  } else {
    cg = 0.5*(ag + bg)
    var_tracker = "l" # To keep track of which side of interval was updated
    while (g(cg) > tol){
      l = 0.618*ag + 0.382*bg
      m = 0.382*ag + 0.618*bg
      if (counter == 1){
        val_l = f(l)
        val_m = f(m)
      } else {
        val_l = ifelse(var_tracker == "l", val_m, f(l))
        val_m = ifelse(var_tracker == "m", val_l, f(m))
      }
      if (f(l) > f(m)){
        ag = l
        var_tracker = "m"
      }
      else {
        bg = m
        var_tracker = "l"
      }
      cg = 0.5*(ag + bg)
    }
    return(cg)
  }
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


grad_desc_ls(f, g, c(1, 1), d = 2)
grad_desc_ls(f, g, d = 2)
grad_desc_ls(f, d = 2)


