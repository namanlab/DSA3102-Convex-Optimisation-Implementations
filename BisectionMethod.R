# Need to manually calculate derivative, and plug in the derivative as a function 
# to find the stationery points
bisection_method <- function(grad_f, a, b, tol){
  if (grad_f(a)*grad_f(b) > 0){return("Incorrect Interval Specified")}
  if (grad_f(a) == 0){return("Solution already at " + a)}
  if (grad_f(b) == 0){return("Solution already at " + b)}
  c = 0.5*(a + b)
  val_f = grad_f(c)
  record_df <- c("a" = a, "b" = b, "mid" = c, "val" = val_f)
  while (abs(val_f) > tol){
    if (val_f*grad_f(a) <= 0){b = c}
    else {a = c}
    c = 0.5*(a + b)
    val_f = grad_f(c)
    print(val_f)
    record_df <- rbind(record_df, c("a" = a, "b" = b, "mid" = c, "val" = val_f))
  }
  return(record_df)
}

bisection_method(function(x) sin(x) + x^3, -1, 5, 0.00000001)

# In case gradient cannot compute, use a numerical differentiation method 
# (centered approach):

# der_h specifies the accuracy of the derivative

bisection_method <- function(f, a, b, tol, der_h){
  grad_f <- function(x) {return((f(x + der_h) - f(x - der_h))/(2*der_h))}
  if (grad_f(a)*grad_f(b) > 0){return("Incorrect Interval Specified")}
  if (grad_f(a) == 0){return("Solution already at " + a)}
  if (grad_f(b) == 0){return("Solution already at " + b)}
  c = 0.5*(a + b)
  val_f = grad_f(c)
  record_df <- c("a" = a, "b" = b, "mid" = c, "val" = val_f)
  while (abs(val_f) > tol){
    if (val_f*grad_f(a) <= 0){b = c}
    else {a = c}
    c = 0.5*(a + b)
    val_f = grad_f(c)
    print(val_f)
    record_df <- rbind(record_df, c("a" = a, "b" = b, "mid" = c, "val" = val_f))
  }
  return(record_df)
}

bisection_method(function(x) -cos(x) + x^4/4, -1, 5, 0.00000001, 0.00001)
