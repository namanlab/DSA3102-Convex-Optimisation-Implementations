# Golden section method to find extremum of unimodal function

golden_section <- function(f, a, b, tol){
  c = 0.5*(a + b)
  records_df <-  c("a" = a, "b" = b, "mid" = c, "val" = f(c))
  while (b - a > tol){
    l = 0.618*a + 0.382*b
    m = 0.382*a + 0.618*b
    if (f(l) > f(m)){a = l}
    else {b = m}
    c = 0.5*(a + b)
    records_df <- rbind(records_df, c("a" = a, "b" = b, "mid" = c, "val" = f(c)))
  }
  return(records_df)
}

golden_section(function(x) (x - 2.8)^2, 0, 10, 0.00001)


# Works, but problem: re-computation, one of f(l) and f(u) does not have to be recomputed.
# So, more efficient: 

golden_section <- function(f, a, b, tol){
  c = 0.5*(a + b)
  records_df <-  c("a" = a, "b" = b, "mid" = c, "val" = f(c))
  counter = 1
  var_tracker = "l" # To keep track of which side of interval was updated
  while (b - a > tol){
    l = 0.618*a + 0.382*b
    m = 0.382*a + 0.618*b
    if (counter == 1){
      val_l = f(l)
      val_m = f(m)
    } else {
      val_l = ifelse(var_tracker == "l", val_m, f(l))
      val_m = ifelse(var_tracker == "m", val_l, f(m))
    }
    if (f(l) > f(m)){
      a = l
      var_tracker = "m"
    }
    else {
      b = m
      var_tracker = "l"
    }
    c = 0.5*(a + b)
    records_df <- rbind(records_df, c("a" = a, "b" = b, "mid" = c, "val" = f(c)))
    counter <- counter +  1
  }
  return(records_df)
}

golden_section(function(x) (x - 2.8)^2, 0, 10, 0.00001)
