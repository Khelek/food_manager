library("limSolve")

getx0 <- function(E, F, G, H) {
    l <- ldei(E,F,G,H)
    if(l$IsError) {
       budget = F[1]
       for(i in 1:10) {
             for(k in 1:2) {
                   F[1] = budget + ((-1)**k) * (50 * 100 * i)
                   print(F[1]/100)
                   l <- ldei(E,F,G,H)
                   if (!l$IsError) {
                      return(l$X)
                   }
             }
       }
  } else {
    return(l$X)
  }
  stop("not compatimable solutions")
}

calculatediet <- function(M1, v) {
  # the inequalities (all pi>0)
  len <- length(M1) / length(v) 
  G <- diag(len)
  H <- rep(0, len)
  
  x0 <- getx0(E = M1, F = v, G = G, H = H)
}
