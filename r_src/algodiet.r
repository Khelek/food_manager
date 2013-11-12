calculatediet <- function(M1, v) {
  len <- length(M1) / length(v) 
  # the inequalities (all pi>0)
  G <- diag(len)
  H <- rep(0, len)
  
  # standart deviation of B
  sdB = rep(5, length(v))
  
  A = t(M1)
  
  res <- xsample(A, B = v, G = G, H = H, sdB = sdB, iter = 4000, burninlength = 1000)
  
  rbind(res$X[2000, ], res$X[2500, ], res$X[3000, ], res$X[3200, ], res$X[3500, ], res$X[3800, ], res$X[4000, ])
}
