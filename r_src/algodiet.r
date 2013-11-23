library("limSolve")

calculatediet <- function(M1, v) {
  # the inequalities (all pi>0)
  len <- length(M1) / length(v) 
  G <- diag(len)
  H <- rep(100, len)
  
  # standart deviation of B
  sdB = rep(5, length(v))
  
  # solve
  res <- xsample(A = M1, B = v, G = G, H = H, sdB = sdB, iter = 2000, burninlength = 1000)
  res$X[2000, ]
  # rbind(res$X[2000, ], res$X[2500, ], res$X[3000, ], res$X[3200, ], res$X[3500, ], res$X[3800, ], res$X[4000, ])
}
