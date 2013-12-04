library("limSolve")

calculatediet <- function(M1, v) {
  # the inequalities (all pi>0)
  len <- length(M1) / length(v) 
  G <- diag(len)
  H <- rep(0, len)
  
  # standart deviation of B
  sdB = c(150000, 600000, rep(60000, 3), rep(10000, 6), rep(500, length(v) - 11))
  # 1500 * 100(price), 200 * 30(day) * 100(ccal), ( 20 * 30(day) * 100 )(3), (3.33 * 30(day) * 100)(6)


  # ldei solve
  #res = ldei(E = M1, F = v, G = G, H = H)
  #res$X
  
  # solve
   res <- xsample(A = M1, B = v, G = G, H = H, sdB = sdB, iter = 100)
   res$X[100, ]


  # rbind(res$X[2000, ], res$X[2500, ], res$X[3000, ], res$X[3200, ], res$X[3500, ], res$X[3800, ], res$X[4000, ])
}
