library("limSolve")

getx0 <- function(E, F, G, H) {
    print("start")
    print(E[1,])
    print(E[2,])
    print(E[3,])
    l <- tryCatch(ldei(E,F,G,H), error = function(e) {
       budget = F[1]
       for(i in 1:10) {
             for(k in 1:2) {
                   F[1] = budget + ((-1)**k) * (710 * 100 * i)
                   print(F[1] / 100)
                   l <- tryCatch(ldei(E,F,G,H), error = function(e) {
                     l = list()
                     l$residualNorm = 10
                     l
                   })
                   if (l$residualNorm <= 1e-5) {
                      return(l)
                   }
             }
       }
  })
  return(l$X)
}

calculatediet <- function(M1, v) {
  # the inequalities (all pi>0)
  len <- length(M1) / length(v) 
  G <- diag(len)
  H <- rep(0, len)
  
  # standart deviation of B
  sdB = rep(100, length(v))

  # ldei solve
  #res = ldei(E = M1, F = v, G = G, H = H)
  #res
  #res$X

  x0 <- getx0(E = M1, F = v, G = G, H = H)
  
  # solve
  # res <- xsample(A = M1, B = v, G = G, H = H, x0 = x0, sdB = sdB, iter = 100)
  # res$X[100, ]


  # rbind(res$X[2000, ], res$X[2500, ], res$X[3000, ], res$X[3200, ], res$X[3500, ], res$X[3800, ], res$X[4000, ])
}
