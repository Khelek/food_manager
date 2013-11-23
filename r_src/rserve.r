require('Rserve')
source("r_src/algodiet.r")



if(port == "") {
  port <- 8085
} 
port = 8085
run.Rserve(debug = FALSE, port = port, 8085 = paste("--RS-port ", port), config.file = "./rserve.conf")
