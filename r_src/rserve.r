require('Rserve')
source("r_src/algodiet.r")



if(port == "") {
  port <- 8085
} 
port
run.Rserve(debug = FALSE, port = port, args = paste("--RS-port ", port), config.file = "./rserve.conf")
