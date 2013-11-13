require('Rserve')
source("r_src/algodiet.r")

# from heroku
port <- Sys.getenv('PORT')

if(port == "") {
  port <- 8082
} 
port
run.Rserve(debug = FALSE, port = port, args = paste("--RS-port ", port), config.file = "./rserve.conf")
