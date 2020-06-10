#!/usr/bin/env Rscript
#############################################
# SP500
source("./funs-ports-IT.R")
spy5 <- rets.sp("./data/spy5-rets.rds")
spy5.coefs  <- readRDS("./data/spy5-coefs.rds")

# REOPT
spy5.w1 <- lapply(spy5.coefs, function(x) reopt.mv(retx(spy5$ret, spy5$mkt), J=500, x))

# REBAL
spy5.ws.20 <- lapply(spy5.w1, function(x) fun.rebal(x, spy5$ret, 20, J=500))
spy5.ws.40 <- lapply(spy5.w1, function(x) fun.rebal(x, spy5$ret, 40, J=500))
spy5.ws.60 <- lapply(spy5.w1, function(x) fun.rebal(x, spy5$ret, 60, J=500))
    
# BIND
spy5.w <- c(spy5.ws.20, spy5.ws.40, spy5.ws.60)
saveRDS(spy5.w, "./data/spy5-weights.rds")

# RPS
spy5 <- c(spy5, rets.oos(spy5.w, spy5$ret, J=500, c=50) )
saveRDS(spy5, "./data/spy5-rps.rds")

#############################################
# IBOV
ibov <- rets.ibov("./data/ibov-rets.rds")
ibov.coefs  <- readRDS("./data/ibov-coefs.rds")

# REOPT
ibov.w1 <- lapply(ibov.coefs, function(x) reopt.mv(retx(ibov$ret, ibov$mkt), J=250, x))

# REBAL
ibov.ws.20 <- lapply(ibov.w1, function(x) fun.rebal(x, ibov$ret, 20, J=250))
ibov.ws.40 <- lapply(ibov.w1, function(x) fun.rebal(x, ibov$ret, 40, J=250))
ibov.ws.60 <- lapply(ibov.w1, function(x) fun.rebal(x, ibov$ret, 60, J=250))
    
# BIND
ibov.w <- c(ibov.ws.20, ibov.ws.40, ibov.ws.60)
saveRDS(ibov.w, "./data/ibov-weights.rds")

# RPS
ibov <- c(ibov, rets.oos(ibov.w, ibov$ret, J=250, c=50) )
saveRDS(ibov, "./data/ibov-rps.rds")

#################################################
cat(" \n ***** FIM DO SCRIPT ****** \n")
