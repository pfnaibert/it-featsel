#!/usr/bin/env Rscript
###########################################
library(leaps)
library(glmnet)
source("./funs-ports-IT.R")

#####################################
# import
spy5 <- rets.sp("./data/spy5-rets.rds")
ibov <- rets.ibov("./data/ibov-rets.rds")

#####################################
# SP500
spy5.coefs <- featsels.spy5(spy5, J=500)
saveRDS(spy5.coefs, "./data/spy5-coefs.rds")

#####################################
# IBOV
ibov.coefs <- featsels.ibov(ibov, J=250)
saveRDS(ibov.coefs, "./data/ibov-coefs.rds")

#################################################
cat(" \n ***** FIM DO SCRIPT ****** \n")
print(Sys.time())
