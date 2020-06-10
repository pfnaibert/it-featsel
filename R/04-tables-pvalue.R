###########################################
source("./funs-ports-IT.R")
source("./funs-LW-SR-VAR.R")
source("./funs-tables.R")
t0 <- Sys.time()
cat("*** \n Starting Script at: "); print(t0)

#####################################
# Import
ibov <- readRDS("./data/ibov-rps.rds")
spy5 <- readRDS("./data/spy5-rps.rds")

#####################################
# Setup
M <- 4999; set.seed(20191709)

#############################################
# MU

#  # RPS
#  cat("\n *** Boot for mu RPS IBOV *** \n")
#  mu.ibov <- boot.table.rps(ibov$rps, ibov$mkt[-seq(1,250)], est="mu", rav=1, M, "IBOV RPS")
#  saveRDS(mu.ibov, "./data/pval-mu-ibov-rps.rds")
#  cat("\n *** Boot for mu RPS SP500 *** \n")
#  mu.spy5 <- boot.table.rps(spy5$rps, spy5$mkt[-seq(1,500)], est="mu", rav=1, M, "SP500 RPS")
#  saveRDS(mu.spy5, "./data/pval-mu-spy5-rps.rds")

# RPN
mu.ibov <- boot.table.rps(ibov$rpn, ibov$mkt[-seq(1,250)], est="mu", rav=1, M, "IBOV RPN")
saveRDS(mu.ibov, "./data/pval-mu-ibov-rpn.rds")
t1 <- Sys.time(); print(t1); print(t1-t0)

mu.spy5 <- boot.table.rps(spy5$rpn, spy5$mkt[-seq(1,500)], est="mu", rav=1, M, "SP500 RPN")
saveRDS(mu.spy5, "./data/pval-mu-spy5-rpn.rds")
t1 <- Sys.time(); print(t1); print(t1-t0)

#############################################
# VAR
#  # RPS
#  cat("\n *** Boot for var RPS IBOV *** \n")
#  var.ibov <- boot.table.rps(ibov$rps, ibov$mkt[-seq(1,250)], est="var", rav=1, M, "IBOV RPS")
#  saveRDS(var.ibov, "./data/pval-var-ibov-rps.rds")
#  cat("\n *** Boot for var RPS SP500 *** \n")
#  var.spy5 <- boot.table.rps(spy5$rps, spy5$mkt[-seq(1,500)], est="var", rav=1, M, "SP500 RPS")
#  saveRDS(var.spy5, "./data/pval-var-spy5-rps.rds")

# RPN
var.ibov <- boot.table.rps(ibov$rpn, ibov$mkt[-seq(1,250)], est="var", rav=1, M, "IBOV RPN")
saveRDS(var.ibov, "./data/pval-var-ibov-rpn.rds")
t1 <- Sys.time(); print(t1); print(t1-t0)

var.spy5 <- boot.table.rps(spy5$rpn, spy5$mkt[-seq(1,500)], est="var", rav=1, M, "SP500 RPN")
saveRDS(var.spy5, "./data/pval-var-spy5-rpn.rds")
t1 <- Sys.time(); print(t1); print(t1-t0)

#############################################
# SR

# RPS
sr.ibov <- boot.table.rps(ibov$rps, ibov$mkt[-seq(1,250)], est="sr", rav=1, M, "IBOV RPS")
saveRDS(sr.ibov, "./data/pval-sr-ibov-rps.rds")
t1 <- Sys.time(); print(t1); print(t1-t0)

sr.spy5 <- boot.table.rps(spy5$rps, spy5$mkt[-seq(1,500)], est="sr", rav=1, M, "SP500 RPS")
saveRDS(sr.spy5, "./data/pval-sr-spy5-rps.rds")
t1 <- Sys.time(); print(t1); print(t1-t0)

# RPN
sr.ibov <- boot.table.rps(ibov$rpn, ibov$mkt[-seq(1,250)], est="sr", rav=1, M, "IBOV RPN")
saveRDS(sr.ibov, "./data/pval-sr-ibov-rpn.rds")
t1 <- Sys.time(); print(t1); print(t1-t0)

sr.spy5 <- boot.table.rps(spy5$rpn, spy5$mkt[-seq(1,500)], est="sr", rav=1, M, "SP500 RPN")
saveRDS(sr.spy5, "./data/pval-sr-spy5-rpn.rds")
t1 <- Sys.time(); print(t1); print(t1-t0)

#####################################
# END
cat("\n *** END OF SCRIPT *** \n")
print(Sys.time())
