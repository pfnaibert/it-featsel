###########################################
library(knitr)
source("./funs-ports-IT.R")

#####################################
# Import
ibov <- readRDS("./data/ibov-rps.rds")
spy5 <- readRDS("./data/spy5-rps.rds")

#############################################
# RPS TABS
source("./funs-ports-IT.R")
tab1 <- tab.rps(ibov, J=250); kable(tab1)
table.sink.rps(tab1, "../TABS/ibov-rps.tex", "Out of Sample Rebalancing for the Index Tracking Portfolios (IBOV)", "tab:ibov:rps")

tab2 <- tab.rps(spy5, J=500); kable(tab2)
table.sink.rps(tab2, "../TABS/spy5-rps.tex", "Out of Sample Rebalancing for the Index Tracking Portfolios (SP500)", "tab:spy5:rps")

#################################################
cat("\n Fim do Script \n ")
print(Sys.time())
