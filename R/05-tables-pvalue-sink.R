###########################################
source("./funs-ports-IT.R")
source("./funs-LW-SR-VAR.R")
source("./funs-tables.R")
library(knitr)

#####################################
# Import
mu.ibov.rpn <- readRDS("./data/pval-mu-ibov-rpn.rds")
mu.spy5.rpn <- readRDS("./data/pval-mu-spy5-rpn.rds")

var.ibov.rpn <- readRDS("./data/pval-var-ibov-rpn.rds")
var.spy5.rpn <- readRDS("./data/pval-var-spy5-rpn.rds")

sr.ibov.rps <- readRDS("./data/pval-sr-ibov-rps.rds")
sr.spy5.rps <- readRDS("./data/pval-sr-spy5-rps.rds")
sr.ibov.rpn <- readRDS("./data/pval-sr-ibov-rpn.rds")
sr.spy5.rpn <- readRDS("./data/pval-sr-spy5-rpn.rds")

#############################################
# MU
table.sink.pval(round(mu.ibov.rpn, 2), "../TABS/pval-mu-rpn-ibov.tex",
                "$p$-values for Difference of Mean of Portfolio Net Returns for the Brazilian Dataset", "tab:pval:mu:rpn:ibov")

table.sink.pval(round(mu.spy5.rpn, 2), "../TABS/pval-mu-rpn-spy5.tex",
                "$p$-values for Difference of Mean of Portfolio Net Returns for the American Dataset", "tab:pval:mu:rpn:spy5")

#############################################
# VAR
table.sink.pval(round(var.ibov.rpn, 2), "../TABS/pval-var-rpn-ibov.tex",
                "$p$-values for Difference of Log Variance of Portfolio Net Returns for the Brazilian Dataset", "tab:pval:var:rpn:ibov")

table.sink.pval(round(var.spy5.rpn, 2), "../TABS/pval-var-rpn-spy5.tex",
                "$p$-values for Difference of Log Variance of Portfolio Net Returns for the American Dataset", "tab:pval:var:rpn:spy5")

#############################################
# SR

# RPS
table.sink.pval(round(sr.ibov.rps, 2), "../TABS/pval-sr-rps-ibov.tex",
                "$p$-values for Difference of SR of Portfolio Returns for the Brazilian Dataset", "tab:pval:sr:rps:ibov")

table.sink.pval(round(sr.spy5.rps, 2), "../TABS/pval-sr-rps-spy5.tex",
                "$p$-values for Difference of SR of Portfolio Returns for the American Dataset", "tab:pval:sr:rps:spy5")

# RPN
table.sink.pval(round(sr.ibov.rpn, 2), "../TABS/pval-sr-rpn-ibov.tex",
                "$p$-values for Difference of SR of Portfolio Net Returns for the Brazilian Dataset", "tab:pval:sr:rpn:ibov")

table.sink.pval(round(sr.spy5.rpn, 2), "../TABS/pval-sr-rpn-spy5.tex",
                "$p$-values for Difference of SR of Portfolio Net Returns for the American Dataset", "tab:pval:sr:rpn:spy5")

#####################################
# END
cat("\n *** END OF SCRIPT *** \n")
print(Sys.time())
