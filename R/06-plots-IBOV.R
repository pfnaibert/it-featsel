#!/usr/bin/env Rscript
# retport
#####################################
source("../funs/funs-ports-IT.R")
source("../funs/funs-plots.R")
library(knitr)
J <- 250; win1 <- seq(1, J)
W <- 250

#####################################
# Import
ports <- readRDS("../data/ibov-rps.rds")
str(ports); head(ports$ac.rps)

mkt    <- ports$mkt[-win1]
mkt.ac <- fun.retac(ports$mkt[-win1])
rps.ac <- ports$ac.rps
rpn.ac <- ports$ac.rpn

#####################################
portnames  <- unique(colnames(rpn.ac))
portnames1 <- gsub('\\.', "-", portnames);
n <- length(portnames); n


#####################################
# FIGURES
for(i in 1:n)
{
pdf(file = paste0("../figs/IBOV-retac-", portnames1[i], ".pdf"), width=8, height=7*sqrt(2))
par(mfrow = c(2,1))
# par(mar=c(2.5, 2, 2, 0.2))
# par(mar=c(4, 4, 2, 0.4))
{
plot.ac2(cbind(mkt.ac, rpn.ac[,grep(portnames[i], colnames(rps.ac))]), "IBOV",
         paste("Cumulative Net Returns of the", portnames[i], "Portfolio") )
plot.vol(cbind(mkt, ports$rpn[,grep(portnames[i], colnames(rps.ac))]),
        "IBOV", paste("SD of Net Returns of the", portnames[i], "portfolio (trailing 250 days)"), W)
}
dev.off()
}

#############################################
cat(" \n ***** FIM DO SCRIPT ****** \n")
