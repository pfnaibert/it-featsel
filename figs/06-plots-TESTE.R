#!/usr/bin/env Rscript
# retport
#####################################
source("./funs-ports-IT.R")
source("./funs-plots.R")
library(knitr)
J <- 500; win1 <- seq(1, J)
W <- 250

#####################################
# Import
ports <- readRDS("./data/spy5-rps.rds")
str(ports); head(ports$ac.rps)

mkt    <- ports$mkt[-win1]
mkt.ac <- fun.retac(ports$mkt[-win1])
rps.ac <- ports$ac.rps
rpn.ac <- ports$ac.rpn

#####################################
id1 <- grep("bwd.30", colnames(rps.ac)); id1

#####################################
# PLOTS 
source("./funs-plots.R")

#####################################
# las.20
par(mfrow = c(2,1))
{
plot.ac2(cbind(mkt.ac, rps.ac[,id1]), "SP500",
         "Cumulative Returns of the las.20 Portfolio without TC" )
plot.ac2(cbind(mkt.ac, rpn.ac[,id1]), "SP500",
         "Cumulative Returns of the las.20 Portfolio with TC" )
}

par(mfrow = c(2,1))
{
plot.vol(cbind(mkt, ports$rps[,id1]), "SP500", "SD of Returns of the las.20 Portfolio without TC (trailing 250 days)", W)
plot.vol(cbind(mkt, ports$rpn[,id1]), "SP500", "SD of Returns of the las.20 Portfolio with TC (trailing 250 days)", W)
}

#############################################
cat(" \n ***** FIM DO SCRIPT ****** \n")
