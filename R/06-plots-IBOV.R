#!/usr/bin/env Rscript
# retport
#####################################
source("./funs-ports-IT.R")
source("./funs-plots.R")
library(knitr)
J <- 250; win1 <- seq(1, J)
W <- 250

#####################################
# Import
ports <- readRDS("./data/ibov-rps.rds")
str(ports); head(ports$ac.rps)

mkt    <- ports$mkt[-win1]
mkt.ac <- fun.retac(ports$mkt[-win1])
rps.ac <- ports$ac.rps
rpn.ac <- ports$ac.rpn

#####################################
id1 <- grep("las.15", colnames(rps.ac))
id2 <- grep("las.20", colnames(rps.ac))
id3 <- grep("bwd.20", colnames(rps.ac))
id4 <- grep("fwd.20", colnames(rps.ac))

#####################################
# PLOTS 
source("./funs-plots.R")

#####################################
# las.15
par(mfrow = c(2,1))
{
plot.ac2(cbind(mkt.ac, rps.ac[,id1]), "IBOV",
         "Cumulative Returns of the las.15 Portfolio without TC" )
plot.ac2(cbind(mkt.ac, rpn.ac[,id1]), "IBOV",
         "Cumulative Returns of the las.15 Portfolio with TC" )
}

par(mfrow = c(2,1))
{
plot.vol(cbind(mkt, ports$rps[,id1]), "IBOV", "las.15", W)
plot.vol(cbind(mkt, ports$rpn[,id1]), "IBOV", "las.15 TC", W)
}

#####################################
# las.20
par(mfrow = c(2,1))
{
plot.ac2(cbind(mkt.ac, rps.ac[,id2]), "IBOV",
         "Cumulative Returns of the las.20 Portfolio without TC" )
plot.ac2(cbind(mkt.ac, rpn.ac[,id2]), "IBOV",
         "Cumulative Returns of the las.20 Portfolio with TC" )
}

par(mfrow = c(2,1))
{
plot.vol(cbind(mkt, ports$rps[,id2]), "IBOV", "las.20", W)
plot.vol(cbind(mkt, ports$rpn[,id2]), "IBOV", "las.20 TC", W)
}

#####################################
# bwd.20
par(mfrow = c(2,1))
{
plot.ac2(cbind(mkt.ac, rps.ac[,id3]), "IBOV",
         "Cumulative Returns of the bwd.20 Portfolio without TC" )
plot.ac2(cbind(mkt.ac, rpn.ac[,id3]), "IBOV",
         "Cumulative Returns of the bwd.20 Portfolio with TC" )
}

par(mfrow = c(2,1))
{
plot.vol(cbind(mkt, ports$rps[,id3]), "IBOV", "bwd.20", W)
plot.vol(cbind(mkt, ports$rpn[,id3]), "IBOV", "bwd.20 TC", W)
}

#####################################
# fwd.20
par(mfrow = c(2,1))
{
plot.ac2(cbind(mkt.ac, rps.ac[,id4]), "IBOV",
         "Cumulative Returns of the fwd.20 Portfolio without TC" )
plot.ac2(cbind(mkt.ac, rpn.ac[,id4]), "IBOV",
         "Cumulative Returns of the fwd.20 Portfolio with TC" )
}

par(mfrow = c(2,1))
{
plot.vol(cbind(mkt, ports$rps[,id4]), "IBOV", "fwd.20", W)
plot.vol(cbind(mkt, ports$rpn[,id4]), "IBOV", "fwd.20 TC", W)
}


#############################################
# SAVES
pdf(file = paste0("../escrita/figs/fig-", indexname, "-retac-", i, ".pdf"), width = 2*5, height=5)
plot.vol(rps[,ids[i,]], indexname, titles2[i], W)
dev.off()

#############################################
cat(" \n ***** FIM DO SCRIPT ****** \n")
