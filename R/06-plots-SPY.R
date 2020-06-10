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
id2 <- grep("bwd.40", colnames(rps.ac))
id3 <- grep("fwd.30", colnames(rps.ac))
id4 <- grep("fwd.40", colnames(rps.ac))
id5 <- grep("las.20", colnames(rps.ac))

#####################################
# PLOTS 
source("./funs-plots.R")

#####################################
# las.20
par(mfrow = c(2,1))
{
plot.ac2(cbind(mkt.ac, rps.ac[,id5]), "SP500",
         "Cumulative Returns of the las.20 Portfolio without TC" )
plot.ac2(cbind(mkt.ac, rpn.ac[,id5]), "SP500",
         "Cumulative Returns of the las.20 Portfolio with TC" )
}

par(mfrow = c(2,1))
{
plot.vol(cbind(mkt, ports$rps[,id5]), "SP500", "las.20", W)
plot.vol(cbind(mkt, ports$rpn[,id5]), "SP500", "las.20 TC", W)
}

#####################################
# bwd.30

par(mfrow = c(2,1))
{
plot.ac2(cbind(mkt.ac, rps.ac[,id1]), "SP500",
         "Cumulative Returns of the bwd.30 Portfolio without TC" )
plot.ac2(cbind(mkt.ac, rpn.ac[,id1]), "SP500",
         "Cumulative Returns of the bwd.30 Portfolio with TC" )
}

par(mfrow = c(2,1))
{
plot.vol(cbind(mkt, ports$rps[,id1]), "SP500", "bwd.30", W)
plot.vol(cbind(mkt, ports$rpn[,id1]), "SP500", "bwd.30 TC", W)
}

#####################################
# bwd.40

par(mfrow = c(2,1))
{
plot.ac2(cbind(mkt.ac, rps.ac[,id2]), "SP500",
         "Cumulative Returns of the bwd.40 Portfolio without TC" )
             
plot.ac2(cbind(mkt.ac, rpn.ac[,id2]), "SP500",
         "Cumulative Returns of the bwd.40 Portfolio with TC" )
}

par(mfrow = c(2,1))
{
plot.vol(cbind(mkt, ports$rps[,id2]), "SP500", "bwd.40", W)
plot.vol(cbind(mkt, ports$rpn[,id2]), "SP500", "bwd.40 TC", W)
}

#####################################
# fwd.30
par(mfrow = c(2,1))
{
plot.ac2(cbind(mkt.ac, rps.ac[,id3]), "SP500",
         "Cumulative Returns of the fwd.30 Portfolio without TC" )
             
plot.ac2(cbind(mkt.ac, rpn.ac[,id3]), "SP500",
         "Cumulative Returns of the fwd.30 Portfolio with TC" )
}

par(mfrow = c(2,1))
{
plot.vol(cbind(mkt, ports$rps[,id3]), "SP500", "fwd.30", W)
plot.vol(cbind(mkt, ports$rpn[,id3]), "SP500", "fwd.30 TC", W)
}

#####################################
# fwd.40

par(mfrow = c(2,1))
{
plot.ac2(cbind(mkt.ac, rps.ac[,id4]), "SP500",
         "Cumulative Returns of the fwd.40 Portfolio without TC" )
             
plot.ac2(cbind(mkt.ac, rpn.ac[,id4]), "SP500",
         "Cumulative Returns of the fwd.40 Portfolio with TC" )
}

par(mfrow = c(2,1))
{
plot.vol(cbind(mkt, ports$rps[,id4]), "SP500", "fwd.40", W)
plot.vol(cbind(mkt, ports$rpn[,id4]), "SP500", "fwd.40 TC", W)
}

#############################################
# SAVES
pdf(file = paste0("../escrita/figs/fig-", indexname, "-retac-", i, ".pdf"), width = 2*5, height=5)
plot.vol(rps[,ids[i,]], indexname, titles2[i], W)
dev.off()

#############################################
cat(" \n ***** FIM DO SCRIPT ****** \n")
