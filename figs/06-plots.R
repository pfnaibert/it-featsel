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
portnames <- c("bwd.30", "bwd.40", "fwd.30", "fwd.40", "las.20")
n <- length(portnames); n

#####################################
# PLOTS 
source("./funs-plots.R")

#####################################
# FIGURES
for(i in 1:n)
{
plot.ac2(cbind(mkt.ac, rps.ac[,grep(portnames[i], colnames(rps.ac))]), "SP500",
         paste("Cumulative Returns of the", portnames[i], "Portfolio without TC") )
    
plot.ac2(cbind(mkt.ac, rpn.ac[,grep(portnames[i], colnames(rps.ac))]), "SP500",
         paste("Cumulative Returns of the", portnames[i], "Portfolio with TC") )

plot.vol(cbind(mkt, ports$rps[,grep(portnames[i], colnames(rps.ac))]),
        "SP500", paste("SD of returns of the", portnames[i], "portfolio without TC (trailing 250 days)"), W)

plot.vol(cbind(mkt, ports$rpn[,grep(portnames[i], colnames(rps.ac))]),
        "SP500", paste("SD of returns of the", portnames[i], "portfolio with TC (trailing 250 days)"), W)
}

#############################################
cat(" \n ***** FIM DO SCRIPT ****** \n")
