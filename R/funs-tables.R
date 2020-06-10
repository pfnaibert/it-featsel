#############################################
# 
#############################################

#############################################
# TABLE BOOT
boot.table <- function(data, bench, est, rav, b, M, dataname)
{

N <- NCOL(data); tab <- matrix(NA, N, 2)
rownames(tab) <- colnames(data)
colnames(tab) <- c("Difference", "$p$-value")

cat("\n ***", dataname, "***")
for(i in (1:N)){tab[i,] <- unlist(boot.test(cbind(data[,i], bench), est, rav, b, M) )}

tab   <- 100*tab

return(round(tab, 2))
}

#############################################
# TABLE BOOT RPS
boot.table.rps <- function(data, bench, est, rav, M, dataname)
{

pval.b.02 <- boot.table(data, bench, est, rav, b=02, M, dataname)
pval.b.04 <- boot.table(data, bench, est, rav, b=04, M, dataname)
pval.b.06 <- boot.table(data, bench, est, rav, b=06, M, dataname)
pval.b.08 <- boot.table(data, bench, est, rav, b=08, M, dataname)
pval.b.10 <- boot.table(data, bench, est, rav, b=10, M, dataname)

pval.tab <- cbind(pval.b.02, pval.b.04[,2], pval.b.06[,2], pval.b.08[,2], pval.b.10[,2])
colnames(pval.tab) <- c("Difference", "$b=02$", "$b=04$", "$b=06$", "$b=08$", "$b=10$")

return(pval.tab)
}

#####################################

#' # REFERENCES
