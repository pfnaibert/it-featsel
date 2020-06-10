#############################################
# 
#############################################

#########################################################################
fun.glmnet <- function(X, y, alpha, dfmax, J)
{
# depends on glmnet

#
win1 <- seq(1,J)
N <- NCOL(X); T <- NROW(X); nprev <- NROW(X[-win1,])
anames <- colnames(X); dnames <- rownames(X[-win1,])

beta <- matrix(0, nrow = nprev, ncol = N, dimnames = list(dnames, anames) )

for(i in 1:nprev)
{
  if(i%%100==0) cat(i, "de", nprev, "\n")
  win <- seq(i,J+i-1) # J+i-p

  reg <- glmnet(X[win,], y[win], alpha=1, dfmax=dfmax, nlambda = 1000); id <- length(reg$df)
  beta[i,] <- reg$beta[,id-1]

  # if(reg$df[id-1]==dfmax)
  # {
  # beta[i,] <- reg$beta[,id-1]
  # } else
  # {
  # grid <- seq(reg$lambda[id-2], reg$lambda[id], length.out = 10000)
  # reg <- glmnet(X[win,], y[win], alpha=1, dfmax=dfmax, lambda = grid); id <- length(reg$df)
  # beta[i,] <- reg$beta[,id-1]
  # }
}

return(beta)
}

#########################################################################
fun.subset.ibov <- function(X, y, J, method)
{
#
win1 <- seq(1,J)
N <- NCOL(X); T <- NROW(X); nprev <- NROW(X[-win1,])
anames <- colnames(X); dnames <- rownames(X[-win1,])

#
namat <- matrix(0, nrow = nprev, ncol = N, dimnames = list(dnames, anames) )
coef.10 <- coef.15 <- coef.20 <- namat

for(i in 1:nprev)
{
  if(i%%100==0) cat(i, "de", nprev, "\n")
  win   <- seq(i,J+i-1)
  reg   <- regsubsets(X[win,], y[win], nvmax=20, method=method, intercept=TRUE)
  
  b.10 <- coef(reg, 10); id.10 <- intersect(anames, names(b.10) )
  b.15 <- coef(reg, 15); id.15 <- intersect(anames, names(b.15) )
  b.20 <- coef(reg, 20); id.20 <- intersect(anames, names(b.20) )
  
  coef.10[i, id.10] <- b.10[-1]
  coef.15[i, id.15] <- b.15[-1]
  coef.20[i, id.20] <- b.20[-1]
}

return(list("10"=coef.10, "15"=coef.15, "20"=coef.20) )
}

#############################################
fun.subset.spy5 <- function(X, y, J, method)
{
#
win1 <- seq(1,J)
N <- NCOL(X); T <- NROW(X); nprev <- NROW(X[-win1,])
anames <- colnames(X); dnames <- rownames(X[-win1,])

#
namat <- matrix(0, nrow = nprev, ncol = N, dimnames = list(dnames, anames) )
coef.20 <- coef.30 <- coef.40 <- namat

for(i in 1:nprev)
{
  if(i%%100==0) cat(i, "de", nprev, "\n")
  win   <- seq(i,J+i-1)
  reg   <- regsubsets(X[win,], y[win], nvmax=40, method=method, intercept=TRUE)
  
  b.20 <- coef(reg, 20); id.20 <- intersect(anames, names(b.20) )
  b.30 <- coef(reg, 30); id.30 <- intersect(anames, names(b.30) )
  b.40 <- coef(reg, 40); id.40 <- intersect(anames, names(b.40) )
  
  coef.20[i, id.20] <- b.20[-1]
  coef.30[i, id.30] <- b.30[-1]
  coef.40[i, id.40] <- b.40[-1]
}

return(list("20"=coef.20, "30"=coef.30, "40"=coef.40) )
}

#############################################
bind.coef <- function(bwd, fwd, las)
{

ws <- list(# "unc"=unc,
          "bwd.10"=bwd[[1]], "fwd.10"=fwd[[1]], "las.10"=las[[1]], 
          "bwd.15"=bwd[[2]], "fwd.15"=fwd[[2]], "las.15"=las[[2]], 
          "bwd.20"=bwd[[3]], "fwd.20"=fwd[[3]], "las.20"=las[[3]]);

return(ws)
}

#########################################################################
reopt.mv <- function(ret, J, w)
{
win1 <- seq(1, J);
anames <- colnames(ret); dnames <- rownames(ret[-win1,]);
N <- length(anames); nprev <- length(dnames);
id.0 <- w!=0; 

# pre-alloc
mv <- matrix(0, nrow = nprev, ncol = N, dimnames = list(dnames, anames));

# loop
for(i in 1:nprev)
{
  if(i%%100==0)cat(i, "de", nprev, "\n")
  win <- seq(i,J+i-1); id1 <- id.0[i,];
  k <- sum(id1); ones <- rep(1, k);
  sig <- cov(ret[win, id1]); tmp <- solve(sig, ones);
  mv[i, id1] <- tmp/sum(tmp);
}

return(mv)
}

#########################################################################
featsels.ibov <- function(data, J)
{
# lasso
cat(" \n ***** LASSO ****** \n")

las1 <- fun.glmnet(data$ret, data$mkt, 1, 10, J)
las2 <- fun.glmnet(data$ret, data$mkt, 1, 15, J)
las3 <- fun.glmnet(data$ret, data$mkt, 1, 20, J)
las <- list("10"=las1, "20"=las2, "20"=las3)

# bwd and fwd
cat(" \n ***** BWD and FWD ****** \n")

bwd <- fun.subset.ibov(data$ret, data$mkt, J, "backward")
fwd <- fun.subset.ibov(data$ret, data$mkt, J, "forward")

# bind coefs
coefs <- list("bwd.10"=bwd[[1]], "fwd.10"=fwd[[1]], "las.10"=las[[1]],
             "bwd.15"=bwd[[2]], "fwd.15"=fwd[[2]], "las.15"=las[[2]],
             "bwd.20"=bwd[[3]], "fwd.20"=fwd[[3]], "las.20"=las[[3]]);

return(coefs)
}

#########################################################################
featsels.spy5 <- function(data, J)
{
# lasso
cat(" \n ***** LASSO ****** \n")

las1 <- fun.glmnet(data$ret, data$mkt, 1, 20, J)
las2 <- fun.glmnet(data$ret, data$mkt, 1, 30, J)
las3 <- fun.glmnet(data$ret, data$mkt, 1, 40, J)
las <- list("20"=las1, "30"=las2, "40"=las3)

# bwd and fwd
cat(" \n ***** BWD and FWD ****** \n")

bwd <- fun.subset.spy5(data$ret, data$mkt, J, "backward")
fwd <- fun.subset.spy5(data$ret, data$mkt, J, "forward")

# bind coefs
coefs <- list("bwd.20"=bwd[[1]], "fwd.20"=fwd[[1]], "las.20"=las[[1]],
             "bwd.30"=bwd[[2]], "fwd.30"=fwd[[2]], "las.30"=las[[2]],
             "bwd.40"=bwd[[3]], "fwd.40"=fwd[[3]], "las.40"=las[[3]]);

return(coefs)
}


#############################################
# 
rets.ibov <- function(filename)
{
# import
data <- readRDS(filename)

ret <- data[,-c(1,2)]/100
rf  <- data[,1]/100
mkt <- data[,2]/100

return(list("ret"=ret, "mkt"=mkt, "rf"=rf))
}

#############################################
# 
rets.sp <- function(filename)
{
# import
data <- readRDS(filename)
ret  <- data[,-1]/100
mkt  <- data[,1]/100

return(list("ret"=ret, "mkt"=mkt))
}

#############################################
# Excess return

retx <- function(rets, index)
{
T <- NROW(rets); N <- NCOL(rets)
mat <- matrix(index, nrow = T, ncol = N)
newdata <- rets-mat

return(newdata)
}

#########################################################################
# funções muport e sigport
muport <- function(peso, mu)
{
return(sum(peso*mu))
} 

varport <- function(peso, sigmat)
{
return(sum(crossprod(peso, sigmat)*peso))
}

sdport <- function(peso, sigmat)
{
return(sqrt(varport(peso, sigmat)))
}

#####################################################
# RETPORT RP
fun.rp <- function(w, ret, J)
{
    
    # prealloc
    win1 <- seq(1,J); nprev <- NROW(ret[-win1,])
    rp   <- rep(NA, nprev);  names(rp) <- rownames(ret[-win1,])
    
    # retorno do portfolio
    for(i in 1:nprev)
    {
        rp[i] <- sum( w[i,]*ret[J+i,] ) 
    }
    
    # output
    return(rp)
}

##########################################
# NET RETURN
fun.rpn <- function(rp, to, c)
{
	# function to compute the net portfolio return

	# net port return
	rpn <- (1+rp)*(1-c*to) -1
	# outputs
	return(rpn)
}

##################################################
# retac
fun.retac <- function(x)
{
    ret1    <- 1 + x
    ret.ac  <- cumprod(ret1)
    ret.ac <- ret.ac/ret.ac[1]
    return(ret.ac)
}

#############################################
# gross exposure
gross <- function(w)
{
return(sum(abs(w))) # negative
}

gross.ts <- function(w.ts)
{
return(apply(w.ts, 1, gross))
}

#############################################
# Short Interest
short <- function(w)
{
return(100*sum(-w[w<0])) # negative
}

short.ts <- function(w.ts)
{
return(apply(w.ts, 1, short))
}

#############################################
# Active Postions
active <- function(w)
{
return(w!=0)
}

active.ts <- function(w.ts)
{
return(rowSums(w.ts!=0))
}

#####################################################
# Turnover
fun.to <- function(w, ret, J)
{

# Liu 2009, p. 577
# TO_t = \sum_{i=1}^{N} abs(w_{i,t+1} - w_{it} \dfrac{1+r_{it}}{1+r_t w_t} )
# TO_{t} is the difference

# 
win1 <- seq(1, J); nprev <- NROW(ret[-win1,]); N <- NCOL(ret)
to   <- rep(0, nprev ) ; names(to) <- rownames(ret[-win1,])
to[1] <- sum(abs(w[1,]))
    
for(i in 2:nprev)
{
  w1    <- w.update(w[i-1,], ret[J+i-1,]) # w updated
  to[i] <- sum( abs( w[i,] - w1 ) ) # diffrence between w_{t+1} and w_{t} updated
}

return(to)
}

#################################################################
# REBAL w update
w.update <- function(w, ret)
{
# w_{it} \times \dfrac{1+r_{it}}{1+r_t w_t} ) # W updated
aux <- (1+ret)*w; w1 <- aux/sum(aux)
return(w1)    
}

#################################################################
# REBAL
fun.rebal <- function(w, ret, freq, J)
{

#
win1   <- seq(1,J); nprev <- NROW(ret[-win1,]); N <- NCOL(ret)
anames <- colnames(ret); dnames <- rownames(ret[-win1,])

w1 <- matrix(NA, nrow = nprev, ncol = N, dimnames = list(dnames, anames) )
w1[1,] <- w[1,]

## LOOP
for(i in 2:nprev)
{
  # Rebalance weights
  if((i%%freq)==0)
  {
      w1[i,] <- w[i,]
  } else if ((i%%freq)!=0)
  {
      w1[i,] <- w.update(w1[i-1,], ret[J+i-1,]) # updates itself
  } 
}

# Resposta final
return(w1)
}

#########################################################################
# UTILIDADES
u.quad <- function(mu.port, var.port, gama)
{
f <- mu.port - (gama/2)*var.port
return(f)
}

#########################################################################
# OC gama implicado
gama.imp.oc <- function(sig, mu, mualvo)
{
# bodnar 2013, on the equivalence...
n    <- NCOL(sig); ones <- rep(1, n)
tmp1 <- solve(sig, mu); tmp2 <- solve(sig, ones)
a    <- sum(mu*tmp1); b <- sum(tmp1); c <- sum(tmp2)
s    <- a - b^2/c
gama <- s/(mualvo - b/c)
return(gama)
}

#############################################
# OC mu_0 implicado

mu.imp.oc <- function(sig, mu, gama)
{
# bodnar 2013, on the equivalence...
n    <- NCOL(sig); ones <- rep(1, n)
tmp1 <- solve(sig, mu); tmp2 <- solve(sig, ones)
a    <- sum(mu*tmp1); b <- sum(tmp1); c <- sum(tmp2)
s    <- a - b^2/c
mualvo <- b/c + s/gama 
return(mualvo)
}

#####################################
# FKO
fko <- function(ret1 , ret2, gama)
{
# gama <- alpha/(1-alpha)
alpha <- gama/(1+gama)
mr1 <- mean(ret1); mr2 <- mean(ret2)
U1 <- 1+ret1 - (alpha/2)*(1+ret1)^2; U2 <- 1+ret2 - (alpha/2)*(1+ret2)^2
mu1 <- mean(U1); mu2 <- mean(U2)

fee <- -(1-gama*mr2)/gama + sqrt( (1-gama*mr2)^2 - 2*gama*(mu1-mu2) )/gama

return(fee)
}

#########################################################################
# OLS

ols.coef <- function(rps, index, digits=2)
{
reg <- lm(rps ~ index); sum <- summary(reg)
# coef(reg)
# sqrt(diag(vcov(reg)))
R2 <- sum$r.squared
IR <- as.numeric(coef(reg)[1]/sqrt(crossprod(reg$residuals)/reg$df.residual) )
ans <- c(10000*coef(sum)[1,1], 100*coef(sum)[1,4], coef(sum)[2,1], 100*coef(sum)[2,4], 100*R2, 100*IR)
names(ans) <- c("Alpha", "pval", "Beta", "pval", "R2", "IR")

return(round(ans, digits))
}

#############################################
# TAB COEF

tab.coef <- function(out, J, digits=2)
{
    tab <- cbind(t(apply(out$rps, 2, function(x)ols.coef(x, out$mkt[-seq(1, J)]) ) ),
                t(apply(out$rpn, 2, function(x)ols.coef(x,out$mkt[-seq(1, J)]) ) ) )

return(round(tab, digits) )
}

#############################################
# OUT OF SAMPLE STATS
rets.oos <- function(weights, rets, J, c)
{

# PORTFOLIO RETURN PORTRET RETPORT
rps <- sapply(weights, function(x) fun.rp(x, rets, J) )

# TURNOVER
tos <- sapply(weights, function(x) fun.to(x, rets, J))

# ACTIVE POSTIONS
active <- sapply(weights, active.ts)

# SHORT INTEREST
short <- sapply(weights, short.ts)

# GROSS EXPOSURE
gross <- sapply(weights, gross.ts)

# NET portfolio returns
rpn  <- fun.rpn(rps, tos, c/10000)

# cumulative Returns
ac.rps <- apply(rps, 2, fun.retac)
ac.rpn <- apply(rpn, 2, fun.retac)

ans <- list("rps"=rps, "rpn"=rpn, "ac.rps"=ac.rps, "ac.rpn"=ac.rpn, "tos"=tos, "gross"=gross, "short"=short, "active"=active)

return(ans)
}

#############################################
# OUT OF SAMPLE TABLE RPS
tab.rps <- function(out, J, freq=1, digits=2)
{
index <- out$mkt[-seq(1,J)]
rps   <- out$rps
rpn   <- out$rpn

# portfolio returns and its statistics
m1 <- colMeans(100*rps); v1 <- apply(100*rps, 2, var); sr1 <- m1/sqrt(v1); rho1 <- cor(rps, index)
m2 <- colMeans(100*rpn); v2 <- apply(100*rpn, 2, var); sr2 <- m2/sqrt(v2); rho2 <- cor(rpn, index)


# TO, SHORT, GROSS    
to     <- colMeans(out$tos);
gross  <- colMeans(out$gross)
short  <- colMeans(out$short);
active <- colMeans(out$active);

# tab
tab0 <- c(100*mean(index), 100*sd(index), mean(index)/sd(index), rep(NA, 9))
tab1 <- cbind(m1*freq, sqrt(v1*freq), sr1, rho1) 
tab2 <- cbind(100*to, gross, short, active)
tab3 <- cbind(m2*freq, sqrt(v2*freq), sr2, rho2)
tab <- rbind("index"=tab0, cbind(tab1, tab2, tab3) )
colnames(tab) <- c("Mean", "SD", "SR", "CORR", "TO", "Gross", "Short", "Active", "Mean", "SD", "SR", "CORR")

return(round(tab, digits))
}


#############################################
# OUT OF SAMPLE TABLE RPX
tab.rpx <- function(out, J, freq=1, digits=2)
{
index <- out$mkt[-seq(1,J)]
rps   <- retx(out$rps, index)
rpn   <- retx(out$rpn, index)

# portfolio returns and its statistics
m1 <- colMeans(100*rps); v1 <- apply(100*rps, 2, var); sr1 <- m1/sqrt(v1)
m2 <- colMeans(100*rpn); v2 <- apply(100*rpn, 2, var); sr2 <- m2/sqrt(v2)

# TO, SHORT, GROSS    
to     <- colMeans(out$tos);
gross  <- colMeans(out$gross)
short  <- colMeans(out$short);
active <- colMeans(out$active);

# tab
tab <- cbind(m1*freq, sqrt(v1*freq), sr1,
            100*to, gross, short, active,
            m2*freq, sqrt(v2*freq), sr2 )
colnames(tab) <- c("Mean", "SD", "SR", "TO", "Gross", "Short", "Active", "Mean", "SD", "SR")

return(round(tab, digits))
}

#############################################
# TABLE SINK
table.sink.rps <- function(mat, filename, title, label)
{
cols <- colnames(mat); rows <- rownames(mat)
nc   <- length(cols);  nr   <- length(rows)
tmp  <- paste(c("l", rep("r", nc), "}"), collapse = "")

# SINK
sink(filename)

# HEADER
cat("\\begin{table}[!ht] \n")
cat("\\centering \n")
cat("\\footnotesize \n")
cat("\\caption{"); cat(title); cat("} \n")
cat("\\vspace{-1 em} \n")
cat("\\label{"); cat(label); cat("} \n")
#        cat("\\scalebox{0.90}{"); cat(" \n")         # SCALEBOX
cat("\\begin{threeparttable} \n")
#        cat("\\begin{tabular}{", tmp, "\n")                      # no extracolsep
cat("\\begin{tabular}{@{\\extracolsep{1 ex}}", tmp, "\n") # extracolsep
cat("\\\\[-1.8ex] \\hline \\hline \n")

cat("% ---------------------------------------- \n")
cat(" ", cols, sep=" & "); cat(" \\\\ \n")
cat("% ---------------------------------------- \n")
cat("\\hline \\\\[-1.8ex] \n")

# BODY

# INDEX
cat("% ---------------------------------------- \n")
cat(rows[1]) ; cat(sprintf(" & $%3.2f$", mat[1,])); cat(" \\\\ \n")
cat("\\hline \\\\[-1.8ex] \n")

# MONTHLY
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel A: Monthly Rebalancing}} \\\\ \n")
for(i in 2:10)
{
cat(rows[i]) ; cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\\\[-1.8ex] \n")

# FF25
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel B: Bimonthly Rebalancing}} \\\\ \n")
for(i in 11:19)
{
cat(rows[i]); cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\\\[-1.8ex] \n")

# IBOV
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel C: Quarterly Rebalancing}} \\\\ \n")
for(i in 20:28)
{
cat(rows[i]); cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\hline \n")

# TAIL
cat("\\end{tabular} \n")
cat("\\vspace{-1 ex} \n")
cat("\\begin{tablenotes} \n")
cat("\\textbf{Source:} The author.\n")
#     cat("\\textbf{Note:} \n")
#     cat("The table shows...")
cat("\n")
cat("\\end{tablenotes} \n")
cat("\\end{threeparttable} \n")     # sem scalebox
cat("\\end{table} \n")
#      cat("\\restoregeometry")

sink()
    
}

#############################################
# TABLE SINK

table.sink.rpx <- function(mat, filename, title, label)
{
cols <- colnames(mat); rows <- rownames(mat)
nc   <- length(cols);  nr   <- length(rows)
tmp  <- paste(c("l", rep("r", nc), "}"), collapse = "")

# SINK
sink(filename)

# HEADER
cat("\\begin{table}[!ht] \n")
cat("\\centering \n")
cat("\\footnotesize \n")
cat("\\caption{"); cat(title); cat("} \n")
cat("\\vspace{-1 em} \n")
cat("\\label{"); cat(label); cat("} \n")
#        cat("\\scalebox{0.90}{"); cat(" \n")         # SCALEBOX
cat("\\begin{threeparttable} \n")
#        cat("\\begin{tabular}{", tmp, "\n")                      # no extracolsep
cat("\\begin{tabular}{@{\\extracolsep{1 ex}}", tmp, "\n") # extracolsep
cat("\\\\[-1.8ex] \\hline \\hline \n")

cat("% ---------------------------------------- \n")
cat(" ", cols, sep=" & "); cat(" \\\\ \n")
cat("% ---------------------------------------- \n")
cat("\\hline \\\\[-1.8ex] \n")

# BODY

# MONTHLY
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel A: Monthly Rebalancing}} \\\\ \n")
for(i in 1:9)
{
cat(rows[i]) ; cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\\\[-1.8ex] \n")

# BIMONTHLY
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel B: Bimonthly Rebalancing}} \\\\ \n")
for(i in 10:18)
{
cat(rows[i]); cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\\\[-1.8ex] \n")

# QUARTERLY
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel C: Quarterly Rebalancing}} \\\\ \n")
for(i in 19:27)
{
cat(rows[i]); cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\hline \n")

# TAIL
cat("\\end{tabular} \n")
cat("\\vspace{-1 ex} \n")
cat("\\begin{tablenotes} \n")
cat("\\textbf{Source:} The author.\n")
#     cat("\\textbf{Note:} \n")
#     cat("The table shows...")
cat("\n")
cat("\\end{tablenotes} \n")
cat("\\end{threeparttable} \n")     # sem scalebox
cat("\\end{table} \n")
#      cat("\\restoregeometry")

sink()
    
}

#############################################
# TABLE SINK PVAL

table.sink.pval <- function(mat, filename, title, label)
{
cols <- colnames(mat); rows <- rownames(mat)
nc   <- length(cols);  nr   <- length(rows)
tmp  <- paste(c("l", rep("r", nc), "}"), collapse = "")

# SINK
sink(filename)

# HEADER
cat("\\begin{table}[!ht] \n")
cat("\\centering \n")
cat("\\footnotesize \n")
cat("\\caption{"); cat(title); cat("} \n")
cat("\\vspace{-1 em} \n")
cat("\\label{"); cat(label); cat("} \n")
#        cat("\\scalebox{0.90}{"); cat(" \n")         # SCALEBOX
cat("\\begin{threeparttable} \n")
#        cat("\\begin{tabular}{", tmp, "\n")                      # no extracolsep
cat("\\begin{tabular}{@{\\extracolsep{1 ex}}", tmp, "\n") # extracolsep
cat("\\\\[-1.8ex] \\hline \\hline \n")

cat("% ---------------------------------------- \n")
cat(" ", cols, sep=" & "); cat(" \\\\ \n")
cat("% ---------------------------------------- \n")
cat("\\hline \\\\[-1.8ex] \n")

# BODY

# MONTHLY
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel A: Monthly Rebalancing}} \\\\ \n")
for(i in 1:9)
{
cat(rows[i]) ; cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\\\[-1.8ex] \n")

# Bimonthly
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel B: Bimonthly Rebalancing}} \\\\ \n")
for(i in 10:18)
{
cat(rows[i]); cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\\\[-1.8ex] \n")

# Quarterly
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel C: Quarterly Rebalancing}} \\\\ \n")
for(i in 19:27)
{
cat(rows[i]); cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\hline \n")

# TAIL
cat("\\end{tabular} \n")
cat("\\vspace{-1 ex} \n")
cat("\\begin{tablenotes} \n")
cat("\\textbf{Source:} The author. \n")
#     cat("\\textbf{Note:} \n")
#     cat("The table shows...")
cat("\n")
cat("\\end{tablenotes} \n")
cat("\\end{threeparttable} \n")     # sem scalebox
cat("\\end{table} \n")
#      cat("\\restoregeometry")

sink()
    
}

