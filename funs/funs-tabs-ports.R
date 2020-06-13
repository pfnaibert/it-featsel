#############################################
# TABLES
# Original: BEFORE 2019 october 22
# Current: 2019 october 22
#############################################

#############################################
# IN SAMPLE TABLE
tab.is <- function(data, freq=1)
{

# statistics
sig <- cov(data); mus  <- colMeans(data)

# IS PORT
ws <- ports(data)
rp <- sapply(ws, function(x) fun.rp.is(x, data) )

# mean and var port
m1 <- colMeans(100*rp) ; v1 <- apply(100*rp, 2, var) ; sr1 <- m1/sqrt(v1)
g.imp <- gama.imp.oc(sig, mus, m1/100)

# Utility
ceq.1 <- 100*u.quad(m1/100, v1/10000, 1); ceq.10 <- 100*u.quad(m1/100, v1/10000, 10)

# weights    
short <- sapply(ws, short)
gross <- sapply(ws, gross)

# TABS
tab1 <- cbind("Mean"=m1*freq, "SD"=sqrt(v1*freq), "SR"=sr1*sqrt(freq))
tab2 <- cbind(tab1, "CEQ 1"=ceq.1, "CEQ 10"=ceq.10, "Gamma"=g.imp)
tab2["MV","Gamma"] <- Inf
tab3 <- cbind(tab2, "Gross"=gross, "Short"=short)

return(round(tab3, 2) )
}

#############################################
# OUT OF SAMPLE TABLE
tab.oos <- function(weights, rets, J, freq=1)
{

aux <- rets.oos(weights, rets, J, c=50)

# portfolio returns and its statistics
m1 <- colMeans(100*aux$rp) ; v1 <- apply(100*aux$rp, 2, var) ; sr1 <- m1/sqrt(v1)
m2 <- colMeans(100*aux$rpn); v2 <- apply(100*aux$rpn, 2, var); sr2 <- m2/sqrt(v2)

# CEQ
ceq.1   <- 100*u.quad(m1/100, v1/10000, 1); ceq.10   <- 100*u.quad(m1/100, v1/10000, 10) 
# ceq.1[abs(ceq.1) > 9999] <- NA
# ceq.5[abs(ceq.5) > 9999] <- NA

ceq.1.n <- 100*u.quad(m2/100, v2/10000, 1); ceq.10.n <- 100*u.quad(m2/100, v2/10000, 10)
# ceq.1.n[abs(ceq.1.n) > 9999] <- NA
# ceq.5.n[abs(ceq.5.n) > 9999] <- NA

# TO, SHORT, GROSS    
to <- colMeans(aux$to)
short <- colMeans(aux$short)
gross <- colMeans(aux$gross)

# tab
tab <- cbind(m1*freq, sqrt(v1*freq), sr1*sqrt(freq), ceq.1, ceq.10,
            100*to, gross, short,
            # m2*freq, sqrt(v2*freq), sr2*sqrt(freq), ceq.1.n, ceq.10.n)
            sr2*sqrt(freq), ceq.1.n, ceq.10.n)

colnames(tab) <- c("Mean", "SD", "SR", "CEQ 1", "CEQ 10",
                  "TO", "Gross", "Short",
                  # "Mean", "SD", "SR", "CEQ 1", "CEQ 5") 
                  "SR", "CEQ 1", "CEQ 10")

return(round(tab, 2) )
}


#############################################
# TABLE SINK

table.sink <- function(mat, filename, title, label)
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

# FF10
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel A: FF10 Dataset}} \\\\ \n")
for(i in 1:4)
{
cat(rows[i]) ; cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\\\[-1.8ex] \n")

# FF25
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel B: FF25 Dataset}} \\\\ \n")
for(i in 5:8)
{
cat(rows[i]); cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\\\[-1.8ex] \n")

# IBOV
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel C: IBOV Dataset}} \\\\ \n")
for(i in 9:12)
{
cat(rows[i]); cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\hline \n")

# TAIL
cat("\\end{tabular} \n")
cat("\\vspace{-1 ex} \n")
cat("\\begin{tablenotes} \n")
cat("\\textbf{Source:} The author. \\\\ \n")
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

# FF10
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel A: FF10 Dataset}} \\\\ \n")
for(i in 1:3)
{
cat(rows[i]) ; cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\\\[-1.8ex] \n")

# FF25
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel B: FF25 Dataset}} \\\\ \n")
for(i in 4:6)
{
cat(rows[i]); cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\\\[-1.8ex] \n")

# IBOV
cat("% ---------------------------------------- \n")
cat("\\multicolumn{",nc,"}{l}{\\textit{Panel C: IBOV Dataset}} \\\\ \n")
for(i in 7:9)
{
cat(rows[i]); cat(sprintf(" & $%3.2f$", mat[i,])); cat(" \\\\ \n")
}
cat("\\hline \\hline \n")

# TAIL
cat("\\end{tabular} \n")
cat("\\vspace{-1 ex} \n")
cat("\\begin{tablenotes} \n")
cat("\\textbf{Source:} The author. \\\\ \n")
#     cat("\\textbf{Note:} \n")
#     cat("The table shows...")
cat("\n")
cat("\\end{tablenotes} \n")
cat("\\end{threeparttable} \n")     # sem scalebox
cat("\\end{table} \n")
#      cat("\\restoregeometry")

sink()
    
}


