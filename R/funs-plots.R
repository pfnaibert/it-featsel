#########################################################################
# PLOTS

#########################################################################
plot.ac1 <- function(data, indexname, title)
{
N <- NCOL(data)
dif <- data - data[,1]

maxy <- max(data)
miny <- min(dif)

plot(data[,1], t="l", ylim = c(miny, maxy), ylab = "Cumulative Returns", xlab = "Time",
     main = title)

abline(h=c(0, 1))

lines(data[,2], col = "red")
lines(data[,3], col = "blue")

lines(dif[,2], col = "red", lty=2)
lines(dif[,3], col = "blue", lty=2)

legend("topleft",
       c(indexname, "las.15", "las.20"),
       col = c("black", "red", "blue"),
       lty = 1, lwd = 2)
}

#########################################################################
plot.ac2 <- function(data, indexname, title)
{
N <- NCOL(data)
dif <- data - data[,1]

maxy <- max(data)
miny <- min(dif)

plot(data[,1], t="l", ylim = c(miny, maxy), ylab = "Cumulative Returns", xlab = "Time",
     main = title)

abline(h=c(0, 1))

lines(data[,2], col = "red")
lines(data[,3], col = "blue")
lines(data[,4], col = "green")

lines(dif[,2], col = "red", lty=2)
lines(dif[,3], col = "blue", lty=2)
lines(dif[,4], col = "green", lty=2)

legend("topleft",
       c(indexname,
         "Monthly", "Bimonthly", "Quarterly"),
       col = c("black", "red", "blue", "green"),
       lty = 1, lwd = 2)
}

#########################################################################
# plot vol

plot.vol <- function(data, indexname, title, W)
{
win1 <- seq(1, W)
nprev <- NROW(data[-win1,]); N <- NCOL(data)
vol1 <- vol2 <- matrix(NA, ncol = N, nrow = nprev)

for(i in 1:nprev)
{
    win <- seq(i, W+i-1)
    # vol1[i,] <- apply(data[win,], 2, function(x) 100*sqrt(250)*sd(x))   # SD
    # vol2[i,] <- apply(data[win,], 2, function(x) 100*sqrt(250)*sd(x-data[win,1])) # SD(TE)
    vol1[i,] <- apply(data[win,], 2, function(x) 100*sd(x))   # SD
    vol2[i,] <- apply(data[win,], 2, function(x) 100*sd(x-data[win,1])) # SD(TE)
}

maxy <- max(vol1); maxy <- max(vol1)

# 
plot(vol1[,1], t="l", ylim = c(0, maxy), ylab = "SD of Returns", xlab = "Time", main = title)

abline(h=0)

lines(vol1[,2], col = "red")
lines(vol1[,3], col = "blue")
lines(vol1[,4], col = "green")

lines(vol2[,2], col = "red", lty=2)
lines(vol2[,3], col = "blue", lty=2)
lines(vol2[,4], col = "green", lty=2)

legend("topleft",
       c(indexname,
         "Monthly", "Bimonthly", "Quarterly"),
       col = c("black", "red", "blue", "green"),
       lty = 1, lwd = 2)

}

#########################################################################
plot.coef.ac <- function(beta, ret, y)
{
N <- NCOL(ret); ew <- rep(1/N, N)
ew.ac <- fun.retac(apply(ret, 1, function(x) sum(x*ew) ))

w   <- beta[-1]
fit <- fun.retac(apply(ret, 1, function(x) sum(x*w) ))

y1 <- fun.retac(y)
y2 <- fun.retac(y - beta[1])

ymax <- max(cbind(y1, y2, fit) )

plot(y1, t="l", ylim = c(0, ymax))
lines(ew.ac, col="green")
lines(y2, col="blue")
lines(fit, col = "red")
}

#########################################################################
plot.weight.ac <- function(w, ret, y)
{

fit <- fun.retac(apply(ret, 1, function(x) sum(x*w) ))
y1 <- fun.retac(y)

ymax <- max(cbind(y1, fit) )

plot(y1, t="l", ylim = c(0, ymax))
lines(fit, col = "blue")
}

#########################################################################
# titlenames
titlename1 <- function(nassets, ndays)
{
return(paste("Cumulative Returns", nassets,  "assets and", ndays , "days rebalancing Without TC"))
}

titlename2 <- function(nassets, ndays)
{
return(paste("SD of Returns", nassets, "assets and", ndays, "days rebalancing (trailing 250 days)"))
}

