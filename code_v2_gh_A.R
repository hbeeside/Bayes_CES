library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dt_gh <- read.csv(file="C:/R_code/Orbis_Services_cleaned.csv", row.names=1, header=T);

rev <- dt_gh[,1:4]
asts <- dt_gh[,5:8]
empl <- dt_gh[,9:12]

asts_n <- asts/empl
rev_n <- log(rev/empl)
rev_s <- rev_n$revenue.2014

for(i in 1:dim(rev_n)[1]) rev_s[i] <- sd(rev_n[i,])

setwd("C:/R_code/finalProject")

d_a <- list(J=dim(rev_n)[1], T=dim(rev_n)[2], k=as.matrix(asts_n), y=as.matrix(rev_n), y_s=rev_s);

fit1_gh <- stan("finalProj_v1_sav.stan", data = d_a, iter = 10000, chains = 4)

fit2_gh <- stan("finalProj_v2_sav.stan", data = d_a, iter = 40000, chains = 4)

#fit2_A_gh <- stan("finalProj_v2_sav_A.stan", data = d_a, iter = 20000, chains = 4)




es1 <- extract(fit1_us)

gma <- data.frame(matrix(0,nrow=35, ncol=4))

for(j in 1:dim(es1$gma)[2]) for(k in 1:dim(es1$gma)[3]) gma[j,k] <- mean(es1$gma[,j,k])

xar <- c("2011", "2012", "2013", "2014")
	
pdf("C:/R_code/finalProject/fig3.pdf")

par(mfrow=c(5,1), mar=c(3,3,1,1), oma=c(0,0,2,0), mgp=c(1.3,0.5,0))
for(i in 1:5) plot(xar,gma[i,], type="b", xlab="", ylab="Efficiency", cex.main=0.75, main=dt_us[i,1])

mtext("Figure 1: The Efficiency Parameter fits for U.S. Financial Service Companies", side=3, outer=TRUE, cex=0.75)

dev.off()

pdf("C:/R_code/finalProject/fig4.pdf")

par(mfrow=c(5,1), mar=c(3,3,1,1), oma=c(0,0,2,0), mgp=c(1.3,0.5,0))
for(i in 6:10) plot(xar,gma[i,], type="b", xlab="", ylab="Efficiency", cex.main=0.65, main=dt_us[i,1])

mtext("Figure 2: The Efficiency Parameter fits for U.S. Financial Service Companies", side=3, outer=TRUE, cex=0.75)

dev.off()

pdf("C:/R_code/finalProject/fig5.pdf")

par(mfrow=c(5,1), mar=c(3,3,1,1), oma=c(0,0,2,0), mgp=c(1.3,0.5,0))
for(i in 11:15) plot(xar,gma[i,], type="b", xlab="", ylab="Efficiency", cex.main=0.65, main=dt_us[i,1])

mtext("Figure 3: The Efficiency Parameter fits for U.S. Financial Service Companies", side=3, outer=TRUE, cex=0.75)

dev.off()

pdf("C:/R_code/finalProject/fig6.pdf")

par(mfrow=c(5,1), mar=c(3,3,1,1), oma=c(0,0,2,0), mgp=c(1.3,0.5,0))
for(i in 16:20) plot(xar,gma[i,], type="b", xlab="", ylab="Efficiency", cex.main=0.65, main=dt_us[i,1])

mtext("Figure 4: The Efficiency Parameter fits for U.S. Financial Service Companies", side=3, outer=TRUE, cex=0.75)

dev.off()

pdf("C:/R_code/finalProject/fig7.pdf")

par(mfrow=c(5,1), mar=c(3,3,1,1), oma=c(0,0,2,0), mgp=c(1.3,0.5,0))
for(i in 21:25) plot(xar,gma[i,], type="b", xlab="", ylab="Efficiency", cex.main=0.65, main=dt_us[i,1])

mtext("Figure 5: The Efficiency Parameter fits for U.S. Financial Service Companies", side=3, outer=TRUE, cex=0.75)

dev.off()

pdf("C:/R_code/finalProject/fig8.pdf")

par(mfrow=c(5,1), mar=c(3,3,1,1), oma=c(0,0,2,0), mgp=c(1.3,0.5,0))
for(i in 26:30) plot(xar,gma[i,], type="b", xlab="", ylab="Efficiency", cex.main=0.65, main=dt_us[i,1])

mtext("Figure 6: The Efficiency Parameter fits for U.S. Financial Service Companies", side=3, outer=TRUE, cex=0.75)

dev.off()

pdf("C:/R_code/finalProject/fig9.pdf")

par(mfrow=c(5,1), mar=c(3,3,1,1), oma=c(0,0,2,0), mgp=c(1.3,0.5,0))
for(i in 31:35) plot(xar,gma[i,], type="b", xlab="", ylab="Efficiency", cex.main=0.65, main=dt_us[i,1])

mtext("Figure 7: The Efficiency Parameter fits for U.S. Financial Service Companies", side=3, outer=TRUE, cex=0.75)

dev.off()
