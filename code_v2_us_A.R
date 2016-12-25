library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dt_us <- read.csv(file="C:/R_code/Orbis_US_Fincl_cleaned.csv", row.names=1, header=T);

i = 1
while(i <= dim(dt_us)[1]) {
	dn = 0
	for(j in 2:dim(dt_us)[2]) {
		if(dt_us[i,j] < 1) {
			dt_us <- dt_us[-c(i),]
			dn = 1
			break
		}
	}
	if(dn == 0) i = i + 1
}

names(dt_us)
dt_new <- dt_us[-grep("UNION",dt_us$Company.name),]
dt_n2 <- dt_new[-grep("TRUST",dt_new$Company.name),]
dt_n3 <- dt_n2[-grep("COMMUNITY",dt_n2$Company.name),]
dt_n4 <- dt_n3[-grep("SAVINGS",dt_n3$Company.name),]
dt_n5 <- dt_n4[-grep("ASSOCIATION",dt_n4$Company.name),]
dt_n6 <- dt_n5[-grep("COUNTY",dt_n5$Company.name),]
dt_n7 <- dt_n6[-grep("NATIONAL",dt_n6$Company.name),]

dt_old <- dt_us
dt_us <- dt_n7

##Based on output of names choose rearrangement of columns...

rev <- 1e6*dt_us[,6:9]
asts <- 1e6*dt_us[,2:5]
empl <- dt_us[,10:13]

asts_n <- asts/empl
rev_n <- log(rev/empl)
rev_s <- rev_n$revenue.2014

rev_sd <- rev_n$revenue.2014

for(i in 1:dim(rev_n)[1]) rev_s[i] <- sd(rev_n[i,])

for(i in 1:dim(rev_n)[1]) rev_sd[i] <- sd(rev[i,])

setwd("C:/R_code/finalProject")

d_a <- list(J=dim(rev_n)[1], T=dim(rev_n)[2], k=as.matrix(asts_n), y=as.matrix(rev_n), y_s=rev_s);

d_b <- list(J=dim(rev_n)[1], T=dim(rev_n)[2], k=as.matrix(asts), y=as.matrix(rev), y_s=rev_sd, n=as.matrix(empl));

fit1_us <- stan("finalProj_v1_sav.stan", data = d_a, iter = 10000, chains = 4)

fit2_us <- stan("finalProj_v2_sav.stan", data = d_a, iter = 40000, chains = 4)

#fit2_A_us <- stan("finalProj_v2_sav_A.stan", data = d_a, iter = 40000, chains = 4)

es1 <- extract(fit1_us)

gma <- data.frame(matrix(0,nrow=35, ncol=4))

for(j in 1:dim(es1$gma)[2]) for(k in 1:dim(es1$gma)[3]) gma[j,k] <- mean(es1$gma[,j,k])

xar <- c("2011", "2012", "2013", "2014")
	
png("C:/R_code/finalProject/fig3.png")

par(mfrow=c(5,1), mar=c(3,3,1,1), oma=c(0,0,2,0), mgp=c(1.3,0.5,0))
for(i in 1:5) plot(xar,gma[i,], type="b", xlab="", ylab="Efficiency", cex.main=0.85, main=dt_us[i,1])

mtext("Figure 1: The Efficiency Parameter fits for U.S. Financial Service Companies", side=3, outer=TRUE, cex=0.85)

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
