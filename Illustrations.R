# References for PIT can be found in Chapter 3 in master thesis

################ PIT ###########################
### True cdf
norm.sample <- rnorm(10000, mean=2.5,sd=1)

# Density of random sample from N(2.5,1
par(mar=c(6.1, 5.1, 4.1, 2.1))
plot(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=1),
     xlab=" ", ylab="Density",main="Density of random sample from N(2.5,1)", pch=18, cex.main=1.7, cex.lab=1.7, cex.axis=1.7)
dev.copy(pdf,'~/figures/Theory/NormalSample.pdf') 
dev.off()

# PIT diagram for sample from proposed cdf
good.fit.norm <- pnorm(norm.sample, mean=2.5,sd=1)
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(good.fit.norm,
     main="PIT diagram for sample from proposed cdf", xlab="CDF-values",cex.main=1.7, cex.lab=1.7, cex.axis=1.7)
dev.copy(pdf,'~/figures/Theory/PITtrueFit.pdf') 
dev.off()


### Heavy right tail
gamma.sample <- rgamma(10000, shape=3.5, scale=1)

# Density of random sample from Gamma(3.5,1)
par(mar=c(6.1, 5.1, 4.1, 2.1))
plot(seq(-3,10, length=100),dgamma(seq(-3,10, length=100),shape=3.5,scale=1),ylim=c(0,0.4),
     pch=18, xlab=" ", ylab="Density", main="Random sample from Gamma(3.5,1)",cex.main=1.7, cex.lab=1.7, cex.axis=1.7)
lines(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=1), col="blue")
legend("topright", legend=c("Sample", "Prop. dist."),
       col=c("black","blue"), lty=c(NA,1), pch=c(18,NA), cex=1.1)
dev.copy(pdf,'~/figures/Theory/HeavyTailSample.pdf') 
dev.off()

# PIT diagram for sample with too heavy right tail
heavy.tail.fit <- pnorm(gamma.sample, mean=2.5,sd=1)
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(heavy.tail.fit,
     main="PIT diagram for sample with too heavy right tail", xlab="CDF-values",cex.main=1.6, cex.lab=1.7, cex.axis=1.7)
dev.copy(pdf,'~/figures/Theory/HeavyTail.pdf') 
dev.off()

### Light tails
norm.light.sample <- rnorm(10000, mean=2.5, sd=0.5)

# Density of random sample from N(2.5,0.5)
par(mar=c(6.1, 5.1, 4.1, 2.1))
plot(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=0.5), pch=18, xlab=" ", ylab="Density",
     main = "Random sample from N(2.5,0.5)",cex.main=1.7, cex.lab=1.7, cex.axis=1.7)
lines(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=1), col="blue")
legend("topright", legend=c("Sample", "Prop. dist."),
       col=c("black","blue"), lty=c(NA,1), pch=c(18,NA),cex=1.1)
dev.copy(pdf,'~/figures/Theory/LightsTailsSample.pdf')
dev.off()

# PIT diagram for sample with too light tails
light.tail.fit <- pnorm(norm.light.sample, mean=2.5,sd=1)
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(light.tail.fit, main="PIT diagram for sample with too light tails", xlab = "CDF-values",cex.main=1.7, cex.lab=1.7, cex.axis=1.7)
dev.copy(pdf,'~/figures/Theory/LightsTails.pdf')
dev.off()

