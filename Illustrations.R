
################ PIT ###########################
### True cdf
norm.sample <- rnorm(10000, mean=2.5,sd=1)

par(mar=c(6.1, 5.1, 4.1, 2.1))
plot(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=1),
     xlab=" ", ylab="Density",main="Density of random sample from N(2.5,1)", pch=18, cex.main=1.7, cex.lab=1.7, cex.axis=1.7)
dev.copy(pdf,'~/figures/Theory/NormalSample.pdf') # Save the plot
dev.off()

good.fit.norm <- pnorm(norm.sample, mean=2.5,sd=1)
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(good.fit.norm,
     main="PIT diagram for sample from proposed cdf", xlab="Quantiles",cex.main=1.7, cex.lab=1.7, cex.axis=1.7)
dev.copy(pdf,'~/figures/Theory/PITtrueFit.pdf') # Save the plot
dev.off()

# alternative to quantiles: F({y_1, ... y_n})

### Heavy right tail
gamma.sample <- rgamma(10000, shape=3.5, scale=1)

par(mar=c(6.1, 5.1, 4.1, 2.1))
plot(seq(-3,10, length=100),dgamma(seq(-3,10, length=100),shape=3.5,scale=1),ylim=c(0,0.4),
     pch=18, xlab=" ", ylab="Density", main="Random sample from Gamma(3.5,1)",cex.main=1.7, cex.lab=1.7, cex.axis=1.7)
lines(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=1), col="blue")
legend("topright", legend=c("Sample", "Prop. dist."),
       col=c("black","blue"), lty=c(NA,1), pch=c(18,NA), cex=1.1)
dev.copy(pdf,'~/figures/Theory/HeavyTailSample.pdf') # Save the plot
dev.off()

heavy.tail.fit <- pnorm(gamma.sample, mean=2.5,sd=1)
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(heavy.tail.fit,
     main="PIT diagram for sample with too heavy right tail", xlab="Quantiles",cex.main=1.6, cex.lab=1.7, cex.axis=1.7)
dev.copy(pdf,'~/figures/Theory/HeavyTail.pdf') # Save the plot
dev.off()

### Light tails
norm.light.sample <- rnorm(10000, mean=2.5, sd=0.5)
par(mar=c(6.1, 5.1, 4.1, 2.1))
plot(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=0.5), pch=18, xlab=" ", ylab="Density",
     main = "Random sample from N(2.5,0.5)",cex.main=1.7, cex.lab=1.7, cex.axis=1.7)
lines(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=1), col="blue")
legend("topright", legend=c("Sample", "Prop. dist."),
       col=c("black","blue"), lty=c(NA,1), pch=c(18,NA),cex=1.1)
dev.copy(pdf,'~/figures/Theory/LightsTailsSample.pdf') # Save the plot
dev.off()

light.tail.fit <- pnorm(norm.light.sample, mean=2.5,sd=1)
par(mar=c(6.1, 5.1, 4.1, 2.1))
hist(light.tail.fit, main="PIT diagram for sample with too light tails", xlab = "Quantiles",cex.main=1.7, cex.lab=1.7, cex.axis=1.7)
dev.copy(pdf,'~/figures/Theory/LightsTails.pdf') # Save the plot
dev.off()


### Heavy tails
norm.heavy.sample <- rnorm(10000, mean=0, sd=3)

plot(seq(-5,5, length=100),dnorm(seq(-5,5, length=100),mean=0,sd=3))

heavy.tails.fit <- pnorm(norm.heavy.sample)
hist(heavy.tails.fit)

############################ QQ PLOT


# Observed values
obs.corr <- rweibull(500,shape=1.7, scale=1.3)

# Correct distribution
plot(seq(0,5, length=500),dweibull(seq(0,5, length=500),shape=1.7, scale=1.3),
     ylab="Density", xlab=" ",main="Density of random sample from Weibull(1.3,1.7)", pch=18)
dev.copy(pdf,'~/figures/Theory/QQ/Correct.pdf') # Save the plot
dev.off()

quant.corr <- pweibull(obs.corr,shape=1.7, scale=1.3)
quant.sort.corr <- sort(quant.corr)

plot(c(1:500/500), quant.sort.corr, main="QQ-plot for sample from proposed distribution",
     ylab="Observed quantiles",xlab="Theoretical quantiles", pch=20)
lines(c(0:1),c(0,1), col="steelblue",lwd=3)
dev.copy(pdf,'~/figures/Theory/QQ/QQCorrect.pdf') # Save the plot
dev.off()

# skewed right tail distribution
plot(seq(0,5, length=500),dweibull(seq(0,5, length=500),shape=1.7, scale=1.3),
     ylab="Density", xlab=" ",main="Probabilitity density of Weibull(1.8,1.7)", pch=18,ylim=c(0,0.7))
lines(seq(0,5, length=1000),dweibull(seq(0,5, length=1000),shape=1.7, scale=1.8), col="blue")
legend("topright", legend=c("Sample", "Prop. dist."),
       col=c("black","blue"), lty=c(NA,1), pch=c(18,NA))
dev.copy(pdf,'~/figures/Theory/QQ/SkewedRight.pdf') # Save the plot
dev.off()

quant.sr <- pweibull(obs.corr,shape=1.7, scale=1.8)
quant.sort.sr <- sort(quant.sr)

plot(c(1:500/500), quant.sort.sr, main="QQ-plot for right skewed proposed dist.",
     ylab="Observed quantiles",xlab="Theoretical quantiles", pch=20)
lines(c(0:1),c(0,1), col="steelblue",lwd=3)
dev.copy(pdf,'~/figures/Theory/QQ/QQSkewedRight.pdf') # Save the plot
dev.off()

# skewed left distribution
plot(seq(0,5, length=500),dweibull(seq(0,5, length=500),shape=1.7, scale=1.3),
     ylab="Density", xlab=" ",main="Probabilitity density of Weibull(1.1,1.7)", pch=18,ylim=c(0,0.9))
lines(seq(0,5, length=1000),dweibull(seq(0,5, length=1000),shape=1.7, scale=0.9), col="blue")
lines(seq(0,5, length=1000),dnorm(seq(0,5, length=1000),mean=1.5,sd=1), col="blue")

legend("topright", legend=c("Sample", "Prop. dist."),
       col=c("black","blue"), lty=c(NA,1), pch=c(18,NA))
dev.copy(pdf,'~/figures/Theory/QQ/SkewedLeft.pdf') # Save the plot
dev.off()

quant.sl <- pweibull(obs.corr,shape=1.7, scale=0.9)
quant.sort.sl <- sort(quant.sl)

plot(c(1:500/500), quant.sort.sl, main="QQ-plot for right skewed proposed dist.",
     ylab="Observed quantiles",xlab="Theoretical quantiles", pch=20)
lines(c(0:1),c(0,1), col="steelblue",lwd=3)
dev.copy(pdf,'~/figures/Theory/QQ/QQSkewedLeft.pdf') # Save the plot
dev.off()

######################### Correct QQ plots


# Observed values
obs.weib <- rweibull(500,shape=1.7, scale=1.3)

# True distribution
plot(seq(0,5, length=500),dweibull(seq(0,5, length=500),shape=1.7, scale=1.3),
     ylab="Density", xlab=" ",main="Density of random sample from Weibull(1.3,1.7)", pch=18)
dev.copy(pdf,'~/figures/Theory/QQ/Correct.pdf') # Save the plot
dev.off()

quant.theor <- qweibull(c(1:500/501),shape=1.7, scale=1.3)

plot(sort(obs.weib), quant.theor, main="QQ-plot for sample from proposed distribution",
     ylab="Observed quantiles",xlab="Theoretical quantiles", pch=20)
lines(c(0:5),c(0:5), col="steelblue",lwd=3)
dev.copy(pdf,'~/figures/Theory/QQ/QQCorrect.pdf') # Save the plot
dev.off()

# Gamma distribution

obs.gamma <- rgamma(500,shape=2,scale=1)
plot(seq(0,5, length=500), dgamma(seq(0,5, length=500),shape=2, scale=1),
     ylab="Density", xlab=" ",main="Density of random sample from Gamma(2,1)", pch=18,ylim=c(0,0.7))
lines(seq(0,5, length=1000),dweibull(seq(0,5, length=1000),shape=1.7, scale=1.3), col="blue")
legend("topright", legend=c("Sample", "Prop. dist."),
       col=c("black","blue"), lty=c(NA,1), pch=c(18,NA))
dev.copy(pdf,'~/figures/Theory/QQ/Gamma.pdf') # Save the plot
dev.off()


plot(sort(obs.gamma), quant.theor, main="QQ-plot for Gamma(2,1) sample",
     ylab="Observed quantiles",xlab="Theoretical quantiles", pch=20, ylim=c(0,8))
lines(c(0:8),c(0:8), col="steelblue",lwd=3)
dev.copy(pdf,'~/figures/Theory/QQ/QQGamma.pdf') # Save the plot
dev.off()



obs.norm <- sort(rnorm(seq(0,5, length=500),mean=1.5,sd=1))

quant.theor<- qweibull(c(1:500/501),shape=1.7, scale=1.3)
plot(quant.theor, obs.norm)
