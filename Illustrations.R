

### True cdf
norm.sample <- rnorm(10000, mean=2.5,sd=1)

plot(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=1),
     xlab=" ", ylab="Density",main="Density of random sample from N(2.5,1)", pch=18)
dev.copy(pdf,'~/figures/Theory/NormalSample.pdf') # Save the plot
dev.off()

good.fit.norm <- pnorm(norm.sample, mean=2.5,sd=1)
hist(good.fit.norm,
     main="PIT diagram for sample from proposed cdf", xlab="Quantiles")
dev.copy(pdf,'~/figures/Theory/PITtrueFit.pdf') # Save the plot
dev.off()

# alternative to quantiles: F({y_1, ... y_n})

### Heavy right tail
gamma.sample <- rgamma(10000, shape=3.5, scale=1)

plot(seq(-3,10, length=100),dgamma(seq(-3,10, length=100),shape=3.5,scale=1),ylim=c(0,0.4),
     pch=18, xlab=" ", ylab="Density", main="Random sample from Gamma(3.5,1)")
lines(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=1), col="blue")
legend("topright", legend=c("Sample", "Prop. dist."),
       col=c("black","blue"), lty=c(NA,1), pch=c(18,NA))
dev.copy(pdf,'~/figures/Theory/HeavyTailSample.pdf') # Save the plot
dev.off()

heavy.tail.fit <- pnorm(gamma.sample, mean=2.5,sd=1)
hist(heavy.tail.fit,
     main="PIT diagram for sample with too heavy right tail", xlab="Quantiles")

dev.copy(pdf,'~/figures/Theory/HeavyTail.pdf') # Save the plot
dev.off()

### Light tails
norm.light.sample <- rnorm(10000, mean=2.5, sd=0.5)

plot(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=0.5), pch=18, xlab=" ", ylab="Density",
     main = "Random sample from N(2.5,0.5)")
lines(seq(-3,10, length=100),dnorm(seq(-3,10, length=100),mean=2.5,sd=1), col="blue")
legend("topright", legend=c("Sample", "Prop. dist."),
       col=c("black","blue"), lty=c(NA,1), pch=c(18,NA))
dev.copy(pdf,'~/figures/Theory/LightsTailsSample.pdf') # Save the plot
dev.off()

light.tail.fit <- pnorm(norm.light.sample, mean=2.5,sd=1)
hist(light.tail.fit, main="PIT diagram for sample with too light tails", xlab = "Quantiles")
dev.copy(pdf,'~/figures/Theory/LightsTails.pdf') # Save the plot
dev.off()


### Heavy tails
norm.heavy.sample <- rnorm(10000, mean=0, sd=3)

plot(seq(-5,5, length=100),dnorm(seq(-5,5, length=100),mean=0,sd=3))

heavy.tails.fit <- pnorm(norm.heavy.sample)
hist(heavy.tails.fit)


