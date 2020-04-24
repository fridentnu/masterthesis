

# Load data from Models.R to access all the prediction distritbutions
load("MyData/Models.RData")

# Sort df.total in the order of increasing systolicbp3
df.order.obs <- df.total
df.order.obs <- df.order.obs[order(df.total$SystolicBP3),]


#############################################################################
# Participant a)

# observed systolic pressure HUNT3
obs.1 <- df.order.obs$SystolicBP3[15400]
obs.1

# person project identification number
pid.1 <- df.order.obs$PID[15400]
pid.1

index.1 <- match(pid.1, df.total$PID)
index.1

# observed systolic pressure HUNT3
obs.hunt2.1<- df.total$SystolicBP2[index.1]
obs.hunt2.1

# predicted systolic pressure HUNT3, Full Gaussian model
pred.full.1 <- full.pred.mod$fitted.values[index.1]
pred.full.1

# predicted systolic pressure HUNT3, Small Gaussian model
pred.small.1 <- small.pred.mod$fitted.values[index.1]
pred.small.1

# predicted systolic pressure HUNT3, Full gamma model
pred.full.gamma.1 <- full.pred.mod.gamma$fitted.values[index.1]
pred.full.gamma.1

# predicted systolic pressure HUNT3, Small gamma model
pred.small.gamma.1 <- small.pred.mod.gamma$fitted.values[index.1]
pred.small.gamma.1

# Residual
resid.1 <- obs.1-pred.full.1
resid.1

#### Health status
df.total$BPMed3[index.1]
df.total$CVD3[index.1]
df.total$Diabetes3[index.1]

# Plot prediction distribion
par(mar=c(6.1, 5.1, 4.1, 2.1))
plot(c(50:220),dnorm(c(50:220), mean=full.pred.mod$fitted.values[index.1], sd=full.sd.y[index.1]), type="l", col="blue",
     ylab="Probability", xlab="Systolic blood pressure [mmHg]",ylim=c(0,0.035),cex.main=1.7, cex.lab=1.8, cex.axis=1.7)
lines(c(50:220),dnorm(c(50:220), mean=small.pred.mod$fitted.values[index.1], sd=small.sd.y[index.1]), col="green")
lines(c(50:220),dgamma(c(50:220),shape=full.gamma.shape,rate=full.gamma.rate[index.1]), col="brown")
lines(c(50:220),dgamma(c(50:220),shape=small.gamma.shape,rate=small.gamma.rate[index.1]), col="orange")
abline(v=df.total$SystolicBP2[index.1], col="magenta",lwd=2,lty=2)
abline(v=df.total$SystolicBP3[index.1], col="red",lwd=2,lty=6)
abline(v=full.pred.mod$fitted.values[index.1], col="blue",lwd=2)
abline(v=small.pred.mod$fitted.values[index.1], col="green",lwd=2)
abline(v=full.pred.mod.gamma$fitted.values[index.1], col="brown",lwd=2)
abline(v=small.pred.mod.gamma$fitted.values[index.1], col="orange",lwd=2)
legend("topright", legend=c("BP2", "BP3", "M1", "M2", "M3", "M4"),
       col=c("magenta","red", "blue", "green","brown","orange"),lty=c(2,6,1,1,1,1),lwd=2,cex=1.2)
dev.copy(pdf,'~/figures/Models/Eval/Participant1.pdf') 
dev.off()



#############################################################################
# Participant b)

index.4 <-882

# systolic blood pressure HUNT3
obs.hunt3.4<- df.total$SystolicBP3[index.4]
obs.hunt3.4

# systolic blood pressure HUNT2
obs.hunt2.4<- df.total$SystolicBP2[index.4]
obs.hunt2.4

# predicted systolic pressure HUNT3, Full Gaussian model
pred.full.4 <- full.pred.mod$fitted.values[index.4]
pred.full.4

# predicted systolic pressure HUNT3, Small Gaussian model
pred.small.4 <- small.pred.mod$fitted.values[index.4]
pred.small.4

# predicted systolic pressure HUNT3, Full gamma model
pred.full.gamma.4 <- full.pred.mod.gamma$fitted.values[index.4]
pred.full.gamma.4

# predicted systolic pressure HUNT3, Small gamma model
pred.small.gamma.4 <- small.pred.mod.gamma$fitted.values[index.4]
pred.small.gamma.4

# Residual
resid.4 <- obs.hunt3.4-pred.full.4
resid.4

### Health status
df.total$BPMed3[index.4]
df.total$CVD3[index.4]
df.total$Diabetes3[index.4]


# Plot prediction distribution
par(mar=c(6.1, 5.1, 4.1, 2.1))
plot(c(50:220),dnorm(c(50:220), mean=full.pred.mod$fitted.values[index.4], sd=full.sd.y[index.4]), type="l", col="blue",
     ylab="Probability", xlab="Systolic blood pressure [mmHg]", ylim=c(0,0.035), cex.main=1.7, cex.lab=1.8, cex.axis=1.7)
lines(c(50:220),dnorm(c(50:220), mean=small.pred.mod$fitted.values[index.4], sd=small.sd.y[index.4]), col="green")
lines(c(50:220),dgamma(c(50:220),shape=full.gamma.shape,rate=full.gamma.rate[index.4]), col="brown")
lines(c(50:220),dgamma(c(50:220),shape=small.gamma.shape,rate=small.gamma.rate[index.4]), col="orange")
abline(v=df.total$SystolicBP2[index.4], col="magenta", lty=2, lwd=2)
abline(v=df.total$SystolicBP3[index.4], col="red", lty=6,lwd=2)
abline(v=full.pred.mod$fitted.values[index.4], col="blue", lwd=2)
abline(v=small.pred.mod$fitted.values[index.4], col="green", lwd=2)
abline(v=full.pred.mod.gamma$fitted.values[index.4], col="brown", lwd=2)
abline(v=small.pred.mod.gamma$fitted.values[index.4], col="orange", lwd=2)
legend("topright", legend=c("BP2", "BP3", "M1", "M2", "M3", "M4"),
       col=c("magenta","red", "blue", "green","brown","orange"),lty=c(2,6,1,1,1,1),lwd=2,cex=1.2)
dev.copy(pdf,'~/figures/Models/Eval/Participant4.pdf') 
dev.off()

