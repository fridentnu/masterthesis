


source("R code/Evaluation.R")


hist(df.total$SystolicBP3)
df.order.obs <- df.total
df.order.obs <- df.order.obs[order(df.total$SystolicBP3),]

#############################################################################
# Example 1, participant #20, BP3 88, diabetes, not cvd or bpmed


obs.1 <- df.order.obs$SystolicBP3[15400]
obs.1

pid.1 <- df.order.obs$PID[15400]
pid.1

index.1 <- match(pid.1, df.total$PID)
index.1

obs.hunt2.1<- df.total$SystolicBP2[index.1]
obs.hunt2.1

pred.full.1 <- full.pred.mod$fitted.values[index.1]
pred.full.1

pred.small.1 <- small.pred.mod$fitted.values[index.1]
pred.small.1

pred.full.gamma.1 <- full.pred.mod.gamma$fitted.values[index.1]
pred.full.gamma.1

pred.small.gamma.1 <- small.pred.mod.gamma$fitted.values[index.1]
pred.small.gamma.1


start.1 <- df.total$SystolicBP2[index.1]
start.1

resid.1 <- obs.1-pred.full.1
resid.1


#### Health status
# Not on medicine
df.total$BPMed3[index.1]
df.total$CVD3[index.1]
df.total$Diabetes3[index.1]

#### Predicted probability of hypertension
# full model
prob.hyp.full.pred[index.1]

# small model
prob.hyp.small.pred[index.1]

# full gamma
prob.hyp.full.gamma.pred[index.1]

# small gamma
prob.hyp.small.gamma.pred[index.1]

# framingham
fram.risk.ad.age[index.1]
fram.risk[index.1]


# Plot prediction distribion
plot(c(50:220),dnorm(c(50:220), mean=full.pred.mod$fitted.values[index.1], sd=full.sd.y[index.1]),
     ylab="Probability", xlab="Systolic blood pressure")
abline(v=df.total$SystolicBP2[index.1], col="green")
abline(v=df.total$SystolicBP3[index.1], col="red")
abline(v=full.pred.mod$fitted.values[index.1], col="blue")
legend("topright", legend=c("BP 2", "BP 3", "Pred 3"),
       col=c("green","red", "blue"),lty=1)

par(mar=c(6.1, 5.1, 4.1, 2.1))
plot(c(50:220),dnorm(c(50:220), mean=full.pred.mod$fitted.values[index.1], sd=full.sd.y[index.1]), type="l", col="blue",
     ylab="Probability", xlab="Systolic blood pressure [mmHg]",ylim=c(0,0.035),cex.main=1.7, cex.lab=1.8, cex.axis=1.7)
lines(c(50:220),dnorm(c(50:220), mean=small.pred.mod$fitted.values[index.1], sd=small.sd.y[index.1]), col="green")
lines(c(50:220),dgamma(c(50:220),shape=full.gamma.shape,rate=full.gamma.rate[index.1]), col="brown")
lines(c(50:220),dgamma(c(50:220),shape=small.gamma.shape,rate=small.gamma.rate[index.1]), col="orange")
abline(v=df.total$SystolicBP2[index.1], col="magenta")
abline(v=df.total$SystolicBP3[index.1], col="red")
abline(v=full.pred.mod$fitted.values[index.1], col="blue")
abline(v=small.pred.mod$fitted.values[index.1], col="green")
abline(v=full.pred.mod.gamma$fitted.values[index.1], col="brown")
abline(v=small.pred.mod.gamma$fitted.values[index.1], col="orange")
legend("topright", legend=c("BP2", "BP3", "M1", "M2", "M3", "M4"),
       col=c("magenta","red", "blue", "green","brown","orange"),lty=1, cex=1.2)
dev.copy(pdf,'~/figures/Models/Eval/Participant1.pdf') # Save the plot
dev.off()

### Comment:
# for index 1 (20) BP2= 135 , BP3=88, (not on bpmed), full and small gamma eqivalent, same for full and small gamma, and all have same fitted values
# for index 1 (1000), BP2=123,BP3=105, all predictions very similar
# for index 1 (10000), BP2=120, BP3=128, gaussian and gamma quite similar, gamma has a very slightly heavier right tail identical fitted value
# for index 1 (16000), BP2= 127, BP3= 151, 
# for index 1 (17200), BP2= 132, BP3=174, very similar, gamma very slightly heavier right tail 
# generally very similar but gamma has very slightly heavier right tail 
# fitted values generally slightly higher than BP2



### New model, where prediction is just adding 6 to systolic bp
mean(df.total$SystolicBP3)-mean(df.total$SystolicBP2)
add.six.pred <- df.total$SystolicBP2+4.016
round(sqrt(mean((df.total$SystolicBP3-add.six.pred)**2)),3)
# our model just perform slightly better than just adding the difference between means to systolicbp2


num.decrease.pred<- rep(0, length(df.total$PID))
num.decrease.obs <- rep(0, length(df.total$PID))
for(i in 1:length(df.total$PID)){
  if(df.total$SystolicBP3[i]<df.total$SystolicBP2[i]){
    num.decrease.obs[i]<-1
    if(full.pred.mod$fitted.values[i]<df.total$SystolicBP2){
      num.decrease.pred[i]=1
    }
  }
}
sum(num.decrease.obs)
sum(num.decrease.pred)
sum(num.decrease.pred)/sum(num.decrease.obs)
## Not able to spot decrease very well, but it is not as dumb as just adding 4 either
#######################################################################################

# Example 2, participant #10 000, 128, not on BP med, CVD or diabetes
df.order.obs$SystolicBP3[10000]


obs.2 <- df.order.obs$SystolicBP3[10000]
obs.2

pid.2 <- df.order.obs$PID[10000]
pid.2

index.2 <- match(pid.2, df.total$PID)
index.2

pred.full.2 <- full.pred.mod$fitted.values[index.2]
pred.full.2


start.2 <- df.total$SystolicBP2[index.2]
start.2

resid.2 <- obs.2-pred.full.2
resid.2

# Not on medicine
df.total$BPMed3[index.2]
df.total$CVD3[index.2]
df.total$Diabetes3[index.2]


prob.hyp.full.pred[index.2]

# Plot prediction distribion
plot(c(50:220),dnorm(c(50:220), mean=full.pred.mod$fitted.values[index.2], sd=full.sd.y[index.2]),
     ylab="Probability", xlab="Systolic blood pressure")
abline(v=df.total$SystolicBP2[index.2], col="green")
abline(v=df.total$SystolicBP3[index.2], col="red")
abline(v=full.pred.mod$fitted.values[index.2], col="blue")
legend("topright", legend=c("BP 2", "BP 3", "Pred 3"),
       col=c("green","red", "blue"),lty=1)

plot(c(50:220),dnorm(c(50:220), mean=full.pred.mod$fitted.values[index.2], sd=full.sd.y[index.2]), type="l", col="blue",
     ylab="Probability", xlab="Systolic blood pressure [mmHg]")
lines(c(50:220),dnorm(c(50:220), mean=small.pred.mod$fitted.values[index.2], sd=small.sd.y[index.2]), col="green")
lines(c(50:220),dgamma(c(50:220),shape=full.gamma.shape,rate=full.gamma.rate[index.2]), col="brown")
lines(c(50:220),dgamma(c(50:220),shape=small.gamma.shape,rate=small.gamma.rate[index.2]), col="orange")
abline(v=df.total$SystolicBP2[index.2], col="magenta")
abline(v=df.total$SystolicBP3[index.2], col="red")
abline(v=full.pred.mod$fitted.values[index.2], col="blue")
abline(v=small.pred.mod$fitted.values[index.2], col="green")
abline(v=full.pred.mod.gamma$fitted.values[index.2], col="brown")
abline(v=small.pred.mod.gamma$fitted.values[index.2], col="orange")
legend("topright", legend=c("BP 2", "BP 3", "M1", "M2", "M3", "M4"),
       col=c("magenta","red", "blue", "green","brown","orange"),lty=1)
dev.copy(pdf,'~/figures/Models/Eval/Participant2.pdf') # Save the plot
dev.off()

# Example 3, participant # 16 000, BP 151, not cvd or diabetes or bpmed
df.order.obs$SystolicBP3[16000]


obs.3 <- df.order.obs$SystolicBP3[16000]
obs.3

pid.3 <- df.order.obs$PID[16000]
pid.3

index.3 <- match(pid.3, df.total$PID)
index.3

pred.full.3 <- full.pred.mod$fitted.values[index.3]
pred.full.3


start.3 <- df.total$SystolicBP2[index.3]
start.3

resid.3 <- obs.2-pred.full.3
resid.3

# Not on medicine
df.total$BPMed3[index.3]
df.total$CVD3[index.3]
df.total$Diabetes3[index.3]


prob.hyp.full.pred[index.3]

# Plot prediction distribion
plot(c(50:220),dnorm(c(50:220), mean=full.pred.mod$fitted.values[index.3], sd=full.sd.y[index.3]),
     ylab="Probability", xlab="Systolic blood pressure")
abline(v=df.total$SystolicBP2[index.3], col="green")
abline(v=df.total$SystolicBP3[index.3], col="red")
abline(v=full.pred.mod$fitted.values[index.3], col="blue")
legend("topright", legend=c("BP 2", "BP 3", "Pred 3"),
       col=c("green","red", "blue"),lty=1)

plot(c(50:220),dnorm(c(50:220), mean=full.pred.mod$fitted.values[index.3], sd=full.sd.y[index.3]), type="l", col="blue",
     ylab="Probability", xlab="Systolic blood pressure [mmHg]")
lines(c(50:220),dnorm(c(50:220), mean=small.pred.mod$fitted.values[index.3], sd=small.sd.y[index.3]), col="green")
lines(c(50:220),dgamma(c(50:220),shape=full.gamma.shape,rate=full.gamma.rate[index.3]), col="brown")
lines(c(50:220),dgamma(c(50:220),shape=small.gamma.shape,rate=small.gamma.rate[index.3]), col="orange")
abline(v=df.total$SystolicBP2[index.3], col="magenta")
abline(v=df.total$SystolicBP3[index.3], col="red")
abline(v=full.pred.mod$fitted.values[index.3], col="blue")
abline(v=small.pred.mod$fitted.values[index.3], col="green")
abline(v=full.pred.mod.gamma$fitted.values[index.3], col="brown")
abline(v=small.pred.mod.gamma$fitted.values[index.3], col="orange")
legend("topright", legend=c("BP 2", "BP 3", "M1", "M2", "M3", "M4"),
       col=c("magenta","red", "blue", "green","brown","orange"),lty=1)
dev.copy(pdf,'~/figures/Models/Eval/Participant3.pdf') # Save the plot
dev.off()


#############################################################################3
# Example 4, participant # 17 200, BP 174, healthy
df.order.obs$SystolicBP3[800]

obs.4 <- df.order.obs$SystolicBP3[800]
obs.4

pid.4 <- df.order.obs$PID[800]
pid.4

index.4 <- match(pid.4, df.total$PID)
index.4



index.4 <-882

obs.hunt3.4<- df.total$SystolicBP3[index.4]
obs.hunt3.4

obs.hunt2.4<- df.total$SystolicBP2[index.4]
obs.hunt2.4

pred.full.4 <- full.pred.mod$fitted.values[index.4]
pred.full.4


pred.small.4 <- small.pred.mod$fitted.values[index.4]
pred.small.4

pred.full.gamma.4 <- full.pred.mod.gamma$fitted.values[index.4]
pred.full.gamma.4

pred.small.gamma.4 <- small.pred.mod.gamma$fitted.values[index.4]
pred.small.gamma.4


start.4 <- df.total$SystolicBP2[index.4]
start.4

resid.4 <- obs.4-pred.full.4
resid.4

# Not on medicine
df.total$BPMed3[index.4]
df.total$CVD3[index.4]
df.total$Diabetes3[index.4]


prob.hyp.full.pred[index.4]

par(mar=c(6.1, 5.1, 4.1, 2.1))
plot(c(50:220),dnorm(c(50:220), mean=full.pred.mod$fitted.values[index.4], sd=full.sd.y[index.4]), type="l", col="blue",
     ylab="Probability", xlab="Systolic blood pressure [mmHg]", ylim=c(0,0.035), cex.main=1.7, cex.lab=1.8, cex.axis=1.7)
lines(c(50:220),dnorm(c(50:220), mean=small.pred.mod$fitted.values[index.4], sd=small.sd.y[index.4]), col="green")
lines(c(50:220),dgamma(c(50:220),shape=full.gamma.shape,rate=full.gamma.rate[index.4]), col="brown")
lines(c(50:220),dgamma(c(50:220),shape=small.gamma.shape,rate=small.gamma.rate[index.4]), col="orange")
abline(v=df.total$SystolicBP2[index.4], col="magenta")
abline(v=df.total$SystolicBP3[index.4], col="red")
abline(v=full.pred.mod$fitted.values[index.4], col="blue")
abline(v=small.pred.mod$fitted.values[index.4], col="green")
abline(v=full.pred.mod.gamma$fitted.values[index.4], col="brown")
abline(v=small.pred.mod.gamma$fitted.values[index.4], col="orange")
legend("topright", legend=c("BP2", "BP3", "M1", "M2", "M3", "M4"),
       col=c("magenta","red", "blue", "green","brown","orange"),lty=1,cex=1.2)
dev.copy(pdf,'~/figures/Models/Eval/Participant4.pdf') # Save the plot
dev.off()

