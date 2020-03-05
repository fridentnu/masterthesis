


source("R code/Evaluation.R")


hist(df.total$SystolicBP3)
df.order.obs <- df.total
df.order.obs <- df.order.obs[order(df.total$SystolicBP3),]

#############################################################################
# Example 1, participant #20, BP 88, diabetes, not cvd or bpmed


obs.1 <- df.order.obs$SystolicBP3[20]
obs.1

pid.1 <- df.order.obs$PID[20]
pid.1

index.1 <- match(pid.1, df.total$PID)
index.1

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

#############################################################################3
# Example 4, participant # 17 200, BP 174, healthy
df.order.obs$SystolicBP3[17200]

obs.4 <- df.order.obs$SystolicBP3[17200]
obs.4

pid.4 <- df.order.obs$PID[17200]
pid.4

index.4 <- match(pid.4, df.total$PID)
index.4

pred.full.4 <- full.pred.mod$fitted.values[index.4]
pred.full.4


start.4 <- df.total$SystolicBP2[index.4]
start.4

resid.4 <- obs.4-pred.full.4
resid.4

# Not on medicine
df.total$BPMed3[index.4]
df.total$CVD3[index.4]
df.total$Diabetes3[index.4]


prob.hyp.full.pred[index.4]

# Plot prediction distribion
plot(c(50:220),dnorm(c(50:220), mean=full.pred.mod$fitted.values[index.4], sd=full.sd.y[index.4]),
     ylab="Probability", xlab="Systolic blood pressure")
abline(v=df.total$SystolicBP2[index.4], col="green")
abline(v=df.total$SystolicBP3[index.4], col="red")
abline(v=full.pred.mod$fitted.values[index.3], col="blue")
legend("topright", legend=c("BP 2", "BP 3", "Pred 3"),
       col=c("green","red", "blue"),lty=1)

#######################################################################3
# Example 5, CVD


resid.cvd.full <- df.total$SystolicBP3[df.total$CVD3]- full.pred.mod$fitted.values[df.total$CVD3]

sd(resid.cvd.full)
sd(full.pred.mod$residuals)
mean(resid.cvd.full)
mean(full.pred.mod$residuals)

sd(resid.cvd.full)
sd(full.pred.mod$residuals)

hist(resid.cvd.full)
hist(full.pred.mod$residuals)

# Framingham, percentage of hypertensive people with cvd who have more than 50% framingham risk
fram.ad.age.accu<- sum(fram.risk.ad.age>0.5&df.total$SystolicHyp)/sum(df.total$SystolicHyp)
sum(fram.risk.ad.age>0.5&df.total$SystolicHyp&df.total$CVD3)/sum(df.total$SystolicHyp[df.total$CVD3])




# Example 6, Diabetes
resid.diabetes <- df.total$SystolicBP3[df.total$Diabetes3]- full.pred.mod$fitted.values[df.total$Diabetes3]


mean(resid.diabetes)
mean(full.pred.mod$residuals)

sd(resid.diabetes)
sd(full.pred.mod$residuals)

hist(resid.diabetes)
hist(full.pred.mod$residuals)

# Framingham, percentage of hypertensive people with diabetes who have more than 50% framingham risk
sum(fram.ad.age.accu[df.total$Diabetes3])/sum(df.total$SystolicHyp[df.total$Diabetes3])

# Example 7, BP Med

resid.bpmed.uncorr<- (df.total$SystolicBP3[df.total$BPMed3]-15)- full.pred.mod$fitted.values[df.total$BPMed3]
resid.bpmed.corr<- (df.total$SystolicBP3[df.total$BPMed3])- full.pred.mod$fitted.values[df.total$BPMed3]


mean(df.total$SystolicBP2[df.total$BPMed3])
mean(df.total$SystolicBP3[df.total$BPMed3])


mean(resid.bpmed.corr)
mean(resid.bpmed.uncorr)

mean(full.pred.mod$residuals)
# does not predict high enough blood pressure. Maybe shouldn't have such big effect of blood pressure?

sd(resid.bpmed.corr)
sd(full.pred.mod$residuals)

hist(resid.bpmed.corr)
hist(full.pred.mod$residuals)

# Framingham, percentage of hypertensive people on bpmed who have more than 50% framingham risk
sum(fram.risk.ad.age>0.5 & df.total$SystolicHyp & df.total$BPMed3)/sum(df.total$SystolicHyp[df.total$BPMed3])

# definitely the biggest difference for residuals, mean residual was 12

