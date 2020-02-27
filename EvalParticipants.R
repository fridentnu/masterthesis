


source("R code/Evaluation.R")


hist(df.total$SystolicBP3)

#df.order.obs <- data.frame("DF"=df.total, "Full"=full.pred.mod)
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
#prob.hyp.ful.gamma.pred[index.1]

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


# Example 6, Diabetes
