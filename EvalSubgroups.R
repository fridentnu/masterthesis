

source("R code/Evaluation.R")


#######################################################################
# Example 5, CVD

# Fitted values

full.cvd <-ggplot(data=data.frame(full.pred.mod$fitted.values[df.total$CVD3]))+
  geom_histogram(aes(full.pred.mod$fitted.values[df.total$CVD3]),binwidth = 1)+
  coord_cartesian(xlim=c(70,200),ylim=c(0,40))+
  xlab("Fitted values, CVD")


res.cvd <-ggplot(data=data.frame(df.total.sc$SystolicBP3[df.total$CVD3]))+
  geom_histogram(aes(df.total.sc$SystolicBP3[df.total$CVD3]), binwidth = 1)+
  coord_cartesian(xlim=c(70,200),ylim=c(0,40))+
  xlab("Observed systolic blood pressure HUNT3, CVD")

res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(70,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure HUNT3")

grid.arrange(res, res.cvd, full.cvd, nrow=3)


# Residuals
resid.cvd.full <- df.total$SystolicBP3[df.total$CVD3]- full.pred.mod$fitted.values[df.total$CVD3]


mean(resid.cvd.full)
mean(full.pred.mod$residuals)
sd(resid.cvd.full)
sd(full.pred.mod$residuals)

# QQ-plot
qqnorm(resid.cvd.full, main="Normal QQ-plot of CVD residuals", ylab="Residuals from full model")
qqline(resid.cvd.full, col = "steelblue", lwd = 2)



### Prediction distribution 
prob.hyp.full.cvd <- prob.hyp.full.pred[df.total$CVD3]

### Evaluation methods

# RMSE
full.rmse.cvd <- sqrt(mean((df.total$SystolicBP3[df.total$CVD3]-full.pred.mod$fitted.values[df.total$CVD3])**2))
round(full.rmse.cvd,3)
# larger variance in residuals

# Brier
full.brier.cvd <- round(BrierScore(resp=df.total.sc$SystolicHyp[df.total$CVD3], pred=prob.hyp.full.cvd),5)
full.brier.cvd
# larger brierscore

# CRPS

full.crps.cvd <- crps(y=df.total.sc$SystolicBP3[df.total$CVD3],family="normal",
                      mean=full.pred.mod$fitted.values[df.total$CVD3], sd=full.sd.y[df.total$CVD3])
round(mean(full.crps.cvd),3)
#larger crps

# PIT
prob.obs.full.cvd <- pnorm(df.total$SystolicBP3[df.total$CVD3], 
                           mean=full.pred.mod$fitted.values[df.total$CVD3], sd=full.sd.y[df.total$CVD3])
hist(prob.obs.full.cvd)

# Sensitivity
# Hypertensive and predicted hypertensive
full.sensitivy.cvd <- round(100*sum(full.pred.mod$fitted.values[df.total$CVD3]>=140 & df.total$SystolicHyp[df.total$CVD3])/sum(df.total$SystolicHyp[df.total$CVD3]),3)
# better sensitivity

# Specificity
# Not hypertensive and not predicted hypertensive
full.specificity.cvd <- round(100*sum(full.pred.mod$fitted.values[df.total$CVD3]<140 & !df.total$SystolicHyp[df.total$CVD3])/sum(!df.total$SystolicHyp[df.total$CVD3]),3)
# worse specificity



# C-stat
cstat.cvd<-round(cstat.func(df.total[df.total$CVD3==TRUE,],prob.hyp.full.pred[df.total$CVD3==TRUE]),5)
cstat.cvd

# Export dataframe with results

df.eval.cvd <- data.frame("RMSE"=round(full.rmse.cvd,3), "BrierScore"=full.brier.cvd, "CRPS"=round(mean(full.crps.cvd),3),
                               "Sensitivity"=full.sensitivy.cvd, "Specificity"=full.specificity.cvd, 
                               "C-statistic"=cstat.cvd)
write.csv(df.eval.cvd,"Tables/Eval/CVD.csv", row.names = FALSE)


# Example 6, Diabetes ########################################################################

# Fitted values

full.diabetes <-ggplot(data=data.frame(full.pred.mod$fitted.values[df.total$Diabetes3]))+
  geom_histogram(aes(full.pred.mod$fitted.values[df.total$Diabetes3]),binwidth = 1)+
  coord_cartesian(xlim=c(70,200),ylim=c(0,40))+
  xlab("Fitted values, CVD")


res.diabetes <-ggplot(data=data.frame(df.total.sc$SystolicBP3[df.total$Diabetes3]))+
  geom_histogram(aes(df.total.sc$SystolicBP3[df.total$Diabetes3]), binwidth = 1)+
  coord_cartesian(xlim=c(70,200),ylim=c(0,40))+
  xlab("Observed systolic blood pressure HUNT3, CVD")

res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(70,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure HUNT3")

grid.arrange(res, res.diabetes, full.diabetes, nrow=3)

# Residuals
resid.diabetes.full <- df.total$SystolicBP3[df.total$Diabetes3]- full.pred.mod$fitted.values[df.total$Diabetes3]


mean(resid.diabetes.full)
mean(full.pred.mod$residuals)

sd(resid.diabetes.full)
sd(full.pred.mod$residuals)


# QQ-plot
qqnorm(resid.diabetes.full, main="Normal QQ-plot of CVD residuals", ylab="Residuals from full model")
qqline(resid.diabetes.full, col = "steelblue", lwd = 2)



### Prediction distribution 
prob.hyp.full.diabetes <- prob.hyp.full.pred[df.total$Diabetes3]

### Evaluation methods

# RMSE
full.rmse.diabetes <- sqrt(mean((df.total$SystolicBP3[df.total$Diabetes3]-full.pred.mod$fitted.values[df.total$Diabetes3])**2))
round(full.rmse.diabetes,3)
# larger variance in residuals

# Brier
full.brier.diabetes<- round(BrierScore(resp=df.total.sc$SystolicHyp[df.total$Diabetes3], pred=prob.hyp.full.diabetes),5)
# larger brierscore

# CRPS
full.crps.diabetes <- crps(y=df.total.sc$SystolicBP3[df.total$Diabetes3],family="normal",
                      mean=full.pred.mod$fitted.values[df.total$Diabetes3], sd=full.sd.y[df.total$Diabetes3])
round(mean(full.crps.diabetes),3)
#larger crps

# PIT
prob.obs.full.diabetes <- pnorm(df.total$SystolicBP3[df.total$Diabetes3], 
                           mean=full.pred.mod$fitted.values[df.total$Diabetes3], sd=full.sd.y[df.total$Diabetes3])
hist(prob.obs.full.diabetes)
# heavy right tail

# Sensitivity
# Hypertensive and predicted hypertensive
full.sensitivy.diabetes <- round(100*sum(full.pred.mod$fitted.values[df.total$Diabetes3]>=140 & df.total$SystolicHyp[df.total$Diabetes3])/sum(df.total$SystolicHyp[df.total$Diabetes3]),3)
# better sensitivity

# Specificity
# Not hypertensive and not predicted hypertensive
full.specificity.diabetes<-round(100*sum(full.pred.mod$fitted.values[df.total$Diabetes3]<140 & !df.total$SystolicHyp[df.total$Diabetes3])/sum(!df.total$SystolicHyp[df.total$Diabetes3]),3)
# wors specificity

# C-stat
cstat.diabetes <-round(cstat.func(df.total[df.total$Diabetes3==TRUE,],prob.hyp.full.pred[df.total$Diabetes3==TRUE]),5)
cstat.diabetes

df.eval.diabetes <- data.frame("RMSE"=round(full.rmse.diabetes,3), "BrierScore"=full.brier.diabetes, "CRPS"=round(mean(full.crps.diabetes),3),
                          "Sensitivity"=full.sensitivy.diabetes, "Specificity"=full.specificity.diabetes, 
                          "C-statistic"=cstat.diabetes)
write.csv(df.eval.diabetes,"Tables/Eval/Diabetes.csv", row.names = FALSE)

# Example 7, BP Med ##############################################################################


# Fitted values

full.bpmed <-ggplot(data=data.frame(full.pred.mod$fitted.values[df.total$BPMed3]))+
  geom_histogram(aes(full.pred.mod$fitted.values[df.total$BPMed3]),binwidth = 1)+
  coord_cartesian(xlim=c(70,200),ylim=c(0,70))+
  xlab("Fitted values, CVD")


res.bpmed <-ggplot(data=data.frame(df.total.sc$SystolicBP3[df.total$BPMed3]))+
  geom_histogram(aes(df.total.sc$SystolicBP3[df.total$BPMed3]), binwidth = 1)+
  coord_cartesian(xlim=c(70,200),ylim=c(0,70))+
  xlab("Observed systolic blood pressure HUNT3, CVD")

res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(70,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure HUNT3")

grid.arrange(res, res.bpmed, full.bpmed, nrow=3)

# Residuals
resid.bpmed.full <- df.total$SystolicBP3[df.total$BPMed3]- full.pred.mod$fitted.values[df.total$BPMed3]


mean(resid.bpmed.full)
mean(full.pred.mod$residuals)

sd(resid.bpmed.full)
sd(full.pred.mod$residuals)


# QQ-plot
qqnorm(resid.bpmed.full, main="Normal QQ-plot of CVD residuals", ylab="Residuals from full model")
qqline(resid.bpmed.full, col = "steelblue", lwd = 2)



### Prediction distribution 
prob.hyp.full.bpmed <- prob.hyp.full.pred[df.total$BPMed3]

### Evaluation methods

# RMSE
full.rmse.bpmed <- sqrt(mean((df.total$SystolicBP3[df.total$BPMed3]-full.pred.mod$fitted.values[df.total$BPMed3])**2))
round(full.rmse.bpmed,3)
# larger variance in residuals

# Brier
full.brier.bpmed<- round(BrierScore(resp=df.total.sc$SystolicHyp[df.total$BPMed3], pred=prob.hyp.full.bpmed),5)
# larger brierscore

# CRPS
full.crps.bpmed <- crps(y=df.total.sc$SystolicBP3[df.total$BPMed3],family="normal",
                           mean=full.pred.mod$fitted.values[df.total$BPMed3], sd=full.sd.y[df.total$BPMed3])
round(mean(full.crps.bpmed),3)
#larger crps

# PIT
prob.obs.full.bpmed <- pnorm(df.total$SystolicBP3[df.total$BPMed3], 
                                mean=full.pred.mod$fitted.values[df.total$BPMed3], sd=full.sd.y[df.total$BPMed3])
hist(prob.obs.full.bpmed)
# heavy right tail

# Sensitivity
# Hypertensive and predicted hypertensive
full.sensitivy.bpmed<- round(100*sum(full.pred.mod$fitted.values[df.total$BPMed3]>=140 & df.total$SystolicHyp[df.total$BPMed3])/sum(df.total$SystolicHyp[df.total$BPMed3]),3)
# better sensitivity

# Specificity
# Not hypertensive and not predicted hypertensive
full.specificity.bpmed<- round(100*sum(full.pred.mod$fitted.values[df.total$BPMed3]<140 & !df.total$SystolicHyp[df.total$BPMed3])/sum(!df.total$SystolicHyp[df.total$BPMed3]),3)
# worse specificity


# C-stat
cstat.bpmed <-round(cstat.func(df.total[df.total$BPMed3==TRUE,],prob.hyp.full.pred[df.total$BPMed3==TRUE]),5)
cstat.bpmed



df.eval.bpmed <- data.frame("RMSE"=round(full.rmse.bpmed,3), "BrierScore"=full.brier.bpmed, "CRPS"=round(mean(full.crps.bpmed),3),
                               "Sensitivity"=full.sensitivy.bpmed, "Specificity"=full.specificity.bpmed, 
                               "C-statistic"=cstat.bpmed)
write.csv(df.eval.bpmed,"Tables/Eval/BPMed.csv", row.names = FALSE)


##################### Compare corrected and uncorrected

resid.bpmed.uncorr<- (df.total$SystolicBP3[df.total$BPMed3]-15)- full.pred.mod$fitted.values[df.total$BPMed3]
resid.bpmed.corr<- (df.total$SystolicBP3[df.total$BPMed3])- full.pred.mod$fitted.values[df.total$BPMed3]

# HUNT2
mean(df.total$SystolicBP2[df.total$BPMed3])

# Corrected HUNT3
mean(df.total$SystolicBP3[df.total$BPMed3])

# Uncorrected HUNT3
mean(df.total$SystolicBP3[df.total$BPMed3]-15)



mean(resid.bpmed.corr)
mean(resid.bpmed.uncorr)

mean(full.pred.mod$residuals)
# does not predict high enough blood pressure. Maybe shouldn't have such big effect of blood pressure?

sd(resid.bpmed.corr)
sd(full.pred.mod$residuals)

# definitely the biggest difference for residuals, mean residual was 12

