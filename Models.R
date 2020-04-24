##### LOAD PACKAGES #######
library(tidyverse) # ggplot2, dplyr etc.
library(DataExplorer) # EDA package
library(gridExtra) # arrange multiple plots in one figure
library(grid) # text size in grid.arrange

load("MyData/EDA.df.total.RData")

############################## STANDARDIZATION ###############################################
### Standardize the continuous variables to make it easier to compare effects
## subtract the mean and divide by the standard deviance

introduce(df.total)

df.total.sc <- df.total
df.total.sc[,c(2,4,5,6,13,14,15,17)] <- scale(df.total.sc[,c(2,4,5,6,13,14,15,17)])

plot_histogram(df.total.sc)


################################# FIT MODELS ###########################################

full.pred.mod <- glm(SystolicBP3 ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholesterol2 + HDLCholesterol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, family= gaussian(link="identity"), data=df.total.sc)

full.pred.mod.gamma <- glm(SystolicBP3 ~ BirthYear + Sex + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2 + RecPA2 + BPHigPar2 + Smoking2 + Cholesterol2 + HDLCholesterol2 +
                      Glucose2 + GFR2 + Creatinine2 + Education2, data=df.total.sc, family=Gamma(link = "identity"))


small.pred.mod <- glm(SystolicBP3 ~ BirthYear + BMI2 + SystolicBP2 + DiastolicBP2 + 
                      PAI2+ BPHigPar2 + HDLCholesterol2 + Education2, family=gaussian(link="identity"),data=df.total.sc)

small.pred.mod.gamma <- glm(SystolicBP3 ~ BirthYear + BMI2 + SystolicBP2 + DiastolicBP2 + 
                       PAI2+ BPHigPar2 + HDLCholesterol2 + Education2, data=df.total.sc, family=Gamma(link = "identity"))


################################ SUMMARY #######################################
summary(full.pred.mod)
summary(full.pred.mod.gamma)
summary(small.pred.mod)
summary(small.pred.mod.gamma)


################################ ESTIMATE PREDICTION DISTRIBUTION PARAMETERS ##################

### FULL GAUSSIAN MODEL
# Variance and standard deviation for each participants response
full.design.mat <- model.matrix(full.pred.mod)

full.var.coeff <- var(full.pred.mod$residuals)*solve(t(full.design.mat)%*%full.design.mat)
full.exp.var <- rep(0,length(df.total$PID))

for(i in 1:length(df.total$PID)){
  full.exp.var[i] <- t(full.design.mat[i,])%*%full.var.coeff%*%full.design.mat[i,]
}

full.var.y <- full.exp.var + sum((residuals(full.pred.mod)-mean(residuals(full.pred.mod)))**2)/(length(df.total$PID)-1)
full.sd.y<- sqrt(full.var.y)



### SMALL GAUSSIAN MODEL
# Variance and standard deviation for each participants response
small.design.mat <- model.matrix(small.pred.mod)

small.var.coeff <- var(small.pred.mod$residuals)*solve(t(small.design.mat)%*%small.design.mat)
small.exp.var <- rep(0,length(df.total$PID))

for(i in 1:length(df.total$PID)){
  small.exp.var[i] <- t(small.design.mat[i,])%*%small.var.coeff%*%small.design.mat[i,]
}

small.var.y <- small.exp.var + sum((residuals(small.pred.mod)-mean(residuals(small.pred.mod)))**2)/(length(df.total$PID)-1)
small.sd.y<- sqrt(small.var.y)

### FULL GAMMA MODEL
# shape and rate for each participants response
full.gamma.shape <- 1/summary(full.pred.mod.gamma)$dispersion
full.gamma.shape
full.gamma.rate <- full.gamma.shape/full.pred.mod.gamma$fitted.values


###### SMALL GAMMA MODEL
small.gamma.shape <- 1/summary(small.pred.mod.gamma)$dispersion
small.gamma.shape
small.gamma.rate <- small.gamma.shape/small.pred.mod.gamma$fitted.values


################################# PLOT OBSERVED AND PREDICTED SYSTOLIC BP ######################################

# Predicted BP, small Gaussian model
small <-ggplot(data=small.pred.mod)+
  geom_histogram(aes(small.pred.mod$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Predicted BP, small Gaussian model")+
  ylab(" ")

# Predicted BP, full Gaussian model
full <-ggplot(data=full.pred.mod)+
  geom_histogram(aes(full.pred.mod$fitted.values),binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Predicted BP, full Gaussian model")+
  ylab(" ")

# Predicted BP, small gamma model
small.gamma <-ggplot(data=small.pred.mod.gamma)+
  geom_histogram(aes(small.pred.mod.gamma$fitted.values), binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Predicted BP, small gamma model")+
  ylab(" ")

# Predicted BP, full gamma model
full.gamma <-ggplot(data=full.pred.mod.gamma)+
  geom_histogram(aes(full.pred.mod.gamma$fitted.values),binwidth = 1)+
  coord_cartesian(xlim=c(0,200), ylim=c(0,800))+
  xlab("Predicted BP, full gamma model")+
  ylab(" ")

# Observed systolic blood pressure HUNT3
res <-ggplot(data=df.total.sc)+
  geom_histogram(aes(df.total.sc$SystolicBP3), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure HUNT3")+
  ylab(" ")

# Observed systolic blood pressure HUNT2
const <- ggplot(data=data.frame(df.total$SystolicBP2))+
  geom_histogram(aes(df.total$SystolicBP2), binwidth = 1)+
  coord_cartesian(xlim=c(0,200),ylim=c(0,800))+
  xlab("Observed systolic blood pressure HUNT2")+
  ylab(" ")

grid.arrange(const,res, small, full, small.gamma, full.gamma, nrow=3, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/Models/Present/FittedValues.pdf') 
dev.off()

# Check means
mean(df.total.sc$SystolicBP3)
mean(df.total$SystolicBP2)
mean(full.pred.mod$fitted.values)
mean(small.pred.mod$fitted.values)
mean(full.pred.mod.gamma$fitted.values)
mean(small.pred.mod.gamma$fitted.values)

##################################### Regression coefficients ###############################
df.coef.full <- summary(full.pred.mod)$coefficients
df.coef.small <- summary(small.pred.mod)$coefficients
df.coef.full.gamma <- summary(full.pred.mod.gamma)$coefficients
df.coef.small.gamma <- summary(small.pred.mod.gamma)$coefficients

## Gaussian models
exp.var.full<- rownames(summary(full.pred.mod)$coefficients)
exp.var.small<- rownames(summary(small.pred.mod)$coefficients)
gauss.coeff <- data.frame("Exp.Variable"=exp.var.full,"FM Est"=round(df.coef.full[,1],3), "SM Est"=rep(NA, length(df.coef.full[,1])),
                                    "FM SD"=round(df.coef.full[,2],3),"SM SD"=rep(NA, length(df.coef.full[,1])), 
                                    "FM p-val"=round(df.coef.full[,4],3), "SM p-val"=rep(NA, length(df.coef.full[,1])))
gauss.coeff$SM.Est[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small[,1],3)
gauss.coeff$SM.SD[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small[,2],3)
gauss.coeff$SM.p.val[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small[,4],3)
gauss.coeff
write.csv(gauss.coeff,"Tables/GaussCoeff.csv", row.names = FALSE)



## Gamma models
exp.var.full.gamma<- rownames(summary(full.pred.mod.gamma)$coefficients)
exp.var.small.gamma<- rownames(summary(small.pred.mod.gamma)$coefficients)
gamma.coeff <- data.frame("Exp.Variable"=exp.var.full.gamma,"FM Est"=round(df.coef.full.gamma[,1],3), 
                          "SM Est"=rep(NA, length(df.coef.full.gamma[,1])),
                          "FM SD"=round(df.coef.full.gamma[,2],3),"SM SD"=rep(NA, length(df.coef.full.gamma[,1])), 
                          "FM p-val"=round(df.coef.full.gamma[,4],3), "SM p-val"=rep(NA, length(df.coef.full.gamma[,1])))

gamma.coeff$SM.Est[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small.gamma[,1],3)
gamma.coeff$SM.SD[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small.gamma[,2],3)
gamma.coeff$SM.p.val[c(1,2,4,5,6,7,8,10,14,19,20,21,22)]<- round(df.coef.small.gamma[,4],3)
gamma.coeff
write.csv(gamma.coeff,"Tables/GammaCoeff.csv", row.names = FALSE)



################# SAVE ########################################

save(df.total.sc, full.pred.mod, small.pred.mod, full.pred.mod.gamma, 
     small.pred.mod, file="MyData/Models.RData")





