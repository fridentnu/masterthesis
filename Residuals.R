##### LOAD PACKAGES #######
library(tidyverse) # ggplot2, dplyr etc.
library(gridExtra) # arrange multiple plots in one figure
library(grid) # text size in grid.arrange


# References can be found in Chapter 3 and 4 in the master thesis

# Run Models.R to access models and the estimated parameters of the prediction distributions
source("R code/Models.R")

################################### RESIDUALS ################################


###### Full Gaussian model 
df.residual.full <-  data.frame("Fitted"=full.pred.mod$fitted.values, "Residuals"=full.pred.mod$residuals) %>%
  mutate( bin=cut_width(x=Fitted, width=10, boundary=0, ordered_result=T) )

plot.residual.full <-ggplot(data=df.residual.full) +
  geom_boxplot(aes(x=bin, y=Residuals ))+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Fitted systolic blood pressure [mmHg]") + ylab("Residuals")+
  ggtitle("Full Gaussian model")
plot.residual.full
dev.copy(pdf,'~/figures/Models/Residuals/ResFitFullGauss.pdf') # Save the plot
dev.off()

#### Small Gaussian model 
df.residual.small <- data.frame("Fitted"=small.pred.mod$fitted.values, "Residuals"=small.pred.mod$residuals)%>%
  mutate( bin=cut_width(x=Fitted, width=10, boundary=0) )

plot.residual.small <- ggplot(data=df.residual.small) +
  geom_boxplot(aes(x=bin, y=Residuals ))+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Fitted systolic blood pressure [mmHg]") + ylab("Residuals")+
  ggtitle("Small Gaussian model")
plot.residual.small
dev.copy(pdf,'~/figures/Models/Residuals/ResFitSmallGauss.pdf') # Save the plot
dev.off()


###### Full gamma model
df.residual.full.gamma <- data.frame("Fitted"=full.pred.mod.gamma$fitted.values, "Residuals"=full.pred.mod.gamma$residuals)%>%
  mutate( bin=cut_width(x=Fitted, width=10, boundary=0) )
plot.residual.full.gamma <- ggplot(data=df.residual.full.gamma) +
  geom_boxplot (aes(x=bin, y=Residuals ))+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Fitted systolic blood pressure HUNT3 [mmHg]") + ylab("Residuals")+
  ggtitle("Full gamma model")
plot.residual.full.gamma
dev.copy(pdf,'~/figures/Models/Residuals/ResFitFullGamma.pdf') # Save the plot
dev.off()

#### Small gamma model 
df.residual.small.gamma <- data.frame("Fitted"=small.pred.mod.gamma$fitted.values, "Residuals"=small.pred.mod.gamma$residuals)%>%
  mutate( bin=cut_width(x=Fitted, width=10, boundary=0) )

plot.residual.small.gamma <- 
  ggplot(data=df.residual.small.gamma) +
  geom_boxplot(aes(x=bin, y=Residuals ))+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 20, hjust = 1),
        axis.text.y=element_text(size = 20),
        plot.title=element_text(size=24))+
  xlab("Predicted systolic blood pressure [mmHg]") + ylab("Residuals")+
  ggtitle("Small gamma model")
plot.residual.small.gamma
dev.copy(pdf,'~/figures/Models/Residuals/ResFitSmallGamma.pdf') # Save the plot
dev.off()

############### Standard deviaton of residuals ############################

# NOTE: we remove all intervals with less than 15 participants

# Function that calculates standard deviation of residuals
sd.cont.res <- function(df.res){
  sd.res <- vector()
  upper.res <- vector()
  lower.res <- vector()
  bin <- vector()
  count=0
  for(i in 1:length(levels(df.res$bin))){
    indexes=which(df.res$bin==levels(df.res$bin)[i])
    if(length(indexes)<15){next}
    print(length(indexes))
    count=count+1
    bin <- c(bin,levels(df.res$bin)[i])
    sd.res<- c(sd.res,sd(df.res$Residuals[indexes]))
    chi_r <-qchisq(0.025,length(indexes)-1, lower.tail=TRUE)
    chi_l <-qchisq(0.025,length(indexes)-1, lower.tail=FALSE)
    lower.res<- c(lower.res,sqrt((length(indexes)-1)*sd.res[count]**2/chi_l))
    upper.res<- c(upper.res,sqrt((length(indexes)-1)*sd.res[count]**2/chi_r))
  }
  return(data.frame("SD"=sd.res, "Lower"=lower.res, "Upper"=upper.res, "bin"=bin))
}


### Full Gaussian model
sd.res.full <- sd.cont.res(df.residual.full)
sd.res.full

plot.res.sd.full <-ggplot(sd.res.full)+
  geom_point(aes(x=bin, y=SD), color="red", shape=8,size=6)+
  geom_point(aes(x=bin,y=Lower), shape=18, size=6,color="blue")+
  geom_point(aes(x=bin,y=Upper), shape=18, size=6,color="blue")+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Fitted systolic blood pressure [mmHg]") + ylab("SD of residuals")+
  ggtitle("Full Gaussian model")+
  scale_x_discrete(limits=levels(df.residual.full$bin))
plot.res.sd.full
dev.copy(pdf,'~/figures/Models/Residuals/SDResFullGauss.pdf') # Save the plot
dev.off()

### Small Gaussian model
sd.res.small <- sd.cont.res(df.residual.small)
sd.res.small

plot.res.sd.small <-ggplot(sd.res.small)+
  geom_point(aes(x=bin, y=SD), color="red", size=6, shape=8)+
  geom_point(aes(x=bin,y=Lower), shape=18, size=6,color="blue")+
  geom_point(aes(x=bin,y=Upper), shape=18,size=6, color="blue")+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Fitted systolic blood pressure [mmHg]") + ylab("SD of residuals")+
  ggtitle("Small Gaussian model")+
  scale_x_discrete(name ="Fitted systolic blood pressure [mmHg]", 
                   limits=levels(df.residual.small$bin))
plot.res.sd.small
dev.copy(pdf,'~/figures/Models/Residuals/SDResSmallGauss.pdf') # Save the plot
dev.off()

### Full Gamma model
sd.res.full.gamma <- sd.cont.res(df.residual.full.gamma)
sd.res.full.gamma

plot.res.sd.full.gamma <-ggplot(sd.res.full.gamma)+
  geom_point(aes(x=bin, y=SD), color="red", size=6, shape=8)+
  geom_point(aes(x=bin,y=Lower), shape=18, size=6, color="blue")+
  geom_point(aes(x=bin,y=Upper), shape=18,size=6, color="blue")+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Fitted systolic blood pressure [mmHg]") + ylab("SD of residuals")+
  ggtitle("Full gamma model")+
  scale_x_discrete(name ="Fitted systolic blood pressure [mmHg]", 
                   limits=levels(df.residual.full.gamma$bin))
plot.res.sd.full.gamma
dev.copy(pdf,'~/figures/Models/Residuals/SDResFullGamma.pdf') # Save the plot
dev.off()

### Small Gamma model
sd.res.small.gamma <- sd.cont.res(df.residual.small.gamma)
sd.res.small.gamma

plot.res.sd.small.gamma <-ggplot(sd.res.small.gamma)+
  geom_point(aes(x=bin,y=Lower), shape=18, size=6,color="blue")+
  geom_point(aes(x=bin,y=Upper), shape=18,size=6, color="blue")+
  geom_point(aes(x=bin, y=SD), color="red", size=6,shape=8)+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 20, hjust = 1),
        axis.text.y=element_text(size = 20),
        plot.title=element_text(size=24))+
  xlab("Predicted systolic blood pressure [mmHg]") + ylab("SD of residuals")+
  ggtitle("Small gamma model")+
  scale_x_discrete(name ="Predicted systolic blood pressure [mmHg]", 
                   limits=levels(df.residual.small.gamma$bin))
plot.res.sd.small.gamma
dev.copy(pdf,'~/figures/Models/Residuals/SDResSmallGamma.pdf') # Save the plot
dev.off()


###########  Residuals versus explanatory variables ###############

##### Small gamma model #########

df.residual.exp.var <- data.frame("SystolicBP2"=df.total$SystolicBP2,
                                  "DiastolicBP2"=df.total$DiastolicBP2,
                                  "Birthyear"=df.total$BirthYear,
                                  "BMI"=df.total$BMI2,
                                  "PAI"=df.total$PAI2,
                                  "BPHigPar"=df.total$BPHigPar2,
                                  "HDLCholesterol"=df.total$HDLCholesterol2,
                                  "Education"=df.total$Education2,
                                  "Residuals"=small.pred.mod.gamma$residuals)
## SystolicBP2
plot.residual.sysBP2 <- df.residual.exp.var %>%
  mutate( bin=cut_width(x=SystolicBP2, width=5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("SystolicBP2 [mmHg]") + ylab("Residuals")
grid.arrange(plot.residual.sysBP2, nrow=1)
dev.copy(pdf,'~/figures/Models/Residuals/ResSys2SmallGamma.pdf') # Save the plot
dev.off()

## DiastolicBP2
plot.residual.diaBP2 <- df.residual.exp.var %>%
  mutate( bin=cut_width(x=DiastolicBP2, width=5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("DiastolicBP2 [mmHg]") + ylab("Residuals")
grid.arrange(plot.residual.diaBP2, nrow=1)
dev.copy(pdf,'~/figures/Models/Residuals/ResDia2SmallGamma.pdf') # Save the plot
dev.off()

## BMI
plot.residual.BMI <- df.residual.exp.var %>%
  mutate( bin=cut_width(x=BMI, width=5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("BMI [kg/m²]") + ylab("Residuals")
grid.arrange(plot.residual.BMI, nrow=1)
dev.copy(pdf,'~/figures/Models/Residuals/ResBMISmallGamma.pdf') # Save the plot
dev.off()

## Birthyear
plot.residual.birthyear<- df.residual.exp.var %>%
  mutate( bin=cut_width(x=Birthyear, width=5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("Birthyear") + ylab("Residuals")
grid.arrange(plot.residual.birthyear, nrow=1)
dev.copy(pdf,'~/figures/Models/Residuals/ResBYSmallGamma.pdf') # Save the plot
dev.off()

## HDL Cholesterol
plot.residual.hdl<- df.residual.exp.var %>%
  mutate( bin=cut_width(x=HDLCholesterol, width=0.2, boundary=0) ) %>%
  ggplot( aes(x=bin, y=Residuals )) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("HDL Cholesterol [mmol/L]") + ylab("Residuals")
grid.arrange(plot.residual.hdl, nrow=1)
dev.copy(pdf,'~/figures/Models/Residuals/ResHDLSmallGamma.pdf') # Save the plot
dev.off()

## PAI
ggplot(df.residual.exp.var)+
  geom_boxplot(mapping = aes(x=PAI, y=Residuals))+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))
dev.copy(pdf,'~/figures/Models/Residuals/ResPAISmallGamma.pdf') # Save the plot
dev.off()

## BPHigPar
ggplot(df.residual.exp.var)+
  geom_boxplot(mapping = aes(x=BPHigPar, y=Residuals))+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))
dev.copy(pdf,'~/figures/Models/Residuals/ResBPHigParSmallGamma.pdf') # Save the plot
dev.off()

## Education
ggplot(df.residual.exp.var)+
  geom_boxplot(mapping = aes(x=Education, y=Residuals))+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))
dev.copy(pdf,'~/figures/Models/Residuals/ResEducSmallGamma.pdf') # Save the plot
dev.off()



################# Standard deviation of residuals versus explanatory variables ##########

## Birthyear
df.residual.by <-  data.frame("BirthYear"=df.total$BirthYear, "Residuals"=small.pred.mod.gamma$residuals) %>%
  mutate( bin=cut_width(x=BirthYear, width=10, boundary=0,dig.lab=5) )

sd.res.by <- sd.cont.res(df.residual.by)
sd.res.by

plot.res.sd.by <-ggplot(sd.res.by)+
  geom_point(aes(x=bin, y=SD), color="red",size=6, shape=8)+
  geom_point(aes(x=bin,y=Lower), shape=18, size=6, color="blue")+
  geom_point(aes(x=bin,y=Upper), shape=18, size=6, color="blue")+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("Birthyear") + ylab("SD of residuals")+
  ylim(10,20)+
  scale_x_discrete(limits=levels(df.residual.by$bin))
plot.res.sd.by
dev.copy(pdf,'~/figures/Models/Residuals/SDResBYSmallGamma.pdf') # Save the plot
dev.off()

## BMI
df.residual.bmi <-  data.frame("BMI"=df.total$BMI, "Residuals"=small.pred.mod.gamma$residuals) %>%
  mutate( bin=cut_width(x=BMI, width=10, boundary=0, dig.lab=5) )

sd.res.bmi <- sd.cont.res(df.residual.bmi)
sd.res.bmi

plot.res.sd.bmi <-ggplot(sd.res.bmi)+
  geom_point(aes(x=bin, y=SD), color="red", size=6,shape=8)+
  geom_point(aes(x=bin,y=Lower), shape=18,size=6, color="blue")+
  geom_point(aes(x=bin,y=Upper), shape=18,size=6, color="blue")+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("BMI [kg/m²]") + ylab("SD of residuals")+
  ylim(10,20)+
  scale_x_discrete(limits=levels(df.residual.bmi$bin))
plot.res.sd.bmi
dev.copy(pdf,'~/figures/Models/Residuals/SDResBMISmallGamma.pdf') # Save the plot
dev.off()

## SystolicBP2
df.residual.sys2 <-  data.frame("Sys2"=df.total$SystolicBP2, "Residuals"=small.pred.mod.gamma$residuals) %>%
  mutate( bin=cut_width(x=Sys2, width=10, boundary=0, dig.lab=5) )

sd.res.sys2 <- sd.cont.res(df.residual.sys2)
sd.res.sys2

plot.res.sd.sys2 <-ggplot(sd.res.sys2)+
  geom_point(aes(x=bin, y=SD), color="red", size=6,shape=8)+
  geom_point(aes(x=bin,y=Lower), shape=18,size=6, color="blue")+
  geom_point(aes(x=bin,y=Upper), shape=18,size=6, color="blue")+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("SystolicBP2 [mmHg]") + ylab("SD of residuals")+
  ylim(10,20)+
  scale_x_discrete(limits=levels(df.residual.sys2$bin))
plot.res.sd.sys2
dev.copy(pdf,'~/figures/Models/Residuals/SDRessys2SmallGamma.pdf') # Save the plot
dev.off()


## DiastolicBP2
df.residual.dia2 <-  data.frame("Dia2"=df.total$DiastolicBP2, "Residuals"=small.pred.mod.gamma$residuals) %>%
  mutate( bin=cut_width(x=Dia2, width=10, boundary=0, dig.lab=5) )

sd.res.dia2 <- sd.cont.res(df.residual.dia2)
sd.res.dia2

plot.res.sd.dia2 <-ggplot(sd.res.dia2)+
  geom_point(aes(x=bin, y=SD), color="red", size=6,shape=8)+
  geom_point(aes(x=bin,y=Lower), shape=18,size=6, color="blue")+
  geom_point(aes(x=bin,y=Upper), shape=18,size=6, color="blue")+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("DiastolicBP2 [mmHg]") + ylab("SD of residuals")+
  ylim(10,20)+
  scale_x_discrete(limits=levels(df.residual.dia2$bin))
plot.res.sd.dia2
dev.copy(pdf,'~/figures/Models/Residuals/SDResDia2SmallGamma.pdf') # Save the plot
dev.off()


## HDL Cholesterol
df.residual.hdl <-  data.frame("hdl"=df.total$HDLCholesterol2, "Residuals"=small.pred.mod.gamma$residuals) %>%
  mutate( bin=cut_width(x=hdl, width=1, boundary=0, dig.lab=5) )

sd.res.hdl <- sd.cont.res(df.residual.hdl)
sd.res.hdl

plot.res.sd.hdl <-ggplot(sd.res.hdl)+
  geom_point(aes(x=bin, y=SD), color="red", size=6,shape=8)+
  geom_point(aes(x=bin,y=Lower), shape=18,size=6, color="blue")+
  geom_point(aes(x=bin,y=Upper), shape=18,size=6, color="blue")+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("HDL Cholesterol [mmol/L]") + ylab("SD of residuals")+
  ylim(10,20)+
  scale_x_discrete(limits=levels(df.residual.hdl$bin))
plot.res.sd.hdl
dev.copy(pdf,'~/figures/Models/Residuals/SDResHDLSmallGamma.pdf') # Save the plot
dev.off()


## PAI
df.residual.pai <-  data.frame("PAI"=df.total$PAI2, "Residuals"=small.pred.mod.gamma$residuals)

sd.pai.res <- function(df.res){
  print(head(df.res$PAI))
  sd.res <- rep(0,length(levels(df.res$PAI)))
  upper.res <- rep(0,length(levels(df.res$PAI)))
  lower.res <- rep(0,length(levels(df.res$PAI)))
  for(i in 1:length(levels(df.res$PAI))){
    indexes=which(df.res$PAI==levels(df.res$PAI)[i])
    if(length(indexes)==1){next}
    print(length(indexes))
    sd.res[i]<- sd(df.res$Residuals[indexes])
    chi_r <-qchisq(0.025,length(indexes)-1, lower.tail=TRUE)
    chi_l <-qchisq(0.025,length(indexes)-1, lower.tail=FALSE)
    lower.res[i]<- sqrt((length(indexes)-1)*sd.res[i]**2/chi_l)
    upper.res[i]<- sqrt((length(indexes)-1)*sd.res[i]**2/chi_r)
  }
  return(data.frame("SD"=sd.res, "Lower"=lower.res, "Upper"=upper.res))
}
sd.pai.res.pai <- sd.pai.res(df.residual.pai)
sd.pai.res.pai

plot.res.sd.pai <-ggplot(sd.pai.res.pai)+
  geom_point(aes(x=levels(df.residual.pai$PAI), y=SD), color="red", size=6, shape=8)+
  geom_point(aes(x=levels(df.residual.pai$PAI),y=Lower), shape=18,size=6, color="blue")+
  geom_point(aes(x=levels(df.residual.pai$PAI),y=Upper), shape=18,size=6, color="blue")+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("PAI") + ylab("SD of residuals")+
  ylim(10,20)+
  scale_x_discrete(limits=levels(df.residual.pai$PAI))
plot.res.sd.pai
dev.copy(pdf,'~/figures/Models/Residuals/SDResPAISmallGamma.pdf') # Save the plot
dev.off()

## Education 
df.residual.edu <-  data.frame("Edu"=df.total$Education2, "Residuals"=small.pred.mod.gamma$residuals)

sd.edu.res <- function(df.res){
  print(head(df.res$Edu))
  sd.res <- rep(0,length(levels(df.res$Edu)))
  upper.res <- rep(0,length(levels(df.res$Edu)))
  lower.res <- rep(0,length(levels(df.res$Edu)))
  for(i in 1:length(levels(df.res$Edu))){
    indexes=which(df.res$Edu==levels(df.res$Edu)[i])
    if(length(indexes)==1){next}
    print(length(indexes))
    sd.res[i]<- sd(df.res$Residuals[indexes])
    chi_r <-qchisq(0.025,length(indexes)-1, lower.tail=TRUE)
    chi_l <-qchisq(0.025,length(indexes)-1, lower.tail=FALSE)
    lower.res[i]<- sqrt((length(indexes)-1)*sd.res[i]**2/chi_l)
    upper.res[i]<- sqrt((length(indexes)-1)*sd.res[i]**2/chi_r)
  }
  return(data.frame("SD"=sd.res, "Lower"=lower.res, "Upper"=upper.res))
}
sd.edu.res.edu <- sd.edu.res(df.residual.edu)
sd.edu.res.edu

plot.res.sd.edu <-ggplot(sd.edu.res.edu)+
  geom_point(aes(x=levels(df.residual.edu$Edu), y=SD), color="red", size=6, shape=8)+
  geom_point(aes(x=levels(df.residual.edu$Edu),y=Lower), shape=18,size=6, color="blue")+
  geom_point(aes(x=levels(df.residual.edu$Edu),y=Upper), shape=18,size=6, color="blue")+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  xlab("Education level") + ylab("SD of residuals")+
  ylim(10,20)+
  scale_x_discrete(limits=levels(df.residual.edu$Edu))
plot.res.sd.edu
dev.copy(pdf,'~/figures/Models/Residuals/SDResEducSmallGamma.pdf') # Save the plot
dev.off()


## BPHigPar
df.residual.bphigpar <-  data.frame("BPHigPar"=df.total$BPHigPar2, "Residuals"=small.pred.mod.gamma$residuals)

# TRUE
sd.res.T<- sd(df.residual.bphigpar$Residuals[df.residual.bphigpar$BPHigPar==TRUE])
chi_r.T <-qchisq(0.025,length(df.residual.bphigpar$BPHigPar==TRUE)-1, lower.tail=TRUE)
chi_l.T <-qchisq(0.025,length(df.residual.bphigpar$BPHigPar==TRUE)-1, lower.tail=FALSE)
lower.res.T<- sqrt((length(df.residual.bphigpar$BPHigPar==TRUE)-1)*sd.res.T**2/chi_l.T)
upper.res.T<- sqrt((length(df.residual.bphigpar$BPHigPar==TRUE)-1)*sd.res.T**2/chi_r.T)
 
# FALSE
sd.res.F<- sd(df.residual.bphigpar$Residuals[df.residual.bphigpar$BPHigPar==FALSE])
chi_r.F <-qchisq(0.025,length(df.residual.bphigpar$BPHigPar==FALSE)-1, lower.tail=TRUE)
chi_l.F <-qchisq(0.025,length(df.residual.bphigpar$BPHigPar==FALSE)-1, lower.tail=FALSE)
lower.res.F<- sqrt((length(df.residual.bphigpar$BPHigPar==FALSE)-1)*sd.res.F**2/chi_l.F)
upper.res.F<- sqrt((length(df.residual.bphigpar$BPHigPar==FALSE)-1)*sd.res.F**2/chi_r.F)

sd.res.bphigpar <- data.frame("SD"=c(sd.res.T, sd.res.F), "Lower"=c(lower.res.T,lower.res.F), "Upper"=c(upper.res.T, upper.res.F))

plot.res.sd.bphigpar <-ggplot(sd.res.bphigpar)+
  geom_point(aes(x=c(TRUE, FALSE), y=SD), color="red", size=6, shape=8)+
  geom_point(aes(x=c(TRUE, FALSE),y=Lower), shape=18,size=6, color="blue")+
  geom_point(aes(x=c(TRUE, FALSE),y=Upper), shape=18,size=6, color="blue")+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 24, hjust = 1),
        axis.text.y=element_text(size = 24))+
  ylim(10,20)+
  xlab("BPHigPar") + ylab("SD of residuals")
plot.res.sd.bphigpar
dev.copy(pdf,'~/figures/Models/Residuals/SDResBPHigParSmallGamma.pdf') # Save the plot
dev.off()


#################### DIFFERENCE BETWEEN RESIDUALS ###################

df.diff.res <- data.frame("DiffGauss"=full.pred.mod$residuals-small.pred.mod$residuals,
                          "DiffGamma"=full.pred.mod.gamma$residuals-small.pred.mod.gamma$residuals,
                          "DiffModSmall"=small.pred.mod$residuals-small.pred.mod.gamma$residuals, 
                          "DiffModFull"=full.pred.mod$residuals-full.pred.mod.gamma$residuals, 
                          "Sys3"=sort(df.total$SystolicBP3))

# Difference in residuals, Gaussian models
df.diff.res %>%
  mutate( bin=cut_width(x=Sys3, width=10, boundary=0) ) %>%
  ggplot( aes(x=bin, y= DiffGauss)) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 20, hjust = 1),
        axis.text.y=element_text(size = 24),
        plot.title =element_text(size = 30))+
  xlab("SystolicBP3 [mmHg]") + ylab("FM - SM Gaussian res.")+
  ggtitle("Gaussian models ")
dev.copy(pdf,'~/figures/Models/Residuals/DiffGauss.pdf') # Save the plot
dev.off()

# Difference in residuals, gamma models
df.diff.res %>%
  mutate( bin=cut_width(x=Sys3, width=10, boundary=0) ) %>%
  ggplot( aes(x=bin, y= DiffGamma)) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 20, hjust = 1),
        axis.text.y=element_text(size = 24),
        plot.title =element_text(size = 30))+
  xlab("SystolicBP3 [mmHg]") + ylab("FM - SM gamma res.")+
  ggtitle("Gamma models")
dev.copy(pdf,'~/figures/Models/Residuals/DiffGamma.pdf') # Save the plot
dev.off()

# Difference in residuals, small gauss and small gamma model
df.diff.res %>%
  mutate( bin=cut_width(x=Sys3, width=10, boundary=0) ) %>%
  ggplot( aes(x=bin, y= DiffModSmall)) +
  geom_boxplot()+
  theme(axis.title.x = element_text(size=28),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 20, hjust = 1),
        axis.text.y=element_text( size = 24),
        plot.title =element_text(size = 30))+
  xlab("SystolicBP3 [mmHg]") + ylab("SM  Gauss. - SM gamma res.")+
  ggtitle("Small models")
dev.copy(pdf,'~/figures/Models/Residuals/DiffResSmall.pdf') # Save the plot
dev.off()

