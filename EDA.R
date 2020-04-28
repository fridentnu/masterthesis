##### LOAD PACKAGES #######
library(tidyverse) # ggplot2, dplyr etc.
library(DataExplorer) # EDA package
library(gridExtra) # arrange multiple plots in one figure
library(grid) # text size in grid.arrange
library(reshape2) # melt function



##### Code inspired by and/or based on information from sources mentioned in Chapter 2 in masther thesis, in addition to
# https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619 Data explorer
# https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/
# https://r4ds.had.co.nz/exploratory-data-analysis.html 
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization 


################## Load data set from DataCleaning.R #########
load("MyData/DataCleaning.df.total.RData")


########################## Overview of data set ####################
introduce(df.total)

# Correlation of all possible explanatory variables with the systolic bp from HUNT3
plot_correlation(df.total[,-c(1,10,11,20,21,22,23,24)])
dev.copy(pdf,'~/figures/EDA/TotalCorrelation.pdf') 
dev.off()

# Correlation of all continuous possible explanatory variables with the systolic bp from HUNT3
cor.mat <- round(cor(df.total[,-c(1,3,7,8,9,10,11,12,16,18,20,21,22,23,24)]),2)
ggplot(data=melt(cor.mat))+
  geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 14, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 14, hjust = 1))
dev.copy(pdf,'~/figures/EDA/ContinuousCorrelation.pdf') 
dev.off()

########################## EXPLORE BP HUNT3 ##########################
# Uncorrect the blood pressure values
SystolicBP3.uncorr <- df.total$SystolicBP3
SystolicBP3.uncorr[df.total$BPMed3] <- df.total$SystolicBP3[df.total$BPMed3]-15
DiastolicBP3.uncorr <- df.total$DiastolicBP3
DiastolicBP3.uncorr[df.total$BPMed3] <- df.total$DiastolicBP3[df.total$BPMed3]-10

# Plot BP HUNT3
res.p1 <- ggplot(data=df.total)+
  geom_histogram(mapping = aes(SystolicBP3), binwidth=1)+
  geom_vline(xintercept = 140, color="red")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 16))+
  ylim(0,700)+
  labs(x="[mmHg]")+
  ggtitle("Systolic")
res.p2 <- ggplot(data=df.total)+
  geom_histogram(mapping = aes(DiastolicBP3), binwidth=1)+
  geom_vline(xintercept = 90, color="red")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 16))+
  labs(x="[mmHg]")+
  ylim(0,700)+
  ggtitle("Diastolic")
grid.arrange(res.p1,res.p2,nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/SysDia3.pdf') 
dev.off()

# Check if systolic bp at HUNT3 is approx normal
qqnorm(df.total$SystolicBP3, main="Normal QQ-plot of SystolicBP3", ylab="Systolic blood pressure [mmHg]", cex.main=1.3, cex.lab=1.4, cex.axis=1.7)
qqline(df.total$SystolicBP3, col = "steelblue", lwd = 2)
dev.copy(pdf,'~/figures/EDA/SysQQ.pdf') 
dev.off()


### Systolic hypertension HUNT3

#Proportion of systolic hypertensives
sum(df.total$SystolicHyp)/length(df.total$SystolicBP3)

ggplot(data=df.total)+
  geom_bar(aes(SystolicHyp))+
  theme(axis.title.y = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.x = element_text(size = 24))+
  labs(y="#Participants", x="Systolic hypertension")
dev.copy(pdf,'~/figures/EDA/SysHyp.pdf') 
dev.off()



#### Compare blood pressure before/after correction of blood pressure medication effect
## Systolic
sys.med.p1 <-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=SystolicBP3.uncorr, fill=BPMed3), binwidth = 1)+
  geom_vline(xintercept = mean(df.total$SystolicBP3[!df.total$BPMed3]))+
  geom_vline(xintercept = mean(SystolicBP3.uncorr[df.total$BPMed3]), color="blue")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 16))+
  labs(x="[mmHg]")+
  ylim(0,550)+
  ggtitle("Uncorrected")
sys.med.p2 <-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=SystolicBP3, fill=BPMed3), binwidth=1)+
  geom_vline(xintercept = mean(df.total$SystolicBP3[!df.total$BPMed3]))+
  geom_vline(xintercept = mean(df.total$SystolicBP3[df.total$BPMed3]), color="blue")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 16))+
  labs(x="[mmHg]")+
  ylim(0,550)+
  ggtitle("Corrected")
grid.arrange(sys.med.p1,sys.med.p2,nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90),
             top=textGrob("Systolic blood pressure", gp=gpar(fontsize=16,font=8)))
dev.copy(pdf,'~/figures/EDA/SysCorrection3.pdf') 
dev.off()

## Diastolic
dias.med.p1 <- ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=DiastolicBP3, fill=BPMed3), binwidth = 1)+
  geom_vline(xintercept = 90)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 16))+
  labs(x="[mmHg]")+
  ggtitle("Corrected")
dias.med.p2 <- ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=DiastolicBP3.uncorr, fill=BPMed3), binwidth=1)+ 
  geom_vline(xintercept = 90)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 16))+
  labs(x="[mmHg]")+
  ggtitle("Uncorrected")
grid.arrange(dias.med.p2,dias.med.p1,nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90), top="Diastolic blood pressure")
dev.copy(pdf,'~/figures/EDA/DiaCorrection3.pdf') 
dev.off()


########### BP for subgroups ##################
ill.p1<-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=SystolicBP3, fill= Diabetes3), binwidth = 1)+
  geom_vline(xintercept = mean(df.total$SystolicBP3[!df.total$Diabetes3]))+
  geom_vline(xintercept = mean(df.total$SystolicBP3[df.total$Diabetes3]), color="blue")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 16))+
  labs(x="[mmHg]")+
  ggtitle("Diabetes")

ill.p2 <-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=SystolicBP3, fill=CVD3), binwidth = 1)+
  geom_vline(xintercept = mean(df.total$SystolicBP3[!df.total$CVD3]))+
  geom_vline(xintercept = mean(df.total$SystolicBP3[df.total$CVD3]), color="blue")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 16))+
  labs(x="[mmHg]")+
  ggtitle("CVD")

grid.arrange(ill.p1,ill.p2, nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90),
             top=textGrob("Systolic blood pressure", gp=gpar(fontsize=16,font=8)))
dev.copy(pdf,'~/figures/EDA/SysEval3.pdf') 
dev.off()


mean(df.total$SystolicBP3[df.total$Diabetes3])
mean(df.total$SystolicBP3[df.total$CVD3])
### very similar means


##################### EXPLANATORY VARIABLES #################
# check that all are in the format they should be 
# check for outliers
# check distribution
# check correlation with SystolicBP3


################# BASIC INFO ########################
# sex, age
df.basic <- data.frame("Sex"=df.total$Sex, "Birthyear"=df.total$BirthYear)
df.basic.res <- data.frame("Sex"=df.total$Sex, "Birthyear"=df.total$BirthYear,
                           "SystolicBP3"=df.total$SystolicBP3, "SystolicHyp"=df.total$SystolicHyp)

### Inspect variables ---------------------------------------------
# Birthyear
ggplot(data=df.basic)+
  geom_histogram(mapping = aes(x=Birthyear), binwidth=1)+
  theme(axis.title.x = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 24))+
  labs(y="#Participants", x="Birthyear")
dev.copy(pdf,'~/figures/EDA/BasicInfo/Birthyear.pdf') 
dev.off()


# Sex
ggplot(data=df.basic)+
  geom_bar(mapping = aes(x=Sex))+
  theme(axis.title.x = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 24))+
  labs(y="#Participants")
dev.copy(pdf,'~/figures/EDA/BasicInfo/Sex.pdf')
dev.off()

###### Correlation between sex and SystolicBP3 -------------------------------------------
ggplot(data=df.basic.res)+
  geom_boxplot(mapping = aes(x=Sex,y=SystolicBP3))+
  theme(axis.title.x = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 24))+
  labs(y="Systolic blood pressure [mmHg]")
dev.copy(pdf,'~/figures/EDA/BasicInfo/SexVSBP.pdf') 
dev.off()




###### Correlation between Birthyear and SystolicBP3 ----------------------------------
cat("Correlation Birthyear and SystolicBP3: ",cor(df.total$BirthYear, df.total$SystolicBP3), sep=" ")

df.basic.res %>%
  mutate( bin=cut_width(Birthyear, width=5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=SystolicBP3) ) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 24))+
  labs(y="Systolic blood pressure [mmHg]", x="Birthyear")
dev.copy(pdf,'~/figures/EDA/BasicInfo/CorrContBirthyear.pdf') 
dev.off()


################# BLOOD PRESSURE #################
# systlic and diastolic bp at hunt2, hypertensive parents
df.bp <- data.frame("SystolicBP2"=df.total$SystolicBP2, "DiastolicBP2"=df.total$DiastolicBP2, "BPHigPar"=df.total$BPHigPar)
df.bp.res <- data.frame("SystolicBP2"=df.total$SystolicBP2, "DiastolicBP2"=df.total$DiastolicBP2, "BPHigPar"=df.total$BPHigPar, 
                        "SystolicBP3"=df.total$SystolicBP3, "SystolicHyp"=df.total$SystolicHyp)

#### Inspect variables ----------------------------------

## Systolic and diastolic bp at HUNT2 
bp.p1 <-ggplot(data=df.bp)+
  geom_histogram(mapping = aes(x=SystolicBP2), binwidth=1)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=20),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 20))+
  labs(x="[mmHg]")+
  ggtitle("Systolic")
bp.p2 <- ggplot(data=df.bp)+
  geom_histogram(mapping = aes(x=DiastolicBP2), binwidth=1)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=20),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 20))+
  labs(x="[mmHg]")+
  ggtitle("Diastolic")
grid.arrange(bp.p2, bp.p1, nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90), 
             top=textGrob("Blood pressure from HUNT2", gp=gpar(fontsize=20,font=8)))
dev.copy(pdf,'~/figures/EDA/BloodPressure/BP2.pdf')
dev.off()

cat("The mean sys bp at HUNT2 was ", mean(df.total$SystolicBP2), "and at HUNT3 it was ", mean(df.total$SystolicBP3))
cat("The mean dias bp at HUNT2 was ", mean(df.total$DiastolicBP2), "and at HUNT3 it was ", mean(df.total$DiastolicBP3))


## Parental hypertension
ggplot(data=df.bp)+
  geom_bar(mapping = aes(x=BPHigPar))+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="Hypertensive parents")
dev.copy(pdf,'~/figures/EDA/BloodPressure/HypertensiveParents.pdf') 
dev.off()


##### Correlation -------------------------------

### Correlation between parental hypertension and systolicbp3
ggplot(data=df.bp.res)+
  geom_boxplot(mapping = aes(x=BPHigPar,y=SystolicBP3))+
  theme(axis.title.y = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.x = element_text(size=24))+
  labs(x="Hypertensive parents",y="Systolic blood pressure [mmHg]")
dev.copy(pdf,'~/figures/EDA/BloodPressure/HypertensiveParentsVSBP.pdf')
dev.off()


###Correlation between systolicbp2 and diastolicbp2 and systolicbp3
cor.mat.bp <- round(cor(data.frame("SystolicBP2"=df.total$SystolicBP2,"DiastolicBP2"=df.total$DiastolicBP2,"SystolicBP3"=df.total$SystolicBP3)),2)
ggplot(data=melt(cor.mat.bp))+
      geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 14, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 14, hjust = 1))
dev.copy(pdf,'~/figures/EDA/BloodPressure/CorrContBP.pdf') 
dev.off()


################## LIFESTYLE ###############
# bmi, smoking, pai, recpa, educational level
df.life <- data.frame("BMI"=df.total$BMI2, "Smoking"=df.total$Smoking2, "PAI"=df.total$PAI2, "RecPA"=df.total$RecPA2, "Education"=df.total$Education2)
df.life.res <- data.frame("BMI"=df.total$BMI2, "Smoking"=df.total$Smoking2, "PAI"=df.total$PAI2, "RecPA"=df.total$RecPA2,
                          "Education"=df.total$Education2, "SystolicBP3"=df.total$SystolicBP3, "SystolicHyp"=df.total$SystolicHyp)

#### Inspect variables ---------------------------------
# BMI
ggplot(data=df.life)+
  geom_histogram(mapping = aes(x=BMI), binwidth=1)+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="BMI [kg/m²]")
dev.copy(pdf,'~/figures/EDA/Life/BMI.pdf') 
dev.off()

# PAI
life.p2 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=PAI))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(x="PAI")

# RecPA
life.p3 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=RecPA))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(x="RecPA")
grid.arrange(life.p2, life.p3, nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/Life/PhysicalActivity.pdf') 
dev.off()

# Smoking
life.p1 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=Smoking))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))+
  labs(x="Smoking")
        
# Education
life.p4 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=Education))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))+
  labs(x="Education")
grid.arrange(life.p1, life.p4, nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/Life/SmokEduc.pdf') 
dev.off()


##### Correlation ---------------------------------
## Correlation between bmi and systolicBP3
cat("Correlation BMI and SystolicBP3: ",cor(df.total$BMI, df.total$SystolicBP3))
df.life.res %>%
  mutate( bin=cut_width(BMI, width=5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=SystolicBP3) ) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=16),
        axis.text.y=element_text(size=16),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24))+
  labs(y="Systolic blood pressure [mmHg]", x="BMI [kg/m²]")
dev.copy(pdf,'~/figures/EDA/Life/CorrContBMIvsBP.pdf') 
dev.off()


# Correlation between smoking and systolicbp3 
life.c1 <-ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Smoking,y=SystolicBP3))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))+
  labs(x="Smoking")

# Correlation between education and systolicbp3 
life.c4 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Education,y=SystolicBP3))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))+
  labs(x="Education")
grid.arrange(life.c1,life.c4, nrow=1, left=textGrob("Systolic blood pressure [mmHg]", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/Life/CorrContSmokEduc.pdf') 
dev.off()

# Correlation between PAI and systolicbp3 
life.c2 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=PAI,y=SystolicBP3))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(x="PAI")

# Correlation between RecPA and systolicbp3 
life.c3 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=RecPA,y=SystolicBP3))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(x="RecPA")
grid.arrange(life.c2, life.c3, nrow=1, left=textGrob("Systolic blood pressure [mmHg]", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/Life/CorrContPA.pdf') 
dev.off()

# check the negative correlation with physical activity
mean(df.total$SystolicBP3[df.total$RecPA])
mean(df.total$SystolicBP3[!df.total$RecPA])

mean(df.total$SystolicBP3[df.total$PAI=="High"])
mean(df.total$SystolicBP3[df.total$PAI=="Low"])

# Check correlation smoking and birthyear
life.corr1 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Smoking,y=df.total$BirthYear))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))

# Check correlation education and birthyear
life.corr2<- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Education,y=df.total$BirthYear))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))
grid.arrange(life.corr1, life.corr2, nrow=1, left=textGrob("Birthyear", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/Life/CorrSmokEducBY.pdf') 
dev.off()


################# BLOOD SAMPLES ###############
# grfe, creatinine, chol, hdl chol, blood glucose

df.blood <- data.frame("GFR"=df.total$GFR2, "Creatinine"=df.total$Creatinine2, "Cholesterol"=df.total$Cholesterol2, 
                       "HDL.Cholesterol"=df.total$HDLCholesterol2, "Glucose"=df.total$Glucose2)
df.blood.res <- data.frame("GFR"=df.total$GFR2, "Creatinine"=df.total$Creatinine2, "Cholesterol"=df.total$Cholesterol2, 
                           "HDL.Cholesterol"=df.total$HDLCholesterol2, "Glucose"=df.total$Glucose2,
                           "SystolicBP3"=df.total$SystolicBP3, "SystolicHyp"=df.total$SystolicHyp)

#### Inspect variables -----------------------------
# GFR
ggplot(data=df.blood)+
  geom_bar(mapping = aes(x=GFR))+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="#Participants")
dev.copy(pdf,'~/figures/EDA/BloodSamples/GFR.pdf') 
dev.off()

### Comment: see from plot that very few in stage 3, only one in 4, and no one in stage 5
cat("Participants in Stage 3: ", sum(df.blood$GFR=="Stage 3"))
cat("Participants in Stage 4: ", sum(df.blood$GFR=="Stage 4"))
cat("Participants in Stage 5: ",sum(df.blood$GFR=="Stage 5"))

# Collapse stage 3,4 and 5 into stage 345
df.total$GFR2<-df.total$GFR2 %>% fct_collapse("Stage 345" = c("Stage 3","Stage 4", "Stage 5"))

ggplot(data=df.total)+
  geom_bar(mapping = aes(x=GFR2))+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="#Participants")
dev.copy(pdf,'~/figures/EDA/BloodSamples/GFR.pdf') 
dev.off()

# Cholesterol
blood.p1 <-ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Cholesterol), binwidth=0.2)+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size=16), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="Cholesterol [mmol/L]")

# HDL Cholesterol
blood.p2 <- ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=HDL.Cholesterol), binwidth=0.1)+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size=16), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="HDL Cholesterol [mmol/L]")

# Creatinine
blood.p3 <-ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Creatinine), binwidth=6)+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size=16), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="Creatinine [micromol/L]")

# Glucose
blood.p4 <- ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Glucose), binwidth=0.2)+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size=16), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="Glucose [mmol/L]")

grid.arrange(blood.p2, blood.p1, blood.p3, blood.p4, nrow=2, top=textGrob("Blood samples", gp=gpar(fontsize=16,font=8)))
dev.copy(pdf,'~/figures/EDA/BloodSamples/continuous.pdf') 
dev.off()

##### Correlation -----------------------------------
## Correlation between GFR and systolicBP3
ggplot(data=df.total)+
  geom_boxplot(mapping = aes(x=GFR2,y=SystolicBP3))+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="Systolic blood pressure [mmHg]", x="GFR")
dev.copy(pdf,'~/figures/EDA/BloodSamples/CorrContGFR.pdf') 
dev.off()

## Correlation between continuous blood samples and systolicBP3 
cor.mat.blood <- round(cor(df.blood.res[,-c(1,7)]),2)
ggplot(data=melt(cor.mat.blood))+
  geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 14, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 14, hjust = 1))
dev.copy(pdf,'~/figures/EDA/BloodSamples/CorrContCont.pdf') 
dev.off()



#############################

save(df.total, file="MyData/EDA.df.total.RData")

