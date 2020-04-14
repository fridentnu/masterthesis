#library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(DataExplorer)
library(gridExtra)
library(reshape2)
library(forcats)


# Sources
# https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619 Data explorer
# https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/
# https://r4ds.had.co.nz/exploratory-data-analysis.html 
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization 

# Load data set from DataCleaning
load("MyData/DataCleaning.df.total.RData")
########################## Overview ####################3


### ADD BINARY HYPERTENSION VARIABLE
df.total$SystolicHyp <- df.total$SystolicBP3>=140


introduce(df.total)
#describe(df.total)
# gmd= Gini mean distance, mean absolute difference between any pair of observations
### Comment: doesn't seem to be any single extreme outliers
### Lower minimum values for BP at HUNT3 than HUNT2
### checked and some people were removed (originally HUNT2 min was 70 and 30),
### which is still higher than (60 and 26) HUNT3


# Continuous variables
plot_histogram(df.total)
### Comment: multiple variables have some values that are much more common than the others
### and see from width of plots that there are some outliers

# Categorical variables
#plot_bar(df.total)  
## Comment: most levels are well represented, excpect stage 3,4 and 5 in GFRest

# Correlation of the all variables with the systolic bp from hunt3
plot_correlation(df.total[,-c(1,10,11,20,21,22,23,24)])
dev.copy(pdf,'~/figures/EDA/TotalCorrelation.pdf') # Save the plot
dev.off()# appendix


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
dev.copy(pdf,'~/figures/EDA/ContinuousCorrelation.pdf') # Save the plot
dev.off()
### Comment: Negative correlation Birthyear and Cholesterol (ie. Cholesterol goes up with age)
### biggest negative correlation with response (SystolicBP3) was birthyear
### biggest positive correlation with response (SystolicBP3) was bpsys2


########################## RESPONSE ##########################
# Uncorrect the blood pressure values
SystolicBP3.uncorr <- df.total$SystolicBP3
SystolicBP3.uncorr[df.total$BPMed3] <- df.total$SystolicBP3[df.total$BPMed3]-15
DiastolicBP3.uncorr <- df.total$DiastolicBP3
DiastolicBP3.uncorr[df.total$BPMed3] <- df.total$DiastolicBP3[df.total$BPMed3]-10

# Look at response
# plot_histogram(data.frame(df$SystolicBP3, df$DiastolicBP3))
res.p1 <- ggplot(data=df.total)+
  geom_histogram(mapping = aes(SystolicBP3), binwidth=1)+
  geom_vline(xintercept = 140, color="red")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 16))+
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
  ggtitle("Diastolic")
grid.arrange(res.p1,res.p2,nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/SysDia3.pdf') # Save the plot
dev.off()
### Comment: many people are hypertensive, see most have problem with systolic
### spike 121 and 122 certain values for systolic 

# check if systolic 3 is approx normal
qqnorm(df.total$SystolicBP3, main="Normal QQ-plot of SystolicBP3", ylab="Systolic blood pressure [mmHg]", cex.main=1.3, cex.lab=1.4, cex.axis=1.7)
qqline(df.total$SystolicBP3, col = "steelblue", lwd = 2)
# seems like data have heavier right tail, and lighter left tail
dev.copy(pdf,'~/figures/EDA/SysQQ.pdf') # Save the plot
dev.off()

df.total$SysDiff <- df.total$SystolicBP3-df.total$SystolicBP2

# distribution of difference between HUNT3 and HUNT2
ggplot(data=df.total)+
  geom_histogram(mapping = aes(SysDiff), binwidth=1)+
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size=16),
        plot.title = element_text(size = 12))+
  labs(x="[mmHg]")+
  ggtitle("Systolic HUNT3 - HUNT2")



qqnorm(df.total$SysDiff)
qqline(df.total$SysDiff, col = "steelblue", lwd = 2)
# heavier right tail


## BINARY HYPERTENSION VARIABLE

# only binary variable for systolic hypertension, since only look at systolic bp as response
sum(df.total$SystolicHyp)/length(df.total$SystolicBP3)

ggplot(data=df.total)+
  geom_bar(aes(SystolicHyp))+
  theme(axis.title.y = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.x = element_text(size = 24))+
  labs(y="#Participants", x="Systolic hypertension")
dev.copy(pdf,'~/figures/EDA/SysHyp.pdf') # Save the plot
dev.off()
### Comment: approximately 20% is hypertensive 

sum(df.total$SystolicHyp)/length(df.total$PID)
## different plots, commented out
##### Compare corrected and uncorrected blood pressure
#plot_histogram(data.frame(SystolicBP3.uncorr,df$SystolicBP3))
#plot_histogram(data.frame(DiastolicBP3.uncorr,df$DiastolicBP3))

# sys.p1 <- ggplot(data=df)+
#   geom_histogram(mapping = aes(SystolicBP3), binwidth=1)+
#   geom_vline(xintercept = 140, color="red")+
#   ggtitle("Corrected systolic")
# sys.p2 <- ggplot(data=data.frame(SystolicBP3.uncorr))+
#   geom_histogram(mapping = aes(SystolicBP3.uncorr), binwidth=1)+
#   geom_vline(xintercept = 140,color= "red")+
#   ggtitle("Uncorrected systolic")
# grid.arrange(sys.p1,sys.p2,nrow=1)
# ### Comment: very small difference, since only 6.7% use bp medication
# 
# sum(df.total$BPMed3)/length(df.total$SystolicBP3)
# 
# dias.p1 <- ggplot(data=df)+
#   geom_histogram(mapping = aes(DiastolicBP3), binwidth=1)+
#   geom_vline(xintercept = 90, color="red")+
#   ggtitle("Corrected diastolic")
# dias.p2 <- ggplot(data=data.frame(DiastolicBP3.uncorr))+
#   geom_histogram(mapping = aes(DiastolicBP3.uncorr), binwidth=1)+
#   geom_vline(xintercept = 90,color= "red")+
#   ggtitle("Uncorrected diastolic")
# grid.arrange(dias.p1,dias.p2,nrow=1)
# ### Comment: very small difference, since only 6.7% use bp medication


### Compare people on/off bp medication before/after correction
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
  ggtitle("Corrected")
grid.arrange(sys.med.p1,sys.med.p2,nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90),
             top=textGrob("Systolic blood pressure", gp=gpar(fontsize=16,font=8)))
dev.copy(pdf,'~/figures/EDA/SysCorrection3.pdf') # Save the plot
dev.off()
### OBS: this is a stacked histogram

### Comment: Slightly heavier right tail for corrected than uncorrected, 
### small difference since so few on bpmed
### but in both cases heavier right tail than left tail
### some spikes for 121,122

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
dev.copy(pdf,'~/figures/EDA/DiaCorrection3.pdf') # Save the plot
dev.off()
### Comment:  very slightly heavier right tail for corrected than uncorrected,


# # scatterplot comparing systolic bp of people using vs not using bp medication, after corrected
# ggplot(data=df.total)+
#   geom_point(mapping = aes(x=1:length(SystolicBP3), y=SystolicBP3))+
#   facet_wrap(~BPMed3)+ geom_hline(yintercept = 140, color="red", size=2)+ 
#   ggtitle("Corrected blood pressure")
# 
# # scatterplot comparing systolic bp of people using vs not using bp medication, not corrected
# ggplot(data=df.total)+
#   geom_point(mapping = aes(x=1:length(SystolicBP3), y=SystolicBP3.uncorr))+
#   facet_wrap(~BPMed3)+geom_hline(yintercept = 140, color="red", size=2)+
#   ggtitle("Uncorrected blood pressure")


########### BP for subgroups ##################3
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

# use corrected version of blood pressure here
grid.arrange(ill.p1,ill.p2, nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90),
             top=textGrob("Systolic blood pressure", gp=gpar(fontsize=16,font=8)))
dev.copy(pdf,'~/figures/EDA/SysEval3.pdf') # Save the plot
dev.off()
### Comment: group with diabetes and group with cvd have higher mean bp than total group
## seems that group with diabetes and group with cvd have similar mean
## highest mean for the corrected bp of group on bp medication (no surprise)

mean(df.total$SystolicBP3[df.total$Diabetes3])
mean(df.total$SystolicBP3[df.total$CVD3])
### very similar means


##################### EXPLANATORY VARIABLES #################
# check that all are in the format they should be (no factor to numerical)
# check for outliers
# check distribution

# boxplot of continuous variables against response
plot_boxplot(df.total,  by="SystolicBP3")
# not sure if need this

################# BASIC INFO ##############################
# sex, age
df.basic <- data.frame("Sex"=df.total$Sex, "Birthyear"=df.total$BirthYear)
df.basic.res <- data.frame("Sex"=df.total$Sex, "Birthyear"=df.total$BirthYear,
                           "SystolicBP3"=df.total$SystolicBP3, "SystolicHyp"=df.total$SystolicHyp)

####### Inspect variables ---------------------------------------------
## Continuous variable
ggplot(data=df.basic)+
  geom_histogram(mapping = aes(x=Birthyear), binwidth=1)+
  theme(axis.title.x = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 24))+
  labs(y="#Participants", x="Birthyear")
dev.copy(pdf,'~/figures/EDA/BasicInfo/Birthyear.pdf') # Save the plot
dev.off()
### Comment: see cut-off value, since have to be 20 years to enter study
### mean birthyear=1953.74, ie. mean age HUNT2 was approx 43, mean age HUNT3 was approx. 54
### a few prticipants born before 1920, earliest in 1910


## Categorical variable
ggplot(data=df.basic)+
  geom_bar(mapping = aes(x=Sex))+
  theme(axis.title.x = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 24))+
  labs(y="#Participants")
dev.copy(pdf,'~/figures/EDA/BasicInfo/Sex.pdf') # Save the plot
dev.off()
### Comment: significantly more females in the study.
### Check if this is the case in these age groups in the population in general

###### Correlation categorical variables -------------------------------------------
## Correlation BP Categorical variable
ggplot(data=df.basic.res)+
  geom_boxplot(mapping = aes(x=Sex,y=SystolicBP3))+
  theme(axis.title.x = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 24))+
  labs(y="Systolic blood pressure [mmHg]")
dev.copy(pdf,'~/figures/EDA/BasicInfo/SexVSBP.pdf') # Save the plot
dev.off()
### Comment: males have higher median bp, but highest bp belongs to a woman, and lowest bp to man

## Correlation Hyp categorical variable
# ggplot(data=df.basic.res)+
#   geom_bar(mapping = aes(x=Sex,fill=SystolicHyp))+
#   ggtitle("Sex vs.Hypertension")
# dev.copy(pdf,'~/figures/EDA/CorrBinSex.pdf') # Save the plot
# dev.off()

count.mat <- aggregate(rep(1, length(df.total$SystolicHyp)),by=list(df.total$Sex, df.total$SystolicHyp), FUN=sum)
count.mat$x<- count.mat$x*100/length(df.total$SystolicHyp)
count.mat

# total percentage 
# ggplot(data=count.mat)+
#   geom_tile(mapping = aes(x=Group.1,y=Group.2,fill=x))+
#   scale_fill_gradient2(limit = c(0,100), space = "Lab", name="Percentage") +
#   geom_text(aes(x=Group.1, y=Group.2,label = round(x,2)), color = "black", size = 4)+
#   theme(axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14),
#         axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
#         axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
#   ggtitle("Sex vs. Hypertension")+
#   xlab("Sex") + ylab("Hypertension")
# dev.copy(pdf,'~/figures/EDA/BasicInfo/CorrBinSexTotalPerc.pdf') # Save the plot
# dev.off()

# relative percentage

count.mat.2 <- aggregate(rep(1, length(df.total$SystolicHyp)),by=list(df.total$Sex, df.total$SystolicHyp), FUN=sum)
count.mat.2$x[count.mat.2$Group.1=="Female"] <- count.mat.2$x[count.mat.2$Group.1=="Female"]*100/sum(count.mat.2$x[count.mat.2$Group.1=="Female"])
count.mat.2$x[!count.mat.2$Group.1=="Female"] <- count.mat.2$x[!count.mat.2$Group.1=="Female"]*100/sum(count.mat.2$x[!count.mat.2$Group.1=="Female"])
count.mat.2

ggplot(data=count.mat.2)+
  geom_tile(mapping = aes(x=Group.1,y=Group.2,fill=x))+
  scale_fill_gradient2(limit = c(0,100), space = "Lab", name="Percentage") +
  geom_text(aes(x=Group.1, y=Group.2,label = round(x,2)), color = "black", size = 4)+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Sex") + ylab("Hypertension")
dev.copy(pdf,'~/figures/EDA/BasicInfo/CorrBinSexRelPerc.pdf') # Save the plot
dev.off()


###### Correlation continuous variables ----------------------------------
# Correlation bp continuous variable
cat("Correlation Birthyear and SystolicBP3: ",cor(df.total$BirthYear, df.total$SystolicBP3), sep=" ")
ggplot(data=df.basic.res)+
  geom_point(mapping = aes(x=Birthyear, y=SystolicBP3), alpha=1/7)+
  labs(y="Systolic blood pressure [mmHg]")
#dev.copy(pdf,'~/figures/EDA/BasicInfo/CorrContBirthyear.pdf') # Save the plot
#dev.off()
### Comment: See both from calculation and plot that there is a negative correlation between birthyear and bp
### but also that there are som outliers that doesn't follow this trend


# boxplot
df.basic.res %>%
  mutate( bin=cut_width(Birthyear, width=5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=SystolicBP3) ) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 24))+
  labs(y="Systolic blood pressure [mmHg]", x="Birthyear")
dev.copy(pdf,'~/figures/EDA/BasicInfo/CorrContBirthyear.pdf') # Save the plot
dev.off()


# density
ggplot(data=df.basic.res)+
  geom_bin2d(mapping = aes(x=Birthyear, y=SystolicBP3))+
  labs(y="Systolic blood pressure [mmHg]")


# Correlation hyp continuous variable
ggplot(data=df.basic.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=Birthyear))+
  labs(x="Systolic hypertension")
dev.copy(pdf,'~/figures/EDA/BasicInfo/CorrBinBirthyear.pdf') # Save the plot
dev.off()
### Comment: see that people with systolic hypertension are older.  


################# BLOOD PRESSURE ###############
# systlic and diastolic bp at hunt2, hypertensive parents
df.bp <- data.frame("SystolicBP2"=df.total$SystolicBP2, "DiastolicBP2"=df.total$DiastolicBP2, "BPHigPar"=df.total$BPHigPar)
df.bp.res <- data.frame("SystolicBP2"=df.total$SystolicBP2, "DiastolicBP2"=df.total$DiastolicBP2, "BPHigPar"=df.total$BPHigPar, 
                        "SystolicBP3"=df.total$SystolicBP3, "SystolicHyp"=df.total$SystolicHyp)

# Data explorer
plot_histogram(df.bp)

## Continuous variables
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
dev.copy(pdf,'~/figures/EDA/BloodPressure/BP2.pdf') # Save the plot
dev.off()
### Comment: see cut-off value for both, seems escpecially abrupt for systolic
### might be because diastolic decreases with age

cat("The mean sys bp at HUNT2 was ", mean(df.total$SystolicBP2), "and at HUNT3 it was ", mean(df.total$SystolicBP3))
cat("The mean dias bp at HUNT2 was ", mean(df.total$DiastolicBP2), "and at HUNT3 it was ", mean(df.total$DiastolicBP3))
### Comment: increasing mean bp for sys (due to including hypertensice and ill?)
### decreasing mean for dias, due to age


## Categorical variables
ggplot(data=df.bp)+
  geom_bar(mapping = aes(x=BPHigPar))+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="Hypertensive parents")
dev.copy(pdf,'~/figures/EDA/BloodPressure/HypertensiveParents.pdf') # Save the plot
dev.off()
### Comment: almost 50% as many people who have hypertensive parents as who haven't got hypertensive parents

# Correlation categorical variables
ggplot(data=df.bp.res)+
  geom_boxplot(mapping = aes(x=BPHigPar,y=SystolicBP3))+
  theme(axis.title.y = element_text(size = 24),
        axis.text = element_text(size=16),
        axis.title.x = element_text(size=24))+
  labs(x="Hypertensive parents",y="Systolic blood pressure [mmHg]")
dev.copy(pdf,'~/figures/EDA/BloodPressure/HypertensiveParentsVSBP.pdf') # Save the plot
dev.off()
### Comment: very slighty higher median bp for people with hypertenive parents
### smaller effect than we thought?

# Correlation BP continuous variables
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
dev.copy(pdf,'~/figures/EDA/BloodPressure/CorrContBP.pdf') # Save the plot
dev.off()
### Comment: see that positive correlations between all (as expected)
### stronger correlations between dias and sys in same survey

# Correlation hyp continuous variable
bp.bc1 <-ggplot(data=df.bp.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=SystolicBP2))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20))+
  ggtitle("Systolic")
bp.bc2 <-ggplot(data=df.bp.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=DiastolicBP2))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20))+
  ggtitle("Diastolic")
grid.arrange(bp.bc1, bp.bc2, nrow=1, 
             left=textGrob("Blood pressure HUNT2 [mmHg]", gp=gpar(fontsize=20,font=8), rot=90),
             bottom=textGrob("Systolic hypertension HUNT3", gp=gpar(fontsize=20,font=8)))
dev.copy(pdf,'~/figures/EDA/BloodPressure/CorrBinBP.pdf') # Save the plot
dev.off()
### Comment: as expected

## Correlation Hyp categorical variable
# ggplot(data=df.bp.res)+
#   geom_bar(mapping = aes(x=BPHigPar,fill=SystolicHyp))+
#   ggtitle("Hypertensive parents vs. Systolic hypertension ")

# Percentage of people with hypertesive parents with hypertension
100*sum(df.bp.res$BPHigPar&df.bp.res$SystolicHyp)/sum(df.bp.res$BPHigPar)
# Percentage of people with hypertensive parents with out hypertension
100*(1-sum(df.bp.res$BPHigPar&df.bp.res$SystolicHyp)/sum(df.bp.res$BPHigPar))
# Percentage of people without hypertensive parents with  hypertension
100*sum((!df.bp.res$BPHigPar)&df.bp.res$SystolicHyp)/sum(!df.bp.res$BPHigPar)
# neither
100*(1-sum((!df.bp.res$BPHigPar)&df.bp.res$SystolicHyp)/sum(!df.bp.res$BPHigPar))
### almost 22% of people with hypertensive parents have hypertension
### while only 17.7% of people with non-hypertensive parents are hypertensive



# relative percentage
count.mat.bp <- aggregate(rep(1, length(df.total$SystolicHyp)),by=list(df.total$BPHigPar2, df.total$SystolicHyp), FUN=sum)
count.mat.bp$x[count.mat.bp$Group.1] <- count.mat.bp$x[count.mat.bp$Group.1]*100/sum(count.mat.bp$x[count.mat.bp$Group.1])
count.mat.bp$x[!count.mat.bp$Group.1] <- count.mat.bp$x[!count.mat.bp$Group.1]*100/sum(count.mat.bp$x[!count.mat.bp$Group.1])
count.mat.bp

ggplot(data=count.mat.bp)+
  geom_tile(mapping = aes(x=Group.1,y=Group.2,fill=x))+
  scale_fill_gradient2(limit = c(0,100), space = "Lab", name="Percentage") +
  geom_text(aes(x=Group.1, y=Group.2,label = round(x,2)), color = "black", size = 10)+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Hypertensive parents") + ylab("Hypertension")
dev.copy(pdf,'~/figures/EDA/BloodPressure/CorrBinBPHigParRelPerc.pdf') # Save the plot
dev.off()
## Very similar to results from sex



################## LIFESTYLE ###############

# bmi, smoking, pai, recpa, educational level
df.life <- data.frame("BMI"=df.total$BMI2, "Smoking"=df.total$Smoking2, "PAI"=df.total$PAI2, "RecPA"=df.total$RecPA2, "Education"=df.total$Education2)
df.life.res <- data.frame("BMI"=df.total$BMI2, "Smoking"=df.total$Smoking2, "PAI"=df.total$PAI2, "RecPA"=df.total$RecPA2,
                          "Education"=df.total$Education2, "SystolicBP3"=df.total$SystolicBP3, "SystolicHyp"=df.total$SystolicHyp)
# Continuous variables
ggplot(data=df.life)+
  geom_histogram(mapping = aes(x=BMI), binwidth=1)+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="BMI")
dev.copy(pdf,'~/figures/EDA/Life/BMI.pdf') # Save the plot
dev.off()
### Comment: long right tail, not symmetric, big variation
### mean 25.31 


# Categorical variables
life.p2 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=PAI))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(x="PAI")

life.p3 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=RecPA))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(x="RecPA")
grid.arrange(life.p2, life.p3, nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/Life/PhysicalActivity.pdf') # Save the plot
dev.off()


#### fikse slik at nivåene på smoking blir riktig, prøv å slette alt og kjøre alt på nytt 
life.p1 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=Smoking))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))+
  labs(x="Smoking")
        

life.p4 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=Education))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))+
  labs(x="Education")
grid.arrange(life.p1, life.p4, nrow=1, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/Life/SmokEduc.pdf') # Save the plot
dev.off()

### Comment: all levels of these are well-represented


## Correlation bp continuous variables
cat("Correlation BMI and SystolicBP3: ",cor(df.total$BMI, df.total$SystolicBP3))
ggplot(data=df.life.res)+
  geom_point(mapping = aes(x=BMI, y=SystolicBP3), alpha=1/7)+
  labs(y="Systolic blood pressure [mmHg]")
#dev.copy(pdf,'~/figures/EDA/Life/CorrContBMIvsBP.pdf') # Save the plot
#dev.off()

### Comment: from both calculation and plot, we see a slight positive correlation
### not as big as might expect, biggest BMI not necesarrily highest bp
### people with highest bp is within 20-30 (healthy and overweight range)



df.life.res %>%
  mutate( bin=cut_width(BMI, width=5, boundary=0) ) %>%
  ggplot( aes(x=bin, y=SystolicBP3) ) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=16),
        axis.text.y=element_text(size=16),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24))+
  labs(y="Systolic blood pressure [mmHg]", x="BMI")
dev.copy(pdf,'~/figures/EDA/Life/CorrContBMIvsBP.pdf') # Save the plot
dev.off()

## Correlation hyp continuous variables
ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=BMI))+
  labs(x="Systolic hypertension")
dev.copy(pdf,'~/figures/EDA/Life/CorrBinBMIvsHyp.pdf') # Save the plot
dev.off()

## Correlation categorical variables
life.c1 <-ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Smoking,y=SystolicBP3))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))+
  labs(x="Smoking")


life.c4 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Education,y=SystolicBP3))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))+
  labs(x="Education")

grid.arrange(life.c1,life.c4, nrow=1, left=textGrob("Systolic blood pressure [mmHg]", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/Life/CorrContSmokEduc.pdf') # Save the plot
dev.off()
### Comment: not very big effect of any of th
### see some negative correlation between bp and physical activity 
### somewhat higher for previous daily smoker (but that might be due to age)
### might be same reason for pattern in education, seems like it


life.c2 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=PAI,y=SystolicBP3))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(x="PAI")

life.c3 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=RecPA,y=SystolicBP3))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(x="RecPA")
grid.arrange(life.c2, life.c3, nrow=1, left=textGrob("Systolic blood pressure [mmHg]", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/Life/CorrContPA.pdf') # Save the plot
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
#dev.copy(pdf,'~/figures/EDA/Life/SmokingVSBirthyear.pdf') # Save the plot
#dev.off()

# Check correlation education and birthyear
life.corr2<- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Education,y=df.total$BirthYear))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1))
grid.arrange(life.corr1, life.corr2, nrow=1, left=textGrob("Birthyear", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/EDA/Life/CorrSmokEducBY.pdf') # Save the plot
dev.off()

### Correlation binary 

# Smoking
count.mat.smok <- aggregate(rep(1, length(df.total$SystolicHyp)),by=list(df.total$Smoking2, df.total$SystolicHyp), FUN=sum)
count.mat.smok$x[count.mat.smok$Group.1=="Never"] <- count.mat.smok$x[count.mat.smok$Group.1=="Never"]*100/sum(count.mat.smok$x[count.mat.smok$Group.1=="Never"])
count.mat.smok$x[count.mat.smok$Group.1=="Previous"] <- count.mat.smok$x[count.mat.smok$Group.1=="Previous"]*100/sum(count.mat.smok$x[count.mat.smok$Group.1=="Previous"])
count.mat.smok$x[count.mat.smok$Group.1=="Current"] <- count.mat.smok$x[count.mat.smok$Group.1=="Current"]*100/sum(count.mat.smok$x[count.mat.smok$Group.1=="Current"])
count.mat.smok

ggplot(data=count.mat.smok)+
  geom_tile(mapping = aes(x=Group.1,y=Group.2,fill=x))+
  scale_fill_gradient2(limit = c(0,100), space = "Lab", name="Percentage") +
  geom_text(aes(x=Group.1, y=Group.2,label = round(x,2)), color = "black", size = 4)+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Smoking") + ylab("Hypertension")
dev.copy(pdf,'~/figures/EDA/Life/CorrBinSmokingRelPerc.pdf') # Save the plot
dev.off()

## PAI
count.mat.pai <- aggregate(rep(1, length(df.total$SystolicHyp)),by=list(df.total$PAI2, df.total$SystolicHyp), FUN=sum)
count.mat.pai$x[count.mat.pai$Group.1=="Low"] <- count.mat.pai$x[count.mat.pai$Group.1=="Low"]*100/sum(count.mat.pai$x[count.mat.pai$Group.1=="Low"])
count.mat.pai$x[count.mat.pai$Group.1=="Moderate"] <- count.mat.pai$x[count.mat.pai$Group.1=="Moderate"]*100/sum(count.mat.pai$x[count.mat.pai$Group.1=="Moderate"])
count.mat.pai$x[count.mat.pai$Group.1=="High"] <- count.mat.pai$x[count.mat.pai$Group.1=="High"]*100/sum(count.mat.pai$x[count.mat.pai$Group.1=="High"])
count.mat.pai

ggplot(data=count.mat.pai)+
  geom_tile(mapping = aes(x=Group.1,y=Group.2,fill=x))+
  scale_fill_gradient2(limit = c(0,100), space = "Lab", name="Percentage") +
  geom_text(aes(x=Group.1, y=Group.2,label = round(x,2)), color = "black", size = 4)+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("PAI") + ylab("Hypertension")
dev.copy(pdf,'~/figures/EDA/Life/CorrBinPAIRelPerc.pdf') # Save the plot
dev.off()


## RecPA
count.mat.recpa <- aggregate(rep(1, length(df.total$SystolicHyp)),by=list(df.total$RecPA2, df.total$SystolicHyp), FUN=sum)
count.mat.recpa$x[count.mat.recpa$Group.1] <- count.mat.recpa$x[count.mat.recpa$Group.1]*100/sum(count.mat.recpa$x[count.mat.recpa$Group.1])
count.mat.recpa$x[!count.mat.recpa$Group.1] <- count.mat.recpa$x[!count.mat.recpa$Group.1]*100/sum(count.mat.recpa$x[!count.mat.recpa$Group.1])
count.mat.recpa

ggplot(data=count.mat.recpa)+
  geom_tile(mapping = aes(x=Group.1,y=Group.2,fill=x))+
  scale_fill_gradient2(limit = c(0,100), space = "Lab", name="Percentage") +
  geom_text(aes(x=Group.1, y=Group.2,label = round(x,2)), color = "black", size = 4)+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("RecPA") + ylab("Hypertension")
dev.copy(pdf,'~/figures/EDA/Life/CorrBinRecPARelPerc.pdf') # Save the plot
dev.off()

### Education
count.mat.educ <- aggregate(rep(1, length(df.total$SystolicHyp)),by=list(df.total$Education2, df.total$SystolicHyp), FUN=sum)
count.mat.educ$x[count.mat.educ$Group.1=="Level 1"] <- count.mat.educ$x[count.mat.educ$Group.1=="Level 1"]*100/sum(count.mat.educ$x[count.mat.educ$Group.1=="Level 1"])
count.mat.educ$x[count.mat.educ$Group.1=="Level 2"] <- count.mat.educ$x[count.mat.educ$Group.1=="Level 2"]*100/sum(count.mat.educ$x[count.mat.educ$Group.1=="Level 2"])
count.mat.educ$x[count.mat.educ$Group.1=="Level 3"] <- count.mat.educ$x[count.mat.educ$Group.1=="Level 3"]*100/sum(count.mat.educ$x[count.mat.educ$Group.1=="Level 3"])
count.mat.educ$x[count.mat.educ$Group.1=="Level 4"] <- count.mat.educ$x[count.mat.educ$Group.1=="Level 4"]*100/sum(count.mat.educ$x[count.mat.educ$Group.1=="Level 4"])
count.mat.educ$x[count.mat.educ$Group.1=="Level 5"] <- count.mat.educ$x[count.mat.educ$Group.1=="Level 5"]*100/sum(count.mat.educ$x[count.mat.educ$Group.1=="Level 5"])
count.mat.educ

ggplot(data=count.mat.educ)+
  geom_tile(mapping = aes(x=Group.1,y=Group.2,fill=x))+
  scale_fill_gradient2(limit = c(0,100), space = "Lab", name="Percentage") +
  geom_text(aes(x=Group.1, y=Group.2,label = round(x,2)), color = "black", size = 4)+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("Education") + ylab("Hypertension")
dev.copy(pdf,'~/figures/EDA/Life/CorrBinEducRelPerc.pdf') # Save the plot
dev.off()

## Correlation Hyp categorical variable
# life.bc1 <-ggplot(data=df.life.res)+
#   geom_bar(mapping = aes(x=Smoking,fill=SystolicHyp))+
#   ggtitle("Smoking vs. Systolic hypertension ")
# life.bc2 <-ggplot(data=df.life.res)+
#   geom_bar(mapping = aes(x=PAI,fill=SystolicHyp))+
#   ggtitle("PAI vs. Systolic hypertension ")
# life.bc3 <-ggplot(data=df.life.res)+
#   geom_bar(mapping = aes(x=RecPA,fill=SystolicHyp))+
#   ggtitle("RecPA vs. Systolic hypertension ")
# life.bc4 <-ggplot(data=df.life.res)+
#   geom_bar(mapping = aes(x=Education,fill=SystolicHyp))+
#   ggtitle("Education vs. Systolic hypertension ")
# grid.arrange(life.bc1,life.bc2,life.bc3,life.bc4, nrow=2)
# dev.copy(pdf,'~/figures/EDA/Life/CorrBinCat.pdf') # Save the plot
# dev.off()

### Comment: need to find some other way to display this

################# BLOOD SAMPLES ###############
# grfe, creatinine, chol, hdl chol, blood glucose

df.blood <- data.frame("GFR"=df.total$GFR2, "Creatinine"=df.total$Creatinine2, "Cholesterol"=df.total$Cholesterol2, 
                       "HDL.Cholesterol"=df.total$HDLCholesterol2, "Glucose"=df.total$Glucose2)
df.blood.res <- data.frame("GFR"=df.total$GFR2, "Creatinine"=df.total$Creatinine2, "Cholesterol"=df.total$Cholesterol2, 
                           "HDL.Cholesterol"=df.total$HDLCholesterol2, "Glucose"=df.total$Glucose2,
                           "SystolicBP3"=df.total$SystolicBP3, "SystolicHyp"=df.total$SystolicHyp)

plot_histogram(df.blood)

## Categorical variabel
ggplot(data=df.blood)+
  geom_bar(mapping = aes(x=GFR))+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="#Participants")
dev.copy(pdf,'~/figures/EDA/BloodSamples/GFR.pdf') # Save the plot
dev.off()
### Comment: see from plot that very few in stage 3, only one in 4, and no one in stage 5
### GFR (glomerular filtration rate) how much blood passes through the glomeruli each minute
### more is better, below 60 might mean kidney disease

cat("Participants in Stage 3: ", sum(df.blood$GFR=="Stage 3"))
cat("Participants in Stage 4: ", sum(df.blood$GFR=="Stage 4"))
cat("Participants in Stage 5: ",sum(df.blood$GFR=="Stage 5"))



df.total$GFR2<-df.total$GFR2 %>% fct_collapse("Stage 345" = c("Stage 3","Stage 4", "Stage 5"))

ggplot(data=df.total)+
  geom_bar(mapping = aes(x=GFR2))+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="#Participants")


## Continuous variables
blood.p1 <-ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Cholesterol), binwidth=0.2)+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size=16), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="Cholesterol [mmol/L]")

blood.p2 <- ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=HDL.Cholesterol), binwidth=0.1)+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size=16), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="HDL Cholesterol [mmol/L]")

blood.p3 <-ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Creatinine), binwidth=6)+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size=16), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="Creatinine [micromol/L]")

blood.p4 <- ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Glucose), binwidth=0.2)+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size=16), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="Glucose [mmol/L]")

grid.arrange(blood.p2, blood.p1, blood.p3, blood.p4, nrow=2, top=textGrob("Blood samples", gp=gpar(fontsize=16,font=8)))
dev.copy(pdf,'~/figures/EDA/BloodSamples/continuous.pdf') # Save the plot
dev.off()
### Comment: see outliers in creatinine, and that there are no registered values for 
### 12,22,32, 42, 52,62,72,83,93,103, 113
### seCreaCorr is calculated, something to do with this?
### Very long tails in blood glucose
### both Cholesterols slight heavier right tail

# Look into creatinine
ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Creatinine), binwidth=1)+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="#Participants", x="Creatinine [micromol/L]")+
  coord_cartesian(ylim = c(0,10))
dev.copy(pdf,'~/figures/EDA/BloodSamples/CreatinineZoom.pdf') # Save the plot
dev.off()
sum(df.total$SeCreaCorr==22)
## Low creatinine means good kidney function, high means bad
## according to wikipedia referance values are 45-90 women and 60-110 mens

## Correlation categorical variable 
ggplot(data=df.total)+
  geom_boxplot(mapping = aes(x=GFR2,y=SystolicBP3))+
  theme(axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size=24), 
        axis.text = element_text(size=16))+
  labs(y="Systolic blood pressure [mmHg]", x="GFR")
dev.copy(pdf,'~/figures/EDA/BloodSamples/CorrContGFR.pdf') # Save the plot
dev.off()
### Comment: Blood pressure increases as the stages increases (ie the gfrest decreases)
### not very big effect, but noticable

## Correlation continuous variables
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
dev.copy(pdf,'~/figures/EDA/BloodSamples/CorrContCont.pdf') # Save the plot
dev.off()
### Comment: no very strong correlations,
### some positive correlation between blood pressure and Cholesterol
### negative correlation between hdl.Cholesterol and creatinine
### very slight negative correlation between HDL.Cholesterol and blood pressure

## Correlation hyp continuous variables
blood.bc1 <- ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=Cholesterol))+
  labs(y="Cholesterol [mmol/L]",x="Hypertension")

blood.bc2 <- ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=HDL.Cholesterol))+
  labs(y="HDL Cholesterol [mmol/L]", x="Hypertension")

blood.bc3 <- ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=Creatinine))+
  labs(y=" Creatinine [micromol/L]",x="Hypertension")

blood.bc4 <- ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=Glucose))+
  labs(y="Glucose [mmol/L]",x="Hypertension")
grid.arrange(blood.bc1, blood.bc2, blood.bc3, blood.bc4, nrow=2)
dev.copy(pdf,'~/figures/EDA/BloodSamples/CorrBinCont.pdf') # Save the plot
dev.off()

### Correlation categorical hyp
count.mat.gfr <- aggregate(rep(1, length(df.total$SystolicHyp)),by=list(df.total$GFR2, df.total$SystolicHyp), FUN=sum)
count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 1"] <- count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 1"]*100/sum(count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 1"])
count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 2"] <- count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 2"]*100/sum(count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 2"])
count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 3"] <- count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 3"]*100/sum(count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 3"])
count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 4"] <- count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 4"]*100/sum(count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 4"])
count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 5"] <- count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 5"]*100/sum(count.mat.gfr$x[count.mat.gfr$Group.1=="Stage 5"])
count.mat.gfr

ggplot(data=count.mat.gfr)+
  geom_tile(mapping = aes(x=Group.1,y=Group.2,fill=x))+
  scale_fill_gradient2(limit = c(0,100), space = "Lab", name="Percentage") +
  geom_text(aes(x=Group.1, y=Group.2,label = round(x,2)), color = "black", size = 4)+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  xlab("GFR") + ylab("Hypertension")
dev.copy(pdf,'~/figures/EDA/BloodSamples/CorrBinGFRRelPerc.pdf') # Save the plot
dev.off()




###### Check overlap systolic hypertension and general hypertension


hypertensive <- df.total$SystolicBP3>=140|df.total$DiastolicBP3>=90|df.total$BPMed3
sum(hypertensive)
hyper.not.sys <- hypertensive & df.total$SystolicBP3<140
sum(hyper.not.sys)
# 534 of 4199 hypertensives are not systolic hypertensive
# ie. approx 13 %
sum(hyper.not.sys)/sum(hypertensive)




#############################

save(df.total, file="MyData/EDA.df.total.RData")

