library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(DataExplorer)
library(gridExtra)
library(reshape2)




# Sources
# https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619 Data explorer
# https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/
# https://r4ds.had.co.nz/exploratory-data-analysis.html 
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization 

# Load data set from DataCleaning
source("R code/DataCleaning.R")

########################## Overview ####################3


### ADD BINARY HYPERTENSION VARIABLE
df.total$SystolicHyp <- df.total$SystolicBP3>140


introduce(df.total)
describe(df.total)
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
plot_bar(df.total)  
## Comment: most levels are well represented, excpect stage 3,4 and 5 in GFRest


# Correlation of the all variables with the systolic bp from hunt3
plot_correlation(df.total[,-c(1,19,20,21)], title="Correlation of responses and explanatory variables")
dev.copy(png,'~/figures/EDA/TotalCorrelation.png') # Save the plot
dev.off()
# appendix


cor.mat <- round(cor(df.total[,-c(1,3,7,8,9,10,14,16,19,20,21)]),2)
ggplot(data=melt(cor.mat))+
  geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  ggtitle("Correlation of continuous response and explanatory variables ")
dev.copy(png,'~/figures/EDA/ContinuousCorrelation.png') # Save the plot
dev.off()
### Comment: Negative correlation Birthyear and cholestrol (ie. cholestrol goes up with age)
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
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Systolic")
res.p2 <- ggplot(data=df.total)+
  geom_histogram(mapping = aes(DiastolicBP3), binwidth=1)+
  geom_vline(xintercept = 90, color="red")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Diastolic")
grid.arrange(res.p1,res.p2,nrow=1, left="#Participants", top="Blood pressure from HUNT3")
dev.copy(png,'~/figures/EDA/SysDia3.png') # Save the plot
dev.off()
### Comment: many people are hypertensive, see most have problem with systolic
### spike 121 and 122 certain values for systolic 





## BINARY HYPERTENSION VARIABLE

# only binary variable for systolic hypertension, since only look at systolic bp as response
sum(df.total$SystolicHyp)/length(df.total$SystolicBP3)

ggplot(data=df.total)+
  geom_bar(aes(SystolicHyp))+
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 16))+
  labs(y="#Participants")+
  ggtitle("Systolic hypertension")
dev.copy(png,'~/figures/EDA/SysHyp.png') # Save the plot
dev.off()
### Comment: approximately 19% is hypertensive 
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
  geom_vline(xintercept = 140)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Uncorrected")
sys.med.p2 <-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=SystolicBP3, fill=BPMed3), binwidth=1)+
  geom_vline(xintercept = 140)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Corrected")
grid.arrange(sys.med.p1,sys.med.p2,nrow=1, left="#Participants", top="Systolic blood pressure from HUNT3")
dev.copy(png,'~/figures/EDA/SysCorrection3.png') # Save the plot
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
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Corrected")
dias.med.p2 <- ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=DiastolicBP3.uncorr, fill=BPMed3), binwidth=1)+ 
  geom_vline(xintercept = 90)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Uncorrected")
grid.arrange(dias.med.p2,dias.med.p1,nrow=1, left="#Participants", top="Diastolic blood pressure from HUNT3")
dev.copy(png,'~/figures/EDA/DiaCorrection3.png') # Save the plot
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
  geom_vline(xintercept = mean(df.total$SystolicBP3))+
  geom_vline(xintercept = mean(df.total$SystolicBP3[df.total$Diabetes3]), color="blue")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Diabetes")

ill.p2 <-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=SystolicBP3, fill=CVD3), binwidth = 1)+
  geom_vline(xintercept = mean(df.total$SystolicBP3))+
  geom_vline(xintercept = mean(df.total$SystolicBP3[df.total$CVD3]), color="blue")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("CVD")

ill.p3<-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=SystolicBP3, fill=BPMed3), binwidth = 1)+
  geom_vline(xintercept = mean(df.total$SystolicBP3))+
  geom_vline(xintercept = mean(df.total$SystolicBP3[df.total$BPMed3]), color="blue")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Blood pressure medication")
# use corrected version of blood pressure here
grid.arrange(ill.p1,ill.p2, ill.p3,nrow=1, left="#Participants", top="Systolic blood pressure from HUNT3")
dev.copy(png,'~/figures/EDA/SysEval3.png') # Save the plot
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

# DataExplorer
# plot_histogram(df.basic)
# plot_bar(df.basic)
# plot_boxplot(df.basic, by="Sex")
# plot_boxplot(data.frame("Sex"=df$Sex,"SystolicBP3"=df$SystolicBP3), by="Sex")
# plot_correlation(data.frame(df.basic, df$SystolicBP3))
# 


####### Inspect variables ---------------------------------------------
## Continuous variable
ggplot(data=df.basic)+
  geom_histogram(mapping = aes(x=Birthyear), binwidth=1)+
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 14))+
  labs(y="#Participants")+
  ggtitle("Birthyear")
dev.copy(png,'~/figures/EDA/BasicInfo/Birthyear.png') # Save the plot
dev.off()
### Comment: see cut-off value, since have to be 20 years to enter study
### mean birthyear=1953.74, ie. mean age HUNT2 was approx 43, mean age HUNT3 was approx. 54
### a few prticipants born before 1920, earliest in 1910


## Categorical variable
ggplot(data=df.basic)+
  geom_bar(mapping = aes(x=Sex))+
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 14))+
  labs(y="#Participants")+
  ggtitle("Sex")
dev.copy(png,'~/figures/EDA/BasicInfo/Sex.png') # Save the plot
dev.off()
### Comment: significantly more females in the study.
### Check if this is the case in these age groups in the population in general

###### Correlation categorical variables -------------------------------------------
## Correlation BP Categorical variable
ggplot(data=df.basic.res)+
  geom_boxplot(mapping = aes(x=Sex,y=SystolicBP3))+
  labs(y="Systolic blood pressure")+
  ggtitle("Sex vs. systolic blood pressure from HUNT3")
dev.copy(png,'~/figures/EDA/BasicInfo/SexVSBP.png') # Save the plot
dev.off()
### Comment: males have higher median bp, but highest bp belongs to a woman, and lowest bp to man

## Correlation Hyp categorical variable
# ggplot(data=df.basic.res)+
#   geom_bar(mapping = aes(x=Sex,fill=SystolicHyp))+
#   ggtitle("Sex vs.Hypertension")
# dev.copy(png,'~/figures/EDA/CorrBinSex.png') # Save the plot
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
# dev.copy(png,'~/figures/EDA/BasicInfo/CorrBinSexTotalPerc.png') # Save the plot
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
  ggtitle("Sex vs. hypertension at HUNT3")+
  xlab("Sex") + ylab("Hypertension")
dev.copy(png,'~/figures/EDA/BasicInfo/CorrBinSexRelPerc.png') # Save the plot
dev.off()


###### Correlation continuous variables ----------------------------------
# Correlation bp continuous variable
cat("Correlation Birthyear and SystolicBP3: ",cor(df.total$BirthYear, df.total$SystolicBP3), sep=" ")
ggplot(data=df.basic.res)+
  geom_point(mapping = aes(x=Birthyear, y=SystolicBP3), alpha=1/7)+
  labs(y="Systolic blood pressure")+
  ggtitle("Birthyear vs. systolic blood pressure from HUNT3")
dev.copy(png,'~/figures/EDA/BasicInfo/CorrContBirthyear.png') # Save the plot
dev.off()
### Comment: See both from calculation and plot that there is a negative correlation between birthyear and bp
### but also that there are som outliers that doesn't follow this trend

# Correlation hyp continuous variable
ggplot(data=df.basic.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=Birthyear))+
  labs(x="Systolic hypertension")+
  ggtitle("Birthyear vs. hypertension at HUNT3")
dev.copy(png,'~/figures/EDA/BasicInfo/CorrBinBirthyear.png') # Save the plot
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
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Systolic")
bp.p2 <- ggplot(data=df.bp)+
  geom_histogram(mapping = aes(x=DiastolicBP2), binwidth=1)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Diastolic")
grid.arrange(bp.p2, bp.p1, nrow=1, left="#Participants", top="Blood pressure from HUNT2")
dev.copy(png,'~/figures/EDA/BloodPressure/CorrBinBirthyear.png') # Save the plot
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
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 12))+
  labs(y="#Participants")+
  ggtitle("Hypertensive parents")
dev.copy(png,'~/figures/EDA/BloodPressure/HypertensiveParents.png') # Save the plot
dev.off()
### Comment: almost 50% as many people who have hypertensive parents as who haven't got hypertensive parents

# Correlation categorical variables
ggplot(data=df.bp.res)+
  geom_boxplot(mapping = aes(x=BPHigPar,y=SystolicBP3))+
  labs(x="Hypertensive parents",y="Systolic blood pressure")+
  ggtitle("Hypertensive parents vs. SystolicBP3")
dev.copy(png,'~/figures/EDA/BloodPressure/HypertensiveParentsVSBP.png') # Save the plot
dev.off()
### Comment: very slighty higher median bp for people with hypertenive parents
### smaller effect than we thought?

# Correlation BP continuous variables
cor.mat.bp <- round(cor(data.frame("SystolicBP2"=df.total$SystolicBP2,"DiastolicBP2"=df.total$DiastolicBP2,"SystolicBP3"=df.total$SystolicBP3,"DiastolicBP3"=df.total$DiastolicBP3)),2)
ggplot(data=melt(cor.mat.bp))+
      geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  ggtitle("Correlation of continous BP variables")
dev.copy(png,'~/figures/EDA/BloodPressure/CorrContBP.png') # Save the plot
dev.off()
### Comment: see that positive correlations between all (as expected)
### stronger correlations between dias and sys in same survey

# Correlation hyp continuous variable
bp.bc1 <-ggplot(data=df.bp.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=SystolicBP2))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Systolic")
bp.bc2 <-ggplot(data=df.bp.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=DiastolicBP2))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  ggtitle("Diastolic")
grid.arrange(bp.bc1, bp.bc2, nrow=1, left="Blood pressure from HUNT2",bottom="Systolic hypertension HUNT3")
dev.copy(png,'~/figures/EDA/BloodPressure/CorrBinBP.png') # Save the plot
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
  geom_text(aes(x=Group.1, y=Group.2,label = round(x,2)), color = "black", size = 4)+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  ggtitle("Hypertensive vs. hypertension at HUNT3")+
  xlab("Hypertensive parents") + ylab("Hypertension")
dev.copy(png,'~/figures/EDA/BloodPressure/CorrBinBPHigParRelPerc.png') # Save the plot
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
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 12))+
  labs(y="#Participants")+
  ggtitle("BMI from HUNT2")
dev.copy(png,'~/figures/EDA/Life/BMI.png') # Save the plot
dev.off()
### Comment: long right tail, not symmetric, big variation
### mean 25.31 


# Categorical variables
life.p2 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=PAI))+
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank())+
  coord_cartesian(ylim = c(0,9000))+
  ggtitle("PAI")

life.p3 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=RecPA))+
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank())+
  coord_cartesian(ylim = c(0,9000))+
  ggtitle("RecPA")
grid.arrange(life.p2, life.p3, nrow=1, left="#Participants")
dev.copy(png,'~/figures/EDA/Life/PhysicalActivity.png') # Save the plot
dev.off()


#### fikse slik at nivåene på smoking blir riktig, prøv å slette alt og kjøre alt på nytt 
life.p1 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=Smoking))+
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
  coord_cartesian(ylim = c(0,9000))+
  ggtitle("Smoking")

life.p4 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=Education))+
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
        coord_cartesian(ylim = c(0,9000))+
  ggtitle("Education")
grid.arrange(life.p1, life.p4, nrow=1, left="#Participants")
dev.copy(png,'~/figures/EDA/Life/SmokEduc.png') # Save the plot
dev.off()

### Comment: all levels of these are well-represented


## Correlation bp continuous variables
cat("Correlation BMI and SystolicBP3: ",cor(df.total$BMI, df.total$SystolicBP3))
ggplot(data=df.life.res)+
  geom_point(mapping = aes(x=BMI, y=SystolicBP3), alpha=1/7)+
  labs(y="Systolic blood pressure")+
  ggtitle("BMI vs. systolic blood pressure from HUNT3")
dev.copy(png,'~/figures/EDA/Life/CorrContBMIvsBP.png') # Save the plot
dev.off()

### Comment: from both calculation and plot, we see a slight positive correlation
### not as big as might expect, biggest BMI not necesarrily highest bp
### people with highest bp is within 20-30 (healthy and overweight range)

## Correlation hyp continuous variables
ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=BMI))+
  labs(x="Systolic hypertension")+
  ggtitle("BMI versus systolic hypertension from HUNT3")
dev.copy(png,'~/figures/EDA/Life/CorrBinBMIvsHyp.png') # Save the plot
dev.off()

## Correlation categorical variables
life.c1 <-ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Smoking,y=SystolicBP3))+
  theme(axis.title.x = element_blank())+
  labs(y="Systolic blood pressure")+
  ggtitle("Smoking vs. SystolicBP3")

life.c2 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=PAI,y=SystolicBP3))+
  theme(axis.title.x = element_blank())+
  labs(y="Systolic blood pressure")+
  ggtitle("PAI vs. SystolicBP3")

life.c3 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=RecPA,y=SystolicBP3))+
  theme(axis.title.x = element_blank())+
  labs(y="Systolic blood pressure")+
  ggtitle("RecPA vs. SystolicBP3")

life.c4 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Education,y=SystolicBP3))+
  theme(axis.title.x = element_blank())+
  labs(y="Systolic blood pressure")+
  ggtitle("Education vs. SystolicBP3")

grid.arrange(life.c2, life.c1, life.c3,life.c4, nrow=2)
dev.copy(png,'~/figures/EDA/Life/CorrContCat.png') # Save the plot
dev.off()
### Comment: not very big effect of any of these
### see some negative correlation between bp and physical activity 
### somewhat higher for previous daily smoker (but that might be due to age)
### might be same reason for pattern in education, seems like it


# check the negative correlation with physical activity
mean(df.total$SystolicBP3[df.total$RecPA])
mean(df.total$SystolicBP3[!df.total$RecPA])

mean(df.total$SystolicBP3[df.total$PAI=="High"])
mean(df.total$SystolicBP3[df.total$PAI=="Low"])

# Check correlation smoking and birthyear
ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Smoking,y=df.total$BirthYear))+
  labs(y="Birthyear")+
  ggtitle("Smoking vs. birthyear")
dev.copy(png,'~/figures/EDA/Life/SmokingVSBirthyear.png') # Save the plot
dev.off()

# Check correlation education and birthyear
ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Education,y=df.total$BirthYear))+
  labs(y="Birthyear")+
  ggtitle("Education vs. birthyear")
dev.copy(png,'~/figures/EDA/Life/EducationVSBirthyear.png') # Save the plot
dev.off()

####################################### FIKSE ALT UNDER DETTE 

## Correlation Hyp categorical variable
life.bc1 <-ggplot(data=df.life.res)+
  geom_bar(mapping = aes(x=Smoking,fill=SystolicHyp))+
  ggtitle("Hypertensive parents vs. Systolic hypertension ")
life.bc2 <-ggplot(data=df.life.res)+
  geom_bar(mapping = aes(x=PAI,fill=SystolicHyp))+
  ggtitle("Hypertensive parents vs. Systolic hypertension ")
life.bc3 <-ggplot(data=df.life.res)+
  geom_bar(mapping = aes(x=RecPA,fill=SystolicHyp))+
  ggtitle("Hypertensive parents vs. Systolic hypertension ")
life.bc4 <-ggplot(data=df.life.res)+
  geom_bar(mapping = aes(x=Education,fill=SystolicHyp))+
  ggtitle("Hypertensive parents vs. Systolic hypertension ")
grid.arrange(life.bc1,life.bc2,life.bc3,life.bc4, nrow=2)
dev.copy(png,'~/figures/EDA/Life/CorrBinCat.png') # Save the plot
dev.off()

### Comment: need to find some other way to display this

################# BLOOD SAMPLES ###############
# grfe, creatinine, chol, hdl chol, blood glucose

df.blood <- data.frame("GFRestStag"=df.total$GFRestStag, "Creatinine"=df.total$SeCreaCorr, "Cholestrol"=df.total$SeChol, 
                       "HDL.Cholestrol"=df.total$SeHDLChol, "Blood.Glucose"=df.total$SeGluNonFast)
df.blood.res <- data.frame("GFRestStag"=df.total$GFRestStag, "Creatinine"=df.total$SeCreaCorr, "Cholestrol"=df.total$SeChol, 
                           "HDL.Cholestrol"=df.total$SeHDLChol, "Blood.Glucose"=df.total$SeGluNonFast,
                           "SystolicBP3"=df.total$SystolicBP3, "SystolicHyp"=df.total$SystolicHyp)

plot_histogram(df.blood)

## Categorical variabel
ggplot(data=df.blood)+
  geom_bar(mapping = aes(x=GFRestStag))+
  ggtitle("GFRestStag")+
  coord_flip()
### Comment: see from plot that very few in stage 3, only one in 4, and no one in stage 5
### GFR (glomerular filtration rate) how much blood passes through the glomeruli each minute
### more is better, below 60 might mean kidney disease

cat("Participants in Stage 3: ", sum(df.blood$GFRestStag=="Stage 3: GFREst 30-59 ml/min"))
cat("Participants in Stage 4: ", sum(df.blood$GFRestStag=="Stage 4: GFREst 15-29 ml/min"))
cat("Participants in Stage 5: ",sum(df.blood$GFRestStag=="Stage 5: GFREst  < 15 ml/min"))



## Continuous variables
blood.p1 <-ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Cholestrol), binwidth=0.2)+
  ggtitle("Cholestrol")

blood.p2 <- ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=HDL.Cholestrol), binwidth=0.2)+
  ggtitle("HDL.Cholestrol")

blood.p3 <-ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Creatinine), binwidth=1)+
  ggtitle("Creatinine")

blood.p4 <- ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Blood.Glucose), binwidth=0.2)+
  ggtitle("Blood.Glucose")

grid.arrange(blood.p2, blood.p1, blood.p3, blood.p4, nrow=2)
### Comment: see outliers in creatinine, and that there are no registered values for 
### 12,22,32, 42, 52,62,72,83,93,103, 113
### seCreaCorr is calculated, something to do with this?
### Very long tails in blood glucose
### both cholestrols slight heavier right tail

# Look into creatinine
ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Creatinine), binwidth=1)+
  ggtitle("Creatinine")+
  coord_cartesian(ylim = c(0,10))
sum(df.total$SeCreaCorr==22)
## Low creatinine means good kidney function, high means bad
## according to wikipedia referance values are 45-90 women and 60-110 mens

## Correlation categorical variable 
ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=GFRestStag,y=SystolicBP3))+
  ggtitle("GFRestStag vs. BP")+ coord_flip()
### Comment: Blood pressure increases as the stages increases (ie the gfrest decreases)
### not very big effect, but noticable

## Correlation continuous variables
cor.mat.blood <- round(cor(df.blood.res[,-1]),2)
ggplot(data=melt(cor.mat.blood))+
  geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  ggtitle("Correlation blood samples and SystolicBP3")
### Comment: no very strong correlations,
### some positive correlation between blood pressure and cholestrol
### negative correlation between hdl.cholestrol and creatinine
### very slight negative correlation between HDL.cholestrol and blood pressure

## Correlation hyp continuous variables
blood.bc1 <- ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=Cholestrol))+
  ggtitle("Cholestrol")

blood.bc2 <- ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=HDL.Cholestrol))+
  ggtitle("HDL Cholestrol")

blood.bc3 <- ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=Creatinine))+
  ggtitle("Creatinine")

blood.bc4 <- ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=SystolicHyp,y=Blood.Glucose))+
  ggtitle("Blood glucose")
grid.arrange(blood.bc1, blood.bc2, blood.bc3, blood.bc4, nrow=2)










