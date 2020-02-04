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

# Data frame with all relevant information
df.total <- data.frame(df, df.eval)

introduce(df)
describe(df)
# gmd= Gini mean distance, mean absolute difference between any pair of observations
### Comment: doesn't seem to be any single extreme outliers
### Lower minimum values for BP at HUNT3 than HUNT2
### checked and some people were removed (originally HUNT2 min was 70 and 30),
### which is still higher than (60 and 26) HUNT3


# Continuous variables
plot_histogram(df)
### Comment: multiple variables have some values that are much more common than the others
### and see from width of plots that there are some outliers

# Categorical variables
plot_bar(df)  
## Comment: most levels are well represented, excpect stage 3,4 and 5 in GFRest


# Correlation of the all variables with the systolic bp from hunt3
plot_correlation(df)
dev.copy(png,'~/figures/EDA/TotalCorrelation.png') # Save the plot
dev.off()
# appendix


cor.mat <- round(cor(df[,-c(1,3,7,8,9,10,14,16)]),2)
ggplot(data=melt(cor.mat))+
  geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  ggtitle("Total correlation continuous")
### Comment: Negative correlation Birthyear and cholestrol (ie. cholestrol goes up with age)
### biggest negative correlation with response (bpsys3) was birthyear
### biggest positive correlation with response (bpsys3) was bpsys2


########################## RESPONSE ##########################
# Uncorrect the blood pressure values
BPSys3.uncorr <- df$BPSys3
BPSys3.uncorr[df.eval$BPMed3] <- df$BPSys3[df.eval$BPMed3]-15
BPDias3.uncorr <- df$BPDias3
BPDias3.uncorr[df.eval$BPMed3] <- df$BPDias3[df.eval$BPMed3]-10

# Look at response
# plot_histogram(data.frame(df$BPSys3, df$BPDias3))
res.p1 <- ggplot(data=df)+
  geom_histogram(mapping = aes(BPSys3), binwidth=1)+
  geom_vline(xintercept = 140, color="red")+
  ggtitle("Systolic bp")
res.p2 <- ggplot(data=df)+
  geom_histogram(mapping = aes(BPDias3), binwidth=1)+
  geom_vline(xintercept = 90, color="red")+
  ggtitle("Diastolic bp")
grid.arrange(res.p1,res.p2,nrow=1)
### Comment: many people are hypertensive, see most have problem with systolic
### spike 121 and 122 certain values for systolic 

##### Compare corrected and uncorrected blood pressure
#plot_histogram(data.frame(BPSys3.uncorr,df$BPSys3))
#plot_histogram(data.frame(BPDias3.uncorr,df$BPDias3))

# sys.p1 <- ggplot(data=df)+
#   geom_histogram(mapping = aes(BPSys3), binwidth=1)+
#   geom_vline(xintercept = 140, color="red")+
#   ggtitle("Corrected systolic")
# sys.p2 <- ggplot(data=data.frame(BPSys3.uncorr))+
#   geom_histogram(mapping = aes(BPSys3.uncorr), binwidth=1)+
#   geom_vline(xintercept = 140,color= "red")+
#   ggtitle("Uncorrected systolic")
# grid.arrange(sys.p1,sys.p2,nrow=1)
# ### Comment: very small difference, since only 6.7% use bp medication
# 
# sum(df.total$BPMed3)/length(df.total$BPSys3)
# 
# dias.p1 <- ggplot(data=df)+
#   geom_histogram(mapping = aes(BPDias3), binwidth=1)+
#   geom_vline(xintercept = 90, color="red")+
#   ggtitle("Corrected diastolic")
# dias.p2 <- ggplot(data=data.frame(BPDias3.uncorr))+
#   geom_histogram(mapping = aes(BPDias3.uncorr), binwidth=1)+
#   geom_vline(xintercept = 90,color= "red")+
#   ggtitle("Uncorrected diastolic")
# grid.arrange(dias.p1,dias.p2,nrow=1)
# ### Comment: very small difference, since only 6.7% use bp medication


### Compare people on/off bp medication before/after correction
## Systolic
sys.med.p1 <-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPSys3.uncorr, fill=BPMed3), binwidth = 1)+
  geom_vline(xintercept = 140)+
  ggtitle("Uncorrected systolic bp")
sys.med.p2 <-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPSys3, fill=BPMed3), binwidth=1)+
  geom_vline(xintercept = 140)+
  ggtitle("Corrected systolic bp")
grid.arrange(sys.med.p1,sys.med.p2,nrow=1)
### Comment: Slightly heavier right tail for corrected than uncorrected, 
### small difference since so few on bpmed
### but in both cases heavier right tail than left tail
### some spikes for 121,122

## Diastolic
dias.med.p1 <- ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPDias3, fill=BPMed3), binwidth = 1)+
  geom_vline(xintercept = 90)+
  ggtitle("Corrected diastolic bp")
dias.med.p2 <- ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPDias3.uncorr, fill=BPMed3), binwidth=1)+ 
  geom_vline(xintercept = 90)+
  ggtitle("Uncorrected diastolic bp")
grid.arrange(dias.med.p2,dias.med.p1,nrow=1)
### Comment:  very slightly heavier right tail for corrected than uncorrected,


# # scatterplot comparing systolic bp of people using vs not using bp medication, after corrected
# ggplot(data=df.total)+
#   geom_point(mapping = aes(x=1:length(BPSys3), y=BPSys3))+
#   facet_wrap(~BPMed3)+ geom_hline(yintercept = 140, color="red", size=2)+ 
#   ggtitle("Corrected blood pressure")
# 
# # scatterplot comparing systolic bp of people using vs not using bp medication, not corrected
# ggplot(data=df.total)+
#   geom_point(mapping = aes(x=1:length(BPSys3), y=BPSys3.uncorr))+
#   facet_wrap(~BPMed3)+geom_hline(yintercept = 140, color="red", size=2)+
#   ggtitle("Uncorrected blood pressure")


########### BP for subgroups ##################3
ill.p1<-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPSys3, fill= Diabetes3), binwidth = 1)+
  geom_vline(xintercept = mean(df$BPSys3))+
  geom_vline(xintercept = mean(df$BPSys3[df.total$Diabetes3]), color="blue")+
  ggtitle("BPSys for diabetes")

ill.p2 <-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPSys3, fill=CVD3), binwidth = 1)+
  geom_vline(xintercept = mean(df$BPSys3))+
  geom_vline(xintercept = mean(df$BPSys3[df.total$CVD3]), color="blue")+
  ggtitle("BPSys for CVD")

ill.p3<-ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPSys3, fill=BPMed3), binwidth = 1)+
  geom_vline(xintercept = mean(df$BPSys3))+
  geom_vline(xintercept = mean(df$BPSys3[df.total$BPMed3]), color="blue")+
  ggtitle("BPSys for BPMed")
# use corrected version of blood pressure here
grid.arrange(ill.p1,ill.p2, ill.p3,nrow=1)
### Comment: group with diabetes and group with cvd have higher mean bp than total group
## seems that group with diabetes and group with cvd have similar mean
## highest mean for the corrected bp of group on bp medication (no surprise)

mean(df$BPSys3[df.total$Diabetes3])
mean(df$BPSys3[df.total$CVD3])
### very similar means


##################### EXPLANATORY VARIABLES #################
# check that all are in the format they should be (no factor to numerical)
# check for outliers
# check distribution

# boxplot of continuous variables against response
plot_boxplot(df,  by="BPSys3")
# not sure if need this

################# BASIC INFO ###############
# sex, age
df.basic <- data.frame("Sex"=df$Sex, "Birthyear"=df$BirthYear)
df.basic.res <- data.frame("Sex"=df$Sex, "Birthyear"=df$BirthYear, "BPSys3"=df$BPSys3)

# DataExplorer
# plot_histogram(df.basic)
# plot_bar(df.basic)
# plot_boxplot(df.basic, by="Sex")
# plot_boxplot(data.frame("Sex"=df$Sex,"BPSys3"=df$BPSys3), by="Sex")
# plot_correlation(data.frame(df.basic, df$BPSys3))
# 

## Continuous variable
ggplot(data=df.basic)+
  geom_histogram(mapping = aes(x=Birthyear), binwidth=1)+
  ggtitle("Birthyear")
### Comment: see cut-off value, since have to be 20 years to enter study
### mean birthyear=1953.74, ie. mean age HUNT2 was approx 43, mean age HUNT3 was approx. 54
### a few prticipants born before 1920, earliest in 1910



## Categorical variable
ggplot(data=df.basic)+
  geom_bar(mapping = aes(x=Sex))+
  ggtitle("Sex")
### Comment: significantly more females in the study

## Correlation Categorical variable
ggplot(data=df.basic.res)+
  geom_boxplot(mapping = aes(x=Sex,y=BPSys3))+
  ggtitle("Sex vs. BP")
### Comment: males have higher median bp, but highest bp belongs to a woman, and lowest bp to man

# Correlation continuous variable
cat("Correlation Birthyear and BPSys3: ",cor(df$BirthYear, df$BPSys3), sep=" ")
ggplot(data=df.basic.res)+
  geom_point(mapping = aes(x=Birthyear, y=BPSys3), alpha=1/10)+
  ggtitle("Birthyear vs. BPSys3")
### Comment: See both from calculation and plot that there is a negative correlation between birthyear and bp
### but also that there are som outliers that doesn't follow this trend

################# BLOOD PRESSURE ###############
# systlic and diastolic bp at hunt2, hypertensive parents
df.bp <- data.frame("BPSys2"=df$BPSys2, "BPDias2"=df$BPDias2, "BPHigPar"=df$BPHigPar)
df.bp.res <- data.frame("BPSys2"=df$BPSys2, "BPDias2"=df$BPDias2, "BPHigPar"=df$BPHigPar, "BPSys3"=df$BPSys3)

# Data explorer
plot_histogram(df.bp)

## Continuous variables
bp.p1 <-ggplot(data=df.bp)+
  geom_histogram(mapping = aes(x=BPSys2), binwidth=1)+
  ggtitle("Systolic")
bp.p2 <- ggplot(data=df.bp)+
  geom_histogram(mapping = aes(x=BPDias2), binwidth=1)+
  ggtitle("Diastolic")
grid.arrange(bp.p2, bp.p1, nrow=1)
### Comment: see cut-off value for both, seems escpecially abrupt for systolic
### might be because diastolic decreases with age

cat("The mean sys bp at HUNT2 was ", mean(df$BPSys2), "and at HUNT3 it was ", mean(df$BPSys3))
cat("The mean dias bp at HUNT2 was ", mean(df$BPDias2), "and at HUNT3 it was ", mean(df$BPDias3))
### Comment: increasing mean bp for sys (due to including hypertensice and ill?)
### decreasing mean for dias, due to age


## Categorical variables
ggplot(data=df.bp)+
  geom_bar(mapping = aes(x=BPHigPar))+
  ggtitle("Hypertensive parents")
### Comment: almost 50% as many people who have hypertensive parents as who haven't got hypertensive parents

# Correlation categorical variables
ggplot(data=df.bp.res)+
  geom_boxplot(mapping = aes(x=BPHigPar,y=BPSys3))+
  ggtitle("Hypertensive parents vs. BP")
### Comment: very slighty higher median bp for people with hypertenive parents
### smaller effect than we thought?

# Correlation continuous variables
cor.mat.bp <- round(cor(data.frame("BPSys2"=df$BPSys2,"BPDias2"=df$BPDias2,"BPSys3"=df$BPSys3,"BPDias3"=df$BPDias3)),2)
ggplot(data=melt(cor.mat.bp))+
      geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y=element_text(vjust = 1, size = 12, hjust = 1))+
  ggtitle("Correlation blood pressure")
### Comment: see that positive correlations between all (as expected)
### stronger correlations between dias and sys in same survey
  

################## LIFESTYLE ###############

# bmi, smoking, pai, recpa, educational level
df.life <- data.frame("BMI"=df$BMI, "Smoking"=df$SmoStat, "PAI"=df$PAI, "RecPA"=df$RecPA, "Education"=df$Educ)
df.life.res <- data.frame("BMI"=df$BMI, "Smoking"=df$SmoStat, "PAI"=df$PAI, "RecPA"=df$RecPA,
                          "Education"=df$Educ, "BPSys3"=df$BPSys3)
# Continuous variables
ggplot(data=df.life)+
  geom_histogram(mapping = aes(x=BMI), binwidth=1)+
  ggtitle("BMI")
### Comment: long right tail, not symmetric, big variation
### mean 25.31 


# Categorical variables
life.p1 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=Smoking))+
  ggtitle("Smoking")

life.p2 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=PAI))+
  ggtitle("PAI")

life.p3 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=RecPA))+
  ggtitle("RecPA")

life.p4 <-ggplot(data=df.life)+
  geom_bar(mapping = aes(x=Education))+
  ggtitle("Education")

grid.arrange(life.p2, life.p1, life.p3,life.p4, nrow=2)
### Comment: all levels of these are well-represented


## Correlation continuous variables
cat("Correlation BMI and BPSys3: ",cor(df$BMI, df$BPSys3))
ggplot(data=df.life.res)+
  geom_point(mapping = aes(x=BMI, y=BPSys3), alpha=1/10)+
  ggtitle("BMI vs. BPSys3")
### Comment: from both calculation and plot, we see a slight positive correlation
### not as big as might expect, biggest BMI not necesarrily highest bp
### people with highest bp is within 20-30 (healthy and overweight range)

## Correlation categorical variables
life.c1 <-ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Smoking,y=BPSys3))+
  ggtitle("Smoking vs. BP")

life.c2 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=PAI,y=BPSys3))+
  ggtitle("PAI vs. BP")

life.c3 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=RecPA,y=BPSys3))+
  ggtitle("RecPA vs. BP")

life.c4 <- ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Education,y=BPSys3))+
  ggtitle("Education vs. BP")

grid.arrange(life.c2, life.c1, life.c3,life.c4, nrow=2)
### Comment: not very big effect of any of these
### see some negative correlation between bp and physical activity 
### somewhat higher for previous daily smoker (but that might be due to age)
### might be same reason for pattern in education, seems like it

# check the negative correlation with physical activity
mean(df$BPSys3[df$RecPA])
mean(df$BPSys3[!df$RecPA])

mean(df$BPSys3[df$PAI=="High"])
mean(df$BPSys3[df$PAI=="Low"])

# Check correlation smoking and birthyear
ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Smoking,y=df$BirthYear))+
  ggtitle("Smoking vs. Birthyear")

# Check correlation education and birthyear
ggplot(data=df.life.res)+
  geom_boxplot(mapping = aes(x=Education,y=df$BirthYear))+
  ggtitle("Education vs. Birthyear")

################# BLOOD SAMPLES ###############
# grfe, creatinine, chol, hdl chol, blood glucose

df.blood <- data.frame("GFRestStag"=df$GFRestStag, "Creatinine"=df$SeCreaCorr, "Cholestrol"=df$SeChol, 
                       "HDL.Cholestrol"=df$SeHDLChol, "Blood.Glucose"=df$SeGluNonFast)
df.blood.res <- data.frame("GFRestStag"=df$GFRestStag, "Creatinine"=df$SeCreaCorr, "Cholestrol"=df$SeChol, 
                           "HDL.Cholestrol"=df$SeHDLChol, "Blood.Glucose"=df$SeGluNonFast,"BPSys3"=df$BPSys3)

plot_histogram(df.blood)

## Categorical variabel
ggplot(data=df.blood)+
  geom_bar(mapping = aes(x=GFRestStag))+
  ggtitle("GFRestStag")+
  coord_flip()
### Comment: see from plot that very few in stage 3, only one in 4, and no one in stage 5

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
sum(df$SeCreaCorr==22)


## Correlation categorical variable 
ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=GFRestStag,y=BPSys3))+
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
  ggtitle("Correlation blood samples and BPSys3")
### Comment: no very strong correlations,
### some positive correlation between blood pressure and cholestrol
### negative correlation between hdl.cholestrol and creatinine
### very slight negative correlation between HDL.cholestrol and blood pressure





#############################################################################








