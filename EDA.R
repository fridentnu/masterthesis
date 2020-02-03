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


# Load data set from DataCleaning
source("R code/DataCleaning.R")

########################## Overview ####################3

# Data frame with all relevant information
df.total <- data.frame(df, df.eval)

introduce(df)
describe(df)
# gmd= Gini mean distance, mean absolute difference between any pair of observations

# Continuous variables
plot_histogram(df)
plot_num(df)

# Categorical variables
plot_bar(df)
freq(df)


# Correlation of the different continuous variables with the systolic bp from hunt3
plot_correlation(df, type="continuous")

# Collection of EDA plots 
#create_report(df)



########################## RESPONSE ##########################
# Correct the blood pressure values
BPSys3.uncorr <- df$BPSys3
BPSys3.uncorr[df.eval$BPMed3] <- df$BPSys3[df.eval$BPMed3]-15
BPDias3.uncorr <- df$BPDias3
BPDias3.uncorr[df.eval$BPMed3] <- df$BPDias3[df.eval$BPMed3]-10

# Look at response
plot_histogram(data.frame(df$BPSys3, df$BPDias3))


# Compare corrected and uncorrected blood pressure
# prøve å dele opp hver bin i to farger, de som bruker og de som ikke bruker bpmed?
plot_histogram(data.frame(BPSys3.uncorr,df$BPSys3))
plot_histogram(data.frame(BPDias3.uncorr,df$BPDias3))
#### ADD VERTICAL LINE HERE at 140 and 90? 

# Corrected
ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPSys3, fill=BPMed3), binwidth=1)+
  geom_vline(xintercept = 140)+
  ggtitle("Corrected systolic bp")

# Unorrected
ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPSys3.uncorr, fill=BPMed3), binwidth = 1)+
  geom_vline(xintercept = 140)+
  ggtitle("Uncorrected systolic bp")

# for systolic bp: 
# Heavier right tail for corrected than uncorrected, 
# but in both cases heavier right tail than left tail

# Corrected
ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPDias3, fill=BPMed3), binwidth = 1)+
  geom_vline(xintercept = 90)+
  ggtitle("Corrected diastolic bp")


# Unorrected
ggplot(data=df.total)+
  geom_histogram(mapping = aes(x=BPDias3.uncorr, fill=BPMed3), binwidth=1)+ 
  geom_vline(xintercept = 90)+
  ggtitle("Uncorrected diastolic bp")

# for diastolic bp, tails seems approximately equal

# scatterplot comparing systolic bp of people using vs not using bp medication, after corrected
ggplot(data=df.total)+
  geom_point(mapping = aes(x=1:length(BPSys3), y=BPSys3))+
  facet_wrap(~BPMed3)+ geom_hline(yintercept = 140, color="red", size=2)+ 
  ggtitle("Corrected blood pressure")

# scatterplot comparing systolic bp of people using vs not using bp medication, not corrected
ggplot(data=df.total)+
  geom_point(mapping = aes(x=1:length(BPSys3), y=BPSys3.uncorr))+
  facet_wrap(~BPMed3)+geom_hline(yintercept = 140, color="red", size=2)+
  ggtitle("Uncorrected blood pressure")


##################### EXPLANATORY VARIABLES #################
# check that all are in the format they should be (no factor to numerical)
# check for outliers
# check distribution

# boxplot of continuous variables against response
plot_boxplot(f,  by="BPSys3")

################# BASIC INFO ###############
# sex, age
df.basic <- data.frame("Sex"=df$Sex, "Birthyear"=df$BirthYear)
df.basic.res <- data.frame("Sex"=df$Sex, "Birthyear"=df$BirthYear, "BPSys3"=df$BPSys3)

# DataExplorer
plot_histogram(df.basic)
plot_bar(df.basic)
plot_boxplot(df.basic, by="Sex")
plot_boxplot(data.frame("Sex"=df$Sex,"BPSys3"=df$BPSys3), by="Sex")
plot_correlation(data.frame(df.basic, df$BPSys3))


## Continuous variable
ggplot(data=df.basic)+
  geom_histogram(mapping = aes(x=Birthyear), binwidth=1)+
  ggtitle("Birthyear")
# Comment: see cut-off value, since have to be 20 years to enter study

## Categorical variable
ggplot(data=df.basic)+
  geom_bar(mapping = aes(x=Sex))+
  ggtitle("Sex")

## Correlation Categorical variable
ggplot(data=df.basic.res)+
  geom_boxplot(mapping = aes(x=Sex,y=BPSys3))+
  ggtitle("Sex vs. BP")

# Correlation continuous variable
cat("Correlation Birthyear and BPSys3: ",cor(df$BirthYear, df$BPSys3), sep=" ")

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
# Comment: see cut-off value, since have to be 20 years to enter study

## Categorical variables
ggplot(data=df.bp)+
  geom_bar(mapping = aes(x=BPHigPar))+
  ggtitle("Hypertensive parents")

# Correlation categorical variables
ggplot(data=df.bp.res)+
  geom_boxplot(mapping = aes(x=BPHigPar,y=BPSys3))+
  ggtitle("Hypertensive parents vs. BP")

# Correlation continuous variables
cor.mat.bp <- cor(df.bp.res[,-3])
ggplot(data=melt(cor.mat.bp))+
      geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))+
  ggtitle("Correlation blood pressure")
       
  

################## LIFESTYLE ###############

# bmi, smoking, pai, recpa, educational level
df.life <- data.frame("BMI"=df$BMI, "Smoking"=df$SmoStat, "PAI"=df$PAI, "RecPA"=df$RecPA, "Education"=df$Educ)
df.life.res <- data.frame("BMI"=df$BMI, "Smoking"=df$SmoStat, "PAI"=df$PAI, "RecPA"=df$RecPA,
                          "Education"=df$Educ, "BPSys3"=df$BPSys3)
# Continuous variables
ggplot(data=df.life)+
  geom_histogram(mapping = aes(x=BMI), binwidth=1)+
  ggtitle("BMI")

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

## Correlation continuous variables
cat("Correlation BMI and BPSys3: ",cor(df$BMI, df$BPSys3))

## Correlation categorical variables
life.c1 <- ggplot(data=df.life.res)+
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


## Continuous variables
blood.p1 <-ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Cholestrol), binwidth=1)+
  ggtitle("Cholestrol")

blood.p2 <- ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=HDL.Cholestrol), binwidth=1)+
  ggtitle("HDL.Cholestrol")

blood.p3 <-ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Creatinine), binwidth=1)+
  ggtitle("Creatinine")

blood.p4 <- ggplot(data=df.blood)+
  geom_histogram(mapping = aes(x=Blood.Glucose), binwidth=1)+
  ggtitle("Blood.Glucose")

grid.arrange(blood.p2, blood.p1, blood.p3, blood.p4, nrow=1)

## Correlation categorical variable 
ggplot(data=df.blood.res)+
  geom_boxplot(mapping = aes(x=GFRestStag,y=BPSys3))+
  ggtitle("GFRestStag vs. BP")+ coord_flip()


## Correlation continuous variables
cor.mat.blood <- cor(df.blood.res[,-1])
ggplot(data=melt(cor.mat.blood))+
  geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))+
  ggtitle("Correlation blood samples and BPSys3")




#############################################################################








