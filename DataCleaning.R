############ LOAD PACKAGES ################
library(foreign) 
library(tibble)  
library(DataExplorer) 
library(gridExtra)
library(tidyverse) 
library(viridis)
library(grid)


##### Some inspiration from the webpages
# https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619 Data explorer
# https://hunt-db.medisin.ntnu.no/hunt-db/#/ 



########## LOAD DATA ###################

# Data Study Cohort I (all participants from HUNT2 and/or HUNT3)
data <- read.spss("/mnt/work/Data/2019-03-26_108676_Data Study I Cohort.sav", to.data.frame=T)

###### Quick overview #######
str(data)
## DATA FORMAT: data is a data.frame of 78962 observations of 236 variables
## variables are either numeric or factors
## data set contains a lot of missing values

### First check that list of participants is unique and does not contain missing values
# No NA in project personidentification 
sum(is.na(data$PID.108676))

# Check if all are unique 
sum(!unique(data$PID.108676)==data$PID.108676)

# Look at names of the 236 variables
var.names.data <-colnames(data)




###################### SELECTING THE POPULATION ########################################

### Keep only people who participated in both HUNT2 and HUNT3
# NA in participation means not invited
indexNT23.part <- data$Part.NT2BLQ1=="Deltatt" & data$Part.NT2BLM=="Deltatt" &
  data$Part.NT2BLQ2=="Deltatt" & data$Part.NT3BLM=="Deltatt" &
  data$Part.NT3BLQ1=="Deltatt"
sum(indexNT23.part, na.rm=T)
indexNT23.miss <- is.na(data$Part.NT2BLQ1) | is.na(data$Part.NT2BLM) | 
  is.na(data$Part.NT2BLQ2) |is.na(data$Part.NT3BLM) | is.na(data$Part.NT3BLQ1)
data <- data[indexNT23.part & !indexNT23.miss,]


#### EXCLUSION CRITERIA #####

# History of CVD at time of HUNT2
# CarInfEv@NT2BLQ1 (ever had heart attack), CarAngEv@NT2BLQ1 (hjertekrampe/chest pain), ApoplEv@NT2BLQ1 (stroke)
indexNT2.CVD <- data$CarInfEv.NT2BLQ1=="Ja" | data$CarAngEv.NT2BLQ1=="Ja" | data$ApoplEv.NT2BLQ1=="Ja"


# History of diabetes, self-reported and from blood glucose level
# also want to exclude people with non fasting glucose above threshold value
# probably undiagnosed diabetes
# "Symptoms plus random blood glucose ?11.1 mmol/L (?200 mg/dL) indicate diabetes" 
# - Cosentino et al, 2019  https://doi.org/10.1093/eurheartj/ehz486
indexNT2.dia <- data$DiaEv.NT2BLQ1=="Ja" | data$SeGluNonFast.NT2BLM>=11.1

# History of hypertension at time of HUNT2
indexNT2.hyp <- data$BPSystMn23.NT2BLM>=140 | data$BPDiasMn23.NT2BLM>=90 |data$BPMedCu.NT2BLQ1=="N\xe5" | data$BPMedCu.NT2BLQ1=="F\xf8r, men ikke n\xe5"

# Percentage of hypertensives at HUNT2
sum(indexNT2.hyp, na.rm = T)/length(data$PID.108676)

# Dataframe with CVD, Hypertension and Diabetes status
df.ill <-data.frame("CVD"=indexNT2.CVD, "Hypertension"=indexNT2.hyp, "Diabetes"=indexNT2.dia)

ill.p1 <-ggplot(data=df.ill)+
  geom_bar(mapping = aes(x=CVD))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))+
  coord_flip()+
  ggtitle("CVD")
ill.p2 <-ggplot(data=df.ill)+
  geom_bar(mapping = aes(x=Diabetes))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))+
  coord_flip()+
  ggtitle("Diabetes")
ill.p3 <-ggplot(data=df.ill)+
  geom_bar(mapping = aes(x=Hypertension))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))+
  coord_flip()+
  ggtitle("Hypertension")
grid.arrange(ill.p1,ill.p2,ill.p3,nrow=3, left=textGrob("#Participants", gp=gpar(fontsize=20,font=8), rot=90))
dev.copy(pdf,'~/figures/DataCleaning/HealthStatus.pdf') 
dev.off()


# Check missing values more closely
df.health <- data.frame("BPMed2"= data$BPMedCu.NT2BLQ1, "SystolicBP2"=data$BPSystMn23.NT2BLM,
                        "DiastolicBP2"=data$BPDiasMn23.NT2BLM, "HeartAttack2"=data$CarInfEv.NT2BLQ1, 
                        "AnginaPectoris2"=data$CarAngEv.NT2BLQ1,"Stroke2"=data$ApoplEv.NT2BLQ1, 
                        "Diabetes2"=data$DiaEv.NT2BLQ1, "Glucose2"=data$SeGluNonFast.NT2BLM,
                        "SystolicBP3"=data$BPSystMn23.NT3BLM,"DiastolicBP3"=data$BPDiasMn23.NT3BLM,
                        "BPMed3"=data$BPMedEv.NT3BLQ1)
plot_missing(df.health, title="Missing values of blood pressure health")
dev.copy(pdf,'~/figures/DataCleaning/MissingValuesStep1.pdf') # Save the plot
dev.off()

#### REMOVE MISSING VALUES ######
# Missing values for cvd at HUNT2
indexNT2.miss.CVD <- is.na(data$CarInfEv.NT2BLQ1) | is.na(data$CarAngEv.NT2BLQ1) | is.na(data$ApoplEv.NT2BLQ1)

# Missing values for diabetes at HUNT2
indexNT2.miss.dia <- is.na(data$DiaEv.NT2BLQ1) | is.na(data$SeGluNonFast.NT2BLM)

# Missing values hypertension at HUNT2
indexNT2.miss.hyp <- is.na(data$BPMedCu.NT2BLQ1)|is.na(data$BPSystMn23.NT2BLM) | is.na(data$BPDiasMn23.NT2BLM)

# Missing values response HUNT3
indexNT3.miss.bp <-is.na(data$BPSystMn23.NT3BLM) | is.na(data$BPDiasMn23.NT3BLM)

# Missing values blood pressure medication HUNT3
indexNT3.miss.bpmed <- is.na(data$BPMedEv.NT3BLQ1) 

## Remove participants who meet exclusion criteria
data.1 <- data[!(indexNT2.CVD| indexNT2.dia | indexNT2.hyp|
                 indexNT2.miss.CVD | indexNT2.miss.dia |indexNT2.miss.hyp |
                 indexNT3.miss.bp|indexNT3.miss.bpmed),]

# Check that all missing values are removed
df.health <- data.frame("BPMed2"= data.1$BPMedCu.NT2BLQ1, "SystolicBPMean2"=data.1$BPSystMn23.NT2BLM,
                        "DiastolicBPMean2"=data.1$BPDiasMn23.NT2BLM, "HeartAttack2"=data.1$CarInfEv.NT2BLQ1, 
                        "AnginaPectoris2"=data.1$CarAngEv.NT2BLQ1,"Stroke2"=data.1$ApoplEv.NT2BLQ1, 
                        "Diabetes2"=data.1$DiaEv.NT2BLQ1, "Glucose2"=data.1$SeGluNonFast.NT2BLM, 
                        "SystolicBPMean3"=data.1$BPSystMn23.NT3BLM, "DiastolicBPMean3"=data.1$BPDiasMn23.NT3BLM,
                        "BPMed3"=data.1$BPMedEv.NT3BLQ1)
plot_missing(df.health)


# Now 19126 participants
introduce(data.1)

############# EXPLANATORY VARIABLES #######################
## Possible explanatory variables:
# Systolic bp at HUNT2, diastolic bp at HUNt2, hypertensive parents, age, sex, PAI,
# RecPA (above/below recommended physical activity),
# BMI, alcohol, smoking, total Cholesterol, HDL Cholesterol
# non-fasting glucose (cut-off >11.1), 
# GRFE, creatinine, educational level

# To see if any of the independent variables will reduce our data set greatly
# we study the number of missing value per variable


############### ADD PAI ##############
# categorical value of physical activity
source("R code/PAI.R")
data.1$PAI.NT2 <- PAIlevel_NT2

############### ADD RecPA ##############
# Logical indicator of whether a person reaches the recommended amount of physical activity
source("R code/MVPA.R")
data.1$RecPA.NT2 <- MeetsPARecomed_NT2


############## ADD BPHigParEv ###############
# One or both parents have hypertension
indexNT2.BPHigParEv <- data.1$BPHigFathEv.NT2BLQ2== "Far - h\xf8yt BT" | data.1$BPHigMothEv.NT2BLQ2== "Mor - h\xf8yt BT" 

# Not answered the question
indexNT2.miss.fam.bp.high <- is.na(data.1$BPHigFamNon.NT2BLQ2) & is.na(data.1$BPHigBrotEv.NT2BLQ2) & 
  is.na(data.1$BPHigFathEv.NT2BLQ2) & is.na(data.1$BPHigChiEv.NT2BLQ2) & is.na(data.1$BPHigSistEv.NT2BLQ2) & 
  is.na(data.1$BPHigMothEv.NT2BLQ2)

# Add new column with logical variable of parental history of hypertension
data.1$BPHigParEv.NT2 <- indexNT2.BPHigParEv

## Parents don't have hypertension if :
# 1.they haven't answered that parents have hypertension 
# 2. they have answered something in the question
data.1$BPHigParEv.NT2[is.na(indexNT2.BPHigParEv) & !indexNT2.miss.fam.bp.high] <-FALSE

### Alcohol
# total number of units of alcohol within last two weeks
alcohol.total.NT2 <-data.1$AlcBeL2WN.NT2BLQ1+data.1$AlcLiL2WN.NT2BLQ1+data.1$AlcWiL2WN.NT2BLQ1

# Data frame of all possible explanatory variables
df.exp.var <- data.frame("PID"=data.1$PID.108676,"BirthYear"=data.1$BirthYear, "Sex"=data.1$Sex, "BMI"=data.1$Bmi.NT2BLM, 
                               "PAI"=data.1$PAI.NT2, "BPHigParEv"=data.1$BPHigParEv.NT2,"RecPA"=data.1$RecPA.NT2, "Smoking"=data.1$SmoStat.NT2BLQ1,
                               "Cholesterol"=data.1$SeChol.NT2BLM, "HDLCholesterol"=data.1$SeHDLChol.NT2BLM, "Glucose"= data.1$SeGluNonFast.NT2BLM,
                               "GFRestStag"=data.1$GFREstStag.NT2BLM, "Creatinine"=data.1$SeCreaCorr.NT2BLM, "Education"=data.1$Educ.NT2BLQ1,
                               "Alcohol"=alcohol.total.NT2, "SystolicBP"=data.1$BPSystMn23.NT2BLM, "DiastolicBP"=data.1$BPDiasMn23.NT2BLM)

# Plot showing the missing values in the explanatory variables
plot_missing(df.exp.var[,-1], title="Missing values in explanatory variables")
dev.copy(pdf,'~/figures/DataCleaning/MissingValuesStep2.pdf') # Save the plot
dev.off()
# SO MANY MISSING VALUES OF ALCOHOL; AND ALCOHOL NOT ONE OF MAIN RISK FACTORS;
# SO WE WON'T KEEP ALCOHOL AS EXPLANATORY FOR NOW

# Check if missing values in bmi
sum(is.na(data.1$Bmi.NT2BLM))
indexNT2.miss.bmi <- is.na(data.1$Bmi.NT2BLM) 

# Check if missing values of PAI
sum(is.na(data.1$PAI.NT2))
indexNT2.miss.pai <- is.na(data.1$PAI.NT2) 

# Check if missing values in variable measuring smoking 
sum(is.na(data.1$SmoStat.NT2BLQ1))
indexNT2.miss.smok <- is.na(data.1$SmoStat.NT2BLQ1) 

# Checking if missing values in HDL Cholesterol
sum(is.na(data.1$SeHDLChol.NT2BLM))
indexNT2.miss.hdlchol <- is.na(data.1$SeHDLChol.NT2BLM) 

# Checking if missing values in gfre
sum(is.na(data.1$GFREstStag.NT2BLM))
indexNT2.miss.gfre <- is.na(data.1$GFREstStag.NT2BLM) 

# Checking if missing values in education level
sum(is.na(data.1$Educ.NT2BLQ1))
indexNT2.miss.edu <- is.na(data.1$Educ.NT2BLQ1) 

### Remove all the participants with missing values in explanatory variables
data.2 <- data.1[!(indexNT2.miss.bmi| indexNT2.miss.pai | indexNT2.miss.fam.bp.high |
                     indexNT2.miss.smok | indexNT2.miss.edu| indexNT2.miss.hdlchol |
                     indexNT2.miss.gfre),]

# Check that there are no more missing values
df.exp.var.2 <- data.frame("PID"=data.2$PID.108676,"BirthYear"=data.2$BirthYear, "Sex"=data.2$Sex, "BMI"=data.2$Bmi.NT2BLM, 
                         "PAI"=data.2$PAI.NT2, "BPHigParEv"=data.2$BPHigParEv.NT2,"RecPA"=data.2$RecPA.NT2, "SmoStat"=data.2$SmoStat.NT2BLQ1,
                         "SeChol"=data.2$SeChol.NT2BLM, "SeHDLChol"=data.2$SeHDLChol.NT2BLM, "SeGluNonFast"= data.2$SeGluNonFast.NT2BLM,
                         "GFRestStag"=data.2$GFREstStag.NT2BLM, "SeCreaCorr"=data.2$SeCreaCorr.NT2BLM, "Educ"=data.2$Educ.NT2BLQ1,
                          "BPSys2"=data.2$BPSystMn23.NT2BLM, "BPDias2"=data.2$BPDiasMn23.NT2BLM)

# Plot showing the missing values
plot_missing(df.exp.var.2)

introduce(data.2)
# Now 17 733 participants


################## HEALTH AT HUNT3 #####################################

###### FIND WHO IS ON BPMED AT HUNT3 AND CORRECT BLOOD PRESSURE
# Data tillegg contains an extra variable, BPMedSiEffEv.NT3CvdQ, 
# which can be used in combination with BPMedEv.NT3BLQ1
# to figure out who uses bp med at time of HUNT3
data.bp.med <- read.spss("/mnt/work/Data/2019-12-19_108676_Data_tillegg.sav", to.data.frame=T)

## Data format 
str(data.bp.med)
# Data frame with 106 426 observations and 15 variables

# Extract relevant variables PID and BPMedSiEffEv 
data.bp.med <- data.frame("PID.108676"= data.bp.med$PID.108676, "BPMedSiEffEv.NT3CvdQ"=data.bp.med$BPMedSiEffEv.NT3CvdQ)

# People who answered yes or no to BpMedSiEffEv
indexNT3.curr.bp.med1 <- !is.na(data.bp.med$BPMedSiEffEv.NT3CvdQ)
pid.curr.bp.med1 <- as.vector(data.bp.med$PID.108676)[indexNT3.curr.bp.med1]

# People who answered yes to BPMedEv
indexNT3.curr.bp.med2 <- data.2$BPMedEv.NT3BLQ1=="Ja"
pid.curr.bp.med2 <- as.vector(data.2$PID.108676)[indexNT3.curr.bp.med2]

# People who answered yes to BPMEdEv and either yes or no to BPMedSiEffev
pid.curr.bp.med <- intersect(pid.curr.bp.med1,pid.curr.bp.med2)
index.curr.bp.med <- match(pid.curr.bp.med, data.2$PID.108676)

# Create logical variable of blood pressure medication usage at HUNT3
indexNT3.bp.med <- vector()
for(i in 1:length(data.2$PID.108676)){
  if(as.vector(data.2$PID.108676)[i] %in% pid.curr.bp.med){
    indexNT3.bp.med[i]=TRUE}
  else{indexNT3.bp.med[i]=FALSE}
}
# Add new column with logical value of whether use bpmed at HUNT3 or not
data.2$BPMed.NT3 <- indexNT3.bp.med

# Correct the blood pressure values
data.2$BPSystMn23.NT3BLM[index.curr.bp.med] <- data.2$BPSystMn23.NT3BLM[index.curr.bp.med]+15

data.2$BPDiasMn23.NT3BLM[index.curr.bp.med] <- data.2$BPDiasMn23.NT3BLM[index.curr.bp.med]+10

####### Evaluation information at HUNT3 ####
df.eval.var.2 <- data.frame("HeartAttack"=data.2$CarInfEv.NT3BLQ1, "AnginaPectoris"=data.2$CarAngEv.NT3BLQ1,
                          "Stroke"=data.2$ApoplEv.NT3BLQ1, "Diabetes"=data.2$DiaEv.NT3BLQ1, 
                          "Glucose"=data.2$SeGluNonFast.NT3BLM)
plot_missing(df.eval.var.2, title="Missing values in evaluation variables")
dev.copy(pdf,'~/figures/DataCleaning/MissingValuesStep3.pdf') 
dev.off()

### Remove all who have missing values of CVD at HUNT3
indexNT3.miss.CVD <- is.na(data.2$CarInfEv.NT3BLQ1) | is.na(data.2$CarAngEv.NT3BLQ1) | is.na(data.2$ApoplEv.NT3BLQ1)

### Remove all who have missing values for diabetes at HUNT3
# self-reported
indexNT3.miss.dia <- is.na(data.2$DiaEv.NT3BLQ1) 

# measured glucose 
indexNT3.miss.high.glu <- is.na(data.2$SeGluNonFast.NT3BLM) 

## Remove participants with missing information of CVD and diabetes at HUNT3
data.3 <- data.2[!(indexNT3.miss.high.glu | indexNT3.miss.dia | indexNT3.miss.CVD),]

# Check missing values
df.eval.var.3 <- data.frame("Heart Attack"=data.3$CarInfEv.NT3BLQ1, "Ang.Pec"=data.3$CarAngEv.NT3BLQ1,
                          "Stroke"=data.3$ApoplEv.NT3BLQ1, "DiaEv3"=data.3$DiaEv.NT3BLQ1, 
                          "SeGlunonFast3"=data.3$SeGluNonFast.NT3BLM)
plot_missing(df.eval.var.3)

introduce(data.3)
# Now 17365 participants


#### PEOPLE WITH DIABETES AT HUNT3
# Add new column with logical values which is true if have diabetes at HUNT3
data.3$DiaCurr.NT3 <- data.3$DiaEv.NT3BLQ1=="Ja" | data.3$SeGluNonFast.NT3BLM>=11.1


### PEOPLE WITH CVD at HUNT3
# Add new column with logical values which is true if have CVD at HUNT3
data.3$CVD.NT3  <- data.3$CarInfEv.NT3BLQ1=="Ja" | data.3$CarAngEv.NT3BLQ1=="Ja" | data.3$ApoplEv.NT3BLQ1=="Ja"
 
############# BASIC ANALYSIS #################
# Total data set after cleaning 
introduce(data.3)

################### Total dataset 
# Create new data set with  relevant dependent and independent variables, 
# and  with information on certain illnesses at HUNT3
df.total <- data.frame("PID"=data.3$PID.108676, "BirthYear"=data.3$BirthYear, "Sex"=data.3$Sex, "BMI2"=data.3$Bmi.NT2BLM, 
                      "SystolicBP2"=data.3$BPSystMn23.NT2BLM, "DiastolicBP2"=data.3$BPDiasMn23.NT2BLM, "PAI2"=data.3$PAI.NT2, 
                      "RecPA2"=data.3$RecPA.NT2, "BPHigPar2"=data.3$BPHigParEv.NT2, "BPHigFath2"=data.3$BPHigFathEv.NT2BLQ2,
                      "BPHigMoth2"=data.3$BPHigMothEv.NT2BLQ2, "Smoking2"=data.3$SmoStat.NT2BLQ1, 
                      "Cholesterol2"=data.3$SeChol.NT2BLM, "HDLCholesterol2"=data.3$SeHDLChol.NT2BLM, "Glucose2"= data.3$SeGluNonFast.NT2BLM, 
                      "GFR2"=data.3$GFREstStag.NT2BLM, "Creatinine2"=data.3$SeCreaCorr.NT2BLM, "Education2"=data.3$Educ.NT2BLQ1, 
                      "SystolicBP3"=data.3$BPSystMn23.NT3BLM,"DiastolicBP3"=data.3$BPDiasMn23.NT3BLM,"Diabetes3"=data.3$DiaCurr.NT3,
                      "CVD3"= data.3$CVD.NT3, "BPMed3"=data.3$BPMed.NT3)
plot_missing(df.total[,-c(10,11)], title = "Missing values in relevant data set") 
dev.copy(pdf,'~/figures/DataCleaning/MissingValuesDF.pdf') 
dev.off()


### TRANSLATION ####
# See that some of the levels are in Norwegian
# Translate levels to English

levels(df.total$Sex)
levels(df.total$Sex)<- c("Female", "Male")


levels(df.total$Smoking2)
levels(df.total$Smoking2)<- c("Never", "Previous", "Current")

levels(df.total$Education2)
#levels(df$Educ)<- c("Primary school 7-10 years, Folk high school", 
#                    "Upper secondary school 1-2 years",
#                    "Upper secondary school 3 years",
#                   "Higher education/University, less than 4 years",             
#                   "Higher education/University, 4 years or more")
levels(df.total$Education2) <- c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5")


levels(df.total$GFR2)
levels(df.total$GFR2)<- c("Stage 1", "Stage 2", "Stage 3", "Stage 4", "Stage 5")

############# SAVE DATA SET################

save(df.total, file="MyData/DataCleaning.df.total.RData")

