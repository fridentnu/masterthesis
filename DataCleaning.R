library(foreign) #to import spss-files
library(tibble)
library(DataExplorer) # EDA package in R


##### OBS: HUSK Å LAGRE KILDER
# https://towardsdatascience.com/exploratory-data-analysis-8fc1cb20fd15
# https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619 Data explorer
# https://hunt-db.medisin.ntnu.no/hunt-db/#/ 

# OBS: husk at man må bruke ctrl isteden for cmd på hurtigtastene
# use viridis colours, colour-blind and printer friendly
# save the random seed if use one!


########## LOAD DATA #############

# Data Study Cohort I (all participants from HUNT2 and/or HUNT3)
data <- read.spss("Data/2019-03-26_108676_Data Study I Cohort.sav", to.data.frame=T)
# får feilmelding om duplicated levels men når jeg sjekker levels så er de unike

### Quick overview
str(data)

### First check that list of participants is unique and does not contain missing values

# No NA in project personidentification 
sum(is.na(data$PID.108676))

# all unique 
sum(!unique(data$PID.108676)==data$PID.108676)

# Look at names of the 232 variables
var.names.data <-colnames(data)

### Keep only people who participated in both HUNT2 anD HUNT3
# everyone invited to BLq1 and BLm, but only those who participated in blm participated in blq2
# NA in participation means not invited
indexNT23.part <- data$Part.NT2BLQ1=="Deltatt" & data$Part.NT2BLM=="Deltatt" & data$Part.NT2BLQ2=="Deltatt" & data$Part.NT3BLM=="Deltatt" & data$Part.NT3BLQ1=="Deltatt"
sum(indexNT23, na.rm=T)
indexNT23.miss <- is.na(data$Part.NT2BLQ1) | is.na(data$Part.NT2BLM) | is.na(data$Part.NT2BLQ2) |is.na(data$Part.NT3BLM) | is.na(data$Part.NT3BLQ1)
data <- data[indexNT23.part & !indexNT23.miss,]



######## DATA REDUCTION AND CLEANING ########
### Remove all variables from HUNT1
indexNT1.data <- grepl('NT1',colnames(data))
data <- data[,!indexNT1.data]


############################# STEP 1 ############




######## EXCLUSION CRITERIA ###########

# History of CVD at time of HUNT2
# CarInfEv@NT2BLQ1 (ever had heart attack), CarAngEv@NT2BLQ1 (hjertekrampe/chest pain), ApoplEv@NT2BLQ1 (stroke)
indexNT2.CVD <- data$CarInfEv.NT2BLQ1=="Ja" | data$CarAngEv.NT2BLQ1=="Ja" | data$ApoplEv.NT2BLQ1=="Ja"


# History of diabetes
# also want to exclude people with non fasting glucose above threshold value or missing value
# probably undiagnosed diabetes
# "Symptoms plus random blood glucose ?11.1 mmol/L (?200 mg/dL) indicate diabetes" 
# - Cosentino et al, 2019  https://doi.org/10.1093/eurheartj/ehz486
indexNT2.dia <- data$DiaEv.NT2BLQ1=="Ja" | data$SeGluNonFast.NT2BLM>=11.1

# History of hypertension at time of HUNT2
indexNT2.hyp <- data$BPSystMn23.NT2BLM>140 | data$BPDiasMn23.NT2BLM>90 |data$BPMedCu.NT2BLQ1=="N\xe5" | data$BPMedCu.NT2BLQ1=="F\xf8r, men ikke n\xe5"

df.ill <-data.frame(indexNT2.CVD, indexNT2.hyp, indexNT2.dia)

plot_bar(df.ill) # Most people in the study are healthy. See that the biggest loss comes from hypertensive people

# Check missing values more closely
df.health <- data.frame("BPMed2"= data$BPMedCu.NT2BLQ1, "SystolicMean2"=data$BPSystMn23.NT2BLM,
                        "DiastolicMean2"=data$BPDiasMn23.NT2BLM, "HeartAttack"=data$CarInfEv.NT2BLQ1, 
                        "Ang.Pec"=data$CarAngEv.NT2BLQ1,"Stroke"=data$ApoplEv.NT2BLQ1, 
                        "Diabetes2"=data$DiaEv.NT2BLQ1, "BloodGlucose2"=data$SeGluNonFast.NT2BLM, "SystolicMean3"=data$BPSystMn23.NT3BLM, 
                        "DiastolicMean3"=data$BPDiasMn23.NT3BLM, "BPMed3"=data$BPMedEv.NT3BLQ1)
plot_missing(df.health)
# few missing values in health data at time of HUNT2, so we remove all with history and NA

#Very few missing rows



#########################################3

## Missing values for cvd at HUNT2
indexNT2.miss.CVD <- is.na(data$CarInfEv.NT2BLQ1) | is.na(data$CarAngEv.NT2BLQ1) | is.na(data$ApoplEv.NT2BLQ1)

# Missing values for diabetes at HUNT2
indexNT2.miss.dia <- is.na(data$DiaEv.NT2BLQ1) | is.na(data$SeGluNonFast.NT2BLM)

#### Missing values hypertension
indexNT2.miss.hyp <- is.na(data$BPMedCu.NT2BLQ1)|is.na(data$BPSystMn23.NT2BLM) | is.na(data$BPDiasMn23.NT2BLM)


indexNT3.miss.bp <-is.na(data$BPSystMn23.NT3BLM) | is.na(data$BPDiasMn23.NT3BLM)
indexNT3.miss.bpmed <- is.na(data$BPMedEv.NT3BLQ1) 


# clean data
data <- data[!(indexNT2.CVD| indexNT2.dia | indexNT2.hyp|
                 indexNT2.miss.CVD | indexNT2.miss.dia |indexNT2.miss.hyp |
                 indexNT3.miss.bp|indexNT3.miss.bpmed),]

df.health <- data.frame("BPMed2"= data$BPMedCu.NT2BLQ1, "SystolicMean2"=data$BPSystMn23.NT2BLM,
                        "DiastolicMean2"=data$BPDiasMn23.NT2BLM, "HeartAttack"=data$CarInfEv.NT2BLQ1, 
                        "Ang.Pec"=data$CarAngEv.NT2BLQ1,"Stroke"=data$ApoplEv.NT2BLQ1, 
                        "Diabetes2"=data$DiaEv.NT2BLQ1, "BloodGlucose2"=data$SeGluNonFast.NT2BLM, "SystolicMean3"=data$BPSystMn23.NT3BLM, 
                        "DiastolicMean3"=data$BPDiasMn23.NT3BLM, "BPMed3"=data$BPMedEv.NT3BLQ1)
plot_missing(df.health)

# Now 19748 participants

########## STEP 2

# # total number of units of alcohol within last two weeks
# alcohol.total.NT2 <-data$AlcBeL2WN.NT2BLQ1+data$AlcLiL2WN.NT2BLQ1+data$AlcWiL2WN.NT2BLQ1
# 
# 
# df.dep.indep.var <- data.frame("PID"=data$PID.108676,"BirthYear"=data$BirthYear, "Sex"=data$Sex, "BMI"=data$Bmi.NT2BLM, 
#                                "PAI"=data$PAI.NT2, "RecPA"=data$RecPA.NT2, "SmoStat"=data$SmoStat.NT2BLQ1, 
#                                "SeChol"=data$SeChol.NT2BLM, "SeHDLChol"=data$SeHDLChol.NT2BLM, "SeGluNonFast"= data$SeGluNonFast.NT2BLM, 
#                                "GFRestStag"=data$GFREstStag.NT2BLM, "SeCreaCorr"=data$SeCreaCorr.NT2BLM, "Educ"=data$Educ.NT2BLQ1, 
#                                "Alc"=alcohol.total.NT2, "BPSys2"=data$BPSystMn23.NT2BLM, "BPDias2"=data$BPDiasMn23.NT2BLM, 
#                                "BPSys3"=data$BPSystMn23.NT3BLM, "BPDias3"=data$BPDiasMn23.NT3BLM)

# Plot showing the missing values
###############################################################################


str(data)

####### INCLUSION CRITERIA ######

##### Evaluation information at HUNT3
df.eval.var <- data.frame("Heart Attack"=data$CarInfEv.NT3BLQ1, "Ang.Pec"=data$CarAngEv.NT3BLQ1,
                          "Stroke"=data$ApoplEv.NT3BLQ1, "DiaEv3"=data$DiaEv.NT3BLQ1, 
                          "SeGlunonFast3"=data$SeGluNonFast.NT3BLM, "BP Med"= data$BPMedEv.NT3BLQ1)
plot_missing(df.eval.var)


### Remove all who have missing values for cvd at HUNT3
indexNT3.miss.CVD <- is.na(data$CarInfEv.NT3BLQ1) | is.na(data$CarAngEv.NT3BLQ1) | is.na(data$ApoplEv.NT3BLQ1)
sum(indexNT3.miss.CVD, na.rm=T)
data <- data[!indexNT3.miss.CVD,]

### Remove all who have missing values for diabetes at HUNT3
# self-reported
indexNT3.miss.dia <- is.na(data$DiaEv.NT3BLQ1) 
sum(indexNT3.miss.dia, na.rm=T)
data <- data[!indexNT3.miss.dia,]

# measured glucose 
sum(data$SeGluNonFast.NT3BLM>=11.1,na.rm=T)
indexNT3.miss.high.glu <- is.na(data$SeGluNonFast.NT3BLM) 
data <- data[!indexNT3.miss.high.glu,]


### Remove all who have missing values for blood pressure medication at HUNT3
indexNT3.miss.bpmed <- is.na(data$BPMedEv.NT3BLQ1) 
sum(indexNT3.miss.bpmed, na.rm=T)
data <- data[!indexNT3.miss.bpmed,]

str(data)

#### DEPENDENT AND INDEPENDENT VARIABLES

# bp ved HUNT2, foreldrenes historie med hypertensjon, alder, kjønn, PAI,
# mvpa score (hvor fysisk man er, kanskje binær, over/under anbefalt verdi),
# bmi (evt. waist circumpherence, større usikkerhet i den), 
# (alkohol), røyking (smostat), totalt kolestrol, hdl-kolestrol, 
# non-fasting glucose (cut-off >11.1, ta bort disse for det er kanskje udiagnostisert diabetes), 
# egrf og kreatinin (assosiert med blodtrykk og nyresykdom),
# sosioøkonomisk status (utdannigsnivå)

# To see if any of the independent variables will reduce our data set greatly
# we study the number of missing value per variable


############### ADD PAI ##############
# categorical value of physical activity
source("R code/PAI.R")
data$PAI.NT2 <- PAIlevel_NT2


############### ADD RecPA ##############
# Logical indicator mof whether a person reaches the recommended amount of physical activity
source("R code/MVPA.R")
data$RecPA.NT2 <- MeetsPARecomed_NT2


# Total number of units of alcohol during last two weeks
alcohol.total.NT2 <-data$AlcBeL2WN.NT2BLQ1+data$AlcLiL2WN.NT2BLQ1+data$AlcWiL2WN.NT2BLQ1


df.dep.indep.var <- data.frame("PID"=data$PID.108676,"BirthYear"=data$BirthYear, "Sex"=data$Sex, "BMI"=data$Bmi.NT2BLM, 
                      "PAI"=data$PAI.NT2, "RecPA"=data$RecPA.NT2, "SmoStat"=data$SmoStat.NT2BLQ1, 
                      "SeChol"=data$SeChol.NT2BLM, "SeHDLChol"=data$SeHDLChol.NT2BLM, "SeGluNonFast"= data$SeGluNonFast.NT2BLM, 
                      "GFRestStag"=data$GFREstStag.NT2BLM, "SeCreaCorr"=data$SeCreaCorr.NT2BLM, "Educ"=data$Educ.NT2BLQ1, 
                      "Alc"=alcohol.total.NT2, "BPSys2"=data$BPSystMn23.NT2BLM, "BPDias2"=data$BPDiasMn23.NT2BLM, 
                      "BPSys3"=data$BPSystMn23.NT3BLM, "BPDias3"=data$BPDiasMn23.NT3BLM)

# Plot showing the missing values in each independent and dependent variable
plot_missing(df.dep.indep.var)

# SO MANY MISSING VALUES; AND ALCOHOL NOT ONE OF MAIN RISK FACTORS;
# SO WE WON'T KEEP ALCOHOL AS RISKFACTORS FOR NOW


#### Blood pressure measurements
### Remove all who have missing mean systolic or mean diastolic bp measurements in HUNT2 or HUNT3
index.miss.bp <-is.na(data$BPSystMn23.NT3BLM) | is.na(data$BPDiasMn23.NT3BLM)
sum(index.miss.bp, na.rm=T)
data <- data[!index.miss.bp,]

# Check if missing values in birth year 
sum(is.na(data$BirthYear))

# Check if missing values in sex
sum(is.na(data$Sex))

# Check if missing values in bmi
sum(is.na(data$Bmi.NT2BLM))
indexNT2.miss.bmi <- is.na(data$Bmi.NT2BLM) 
data <- data[!indexNT2.miss.bmi,]

# Check if missing values of PAI
sum(is.na(data$PAI.NT2))
indexNT2.miss.pai <- is.na(data$PAI.NT2) 
data <- data[!indexNT2.miss.pai,]

# Check if missing values of RecPA
sum(is.na(data$RecPA.NT2))


# Check for missing values in parental blood pressure 
# Missing value in all categories
indexNT2.miss.fam.bp.high <- is.na(data$BPHigFamNon.NT2BLQ2) & is.na(data$BPHigBrotEv.NT2BLQ2) & is.na(data$BPHigFathEv.NT2BLQ2) & is.na(data$BPHigChiEv.NT2BLQ2) & is.na(data$BPHigSistEv.NT2BLQ2) & is.na(data$BPHigMothEv.NT2BLQ2)
sum(indexNT2.miss.fam.bp.high)
data <- data[!indexNT2.miss.fam.bp.high,]

# Parents have high blood pressure if answered either mom or dad, 
# parents don't have high blood pressure, if both mom and dad NA and answered at least one question
indexNT2.BPHigParEv <- data$BPHigFathEv.NT2BLQ2== "Far - h\xf8yt BT" | data$BPHigMothEv.NT2BLQ2== "Mor - h\xf8yt BT" 
sum(indexNT2.BPHigParEv, na.rm=T)

# Add new column with logical variable of parental history of hypertension
data$BPHigParEv.NT2 <- indexNT2.BPHigParEv
data$BPHigParEv.NT2[is.na(indexNT2.BPHigParEv)] <-FALSE  #NA meant either no or na previosly


# Check if missing values in variable measuring smoking 
sum(is.na(data$SmoStat.NT2BLQ1))
indexNT2.miss.smok <- is.na(data$SmoStat.NT2BLQ1) 
data <- data[!indexNT2.miss.smok,]

# Check if missing values in variable measuring alcohol
# Alc2W.NT2BLQ1 the total number of units variable (the sum of the ones below)
#sum(is.na(data$AlcBeL2WN.NT2BLQ1))
#sum(is.na(data$AlcWiL2WN.NT2BLQ1))
#sum(is.na(data$AlcLiL2WN.NT2BLQ1))
#indexNT2.miss.alc <- is.na(data$AlcBeL2WN.NT2BLQ1) | is.na(data$AlcWiL2WN.NT2BLQ1) | is.na(data$AlcLiL2WN.NT2BLQ1)
#sum(indexNT2.miss.alc, na.rm=T)
#data <- data[!indexNT2.miss.alc,]


# Checking if missing values in total cholestrol
sum(is.na(data$SeChol.NT2BLM))
indexNT2.miss.chol <- is.na(data$SeChol.NT2BLM) 
data <- data[!indexNT2.miss.chol,]

# Checking if missing values in HDL cholestrol
sum(is.na(data$SeHDLChol.NT2BLM))
indexNT2.miss.hdlchol <- is.na(data$SeHDLChol.NT2BLM) 
data <- data[!indexNT2.miss.hdlchol,]

# Checking if missing values in gfre
sum(is.na(data$GFREstStag.NT2BLM))
indexNT2.miss.gfre <- is.na(data$GFREstStag.NT2BLM) 
data <- data[!indexNT2.miss.gfre,]

# Checking if missing values in Creatinine
sum(is.na(data$SeCreaCorr.NT2BLM))

# Checking if missing values in education level
sum(is.na(data$Educ.NT2BLQ1))
indexNT2.miss.edu <- is.na(data$Educ.NT2BLQ1) 
data <- data[!indexNT2.miss.edu,]

str(data)


###### FIND WHO IS ON BPMED AT HUNT3 AND CORRECT BLOOD PRESSURE

# Data tillegg contains an extra variable, BPMedSiEffEv.NT3CvdQ, 
# which can be used in combination with BPMedEv.NT3BLQ1
# to figure out who uses bp med at time of hunt3
data.bp.med <- read.spss("Data/2019-12-19_108676_Data_tillegg.sav", to.data.frame=T)

# Extract relevant variables PID and BPMedSiEffEv 
data.bp.med <- data.frame("PID.108676"= data.bp.med$PID.108676, "BPMedSiEffEv.NT3CvdQ"=data.bp.med$BPMedSiEffEv.NT3CvdQ)

# People who answered yes or no to BpMedSiEffEv
indexNT3.curr.bp.med1 <- !is.na(data.bp.med$BPMedSiEffEv.NT3CvdQ)
pid.curr.bp.med1 <- as.vector(data.bp.med$PID.108676)[indexNT3.curr.bp.med1]

# People who answered yes to BPMedEv
indexNT3.curr.bp.med2 <- data$BPMedEv.NT3BLQ1=="Ja"
pid.curr.bp.med2 <- as.vector(data$PID.108676)[indexNT3.curr.bp.med2]

# People who answered yes to BPMEdEv and either yes or no to BPMedSiEffev
pid.curr.bp.med <- intersect(pid.curr.bp.med1,pid.curr.bp.med2)
index.curr.bp.med <- match(pid.curr.bp.med, data$PID.108676)


indexNT3.bp.med <- vector()
for(i in 1:length(data$PID.108676)){
  if(as.vector(data$PID.108676)[i] %in% pid.curr.bp.med){
    indexNT3.bp.med[i]=TRUE}
  else{indexNT3.bp.med[i]=FALSE}
}
# Add new column with logical value of whether use bpmed at HUNT3 or not
data$BPMed.NT3 <- indexNT3.bp.med

# Correct the blood pressure values
data$BPSystMn23.NT3BLM[index.curr.bp.med] <- data$BPSystMn23.NT3BLM[index.curr.bp.med]+15

data$BPDiasMn23.NT3BLM[index.curr.bp.med] <- data$BPDiasMn23.NT3BLM[index.curr.bp.med]+10


#### PEOPLE WITH DIABETES AT HUNT 3
# Self-reported diabetes
indexNT3.dia <- data$DiaEv.NT3BLQ1=="Ja"

# Probably undiagnosed diabetes
indexNT3.high.glu <- data$SeGluNonFast.NT3BLM>=11.1

# Add new column with logical values which is true if have diabetes at HUNT3
data$DiaCurr.NT3 <- indexNT3.dia | indexNT3.high.glu


### PEOPLE WITH CVD at HUNT3
# Add new column with logical values which is true if have CVD at HUNT3
data$CVD.NT3  <- data$CarInfEv.NT3BLQ1=="Ja" | data$CarAngEv.NT3BLQ1=="Ja" | data$ApoplEv.NT3BLQ1=="Ja"
 
############# BASIC ANALYSIS #################
# Total data set after cleaning 
str(data)

#"PID"=data$PID.108676 include this?

# Create new data set with only relevant dependent and independent variables
df <- data.frame("PID"=data$PID.108676, "BirthYear"=data$BirthYear, "Sex"=data$Sex, "BMI"=data$Bmi.NT2BLM, 
                 "BPSys2"=data$BPSystMn23.NT2BLM, "BPDias2"=data$BPDiasMn23.NT2BLM, "PAI"=data$PAI.NT2, 
                 "RecPA"=data$RecPA.NT2, "BPHigPar"=data$BPHigParEv.NT2, "SmoStat"=data$SmoStat.NT2BLQ1, 
                 "SeChol"=data$SeChol.NT2BLM, "SeHDLChol"=data$SeHDLChol.NT2BLM, "SeGluNonFast"= data$SeGluNonFast.NT2BLM, 
                 "GFRestStag"=data$GFREstStag.NT2BLM, "SeCreaCorr"=data$SeCreaCorr.NT2BLM, "Educ"=data$Educ.NT2BLQ1, 
                 "BPSys3"=data$BPSystMn23.NT3BLM, "BPDias3"=data$BPDiasMn23.NT3BLM)

# Create new dataframe with information used to evaluate performance of model on certain subgroups
df.eval.3 <- data.frame("Diabetes3"=data$DiaCurr.NT3, "CVD3"= data$CVD.NT3, "BPMed3"=data$BPMed.NT3)

#Overview of data sets
plot_str(df)

plot_str(df.eval.3)

# # Translate letters to Norwegian
# levels(df$SmoStat)
# levels(df$SmoStat)<- c("Aldri roeykt daglig", "Tidligere daglig roeyker", "Daglig roeyker")
# 
# levels(df$Educ)
# levels(df$Educ)<- c("Grunnskole 7-10 aar, framhaldsskole, folkehoegskole", 
#                     "Realskole, middelskole, yrkesskole 1-2 aarig vgs",
#                     "Artium, oek.gymnas, allmennfaglig retning i vgs",
#                     "Hoegskole/universitet, mindre enn 4 aar",             
#                     "Hoegskole/universitet, 4 aar eller mer")

# Translate letters to Norwegian
levels(df$Sex)
levels(df$Sex)<- c("Female", "Male")


levels(df$SmoStat)
levels(df$SmoStat)<- c("Never smoked daily", "Previous daily smoker", "Daily smoker")

levels(df$Educ)
levels(df$Educ)<- c("Primary school 7-10 years, Folk high school", 
                    "Upper secondary school 1-2 years",
                    "Upper secondary school 3 years",
                    "Higher education/University, less than 4 years",             
                    "Higher education/University, 4 years or more")


# 18 variables, where 15 are explanatory variables and, 2 are dependent variables?
str(df)

###########################

# No missing  values 
plot_missing(df) 

# No missing values
plot_missing(df.eval.3)

#################################################################################




