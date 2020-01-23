library(foreign)
#library(tibble)
library(DataExplorer)


##### OBS: HUSK Å LAGRE KILDER
# https://towardsdatascience.com/exploratory-data-analysis-8fc1cb20fd15
# https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619 Data explorer
# https://hunt-db.medisin.ntnu.no/hunt-db/#/ 

# OBS: husk at man må bruke ctrl isteden for cmd på hurtigtastene
# use viridis colours, colour-blind and printer friendly
# save the random seed if use one!


########## LOAD DATA #############

# Data Study Cohort I är datafilen. Denna fil har inte PAI utrräknat, men återkommer med de variablerna ASAP.
data <- read.spss("Data/2019-03-26_108676_Data Study I Cohort.sav", to.data.frame=T)
# får feilmelding om duplicated levels men når jeg sjekker levels så er de unike

# Which other study parts are relevant? Are there some who have na on part. but still have answered?
indexNT23 <- data$Part.NT2BLQ1=="Deltatt" & data$Part.NT3BLM=="Deltatt" & data$Part.NT2BLM=="Deltatt"
sum(indexNT23, na.rm=T)


############### ADD PAI ##############
# categorical value of physical activity

source("R code/PAI.R")
data$PAI.NT2 <- PAIlevel_NT2



############### ADD RecPA ##############
# Logical indicator mof whether a person reaches the recommended amount of physical activity

source("R code/MVPA.R")
data$RecPA.NT2 <- MeetsPARecomed_NT2


############# REMOVE IRRELEVANT DATA ########


#### Remove all variables from HUNT1
indexNT1.data <- grepl('NT1',colnames(data))
data <- data[!indexNT1.data]

#### Remove all who have self-reported history of CVD at time of HUNT2
# CarInfEv@NT2BLQ1 (ever had heart attack), CarAngEv@NT2BLQ1 (hjertekrampe/chest pain), ApoplEv@NT2BLQ1 (stroke)
# dersom svart ja på en av disse, så har cvd historie
indexNT2.CVD <- data$CarInfEv.NT2BLQ1=="Ja" | data$CarAngEv.NT2BLQ1=="Ja" | data$ApoplEv.NT2BLQ1=="Ja"
sum(indexNT2.CVD, na.rm=T)
data <- data[!indexNT2.CVD,]


#### Remove all who have self-reported history of diabetes at time of HUNT2
indexNT2.dia <- data$DiaEv.NT2BLQ1=="Ja"
sum(indexNT2.dia, na.rm=T)
data <- data[!indexNT2.dia,]

# also want to exclude people with non fasting glucose above threshold value
# probably undiagnosed diabetes
# "Symptoms plus random blood glucose ≥11.1 mmol/L (≥200 mg/dL) indicate diabetes" 
# - Cosentino et al, 2019  https://doi.org/10.1093/eurheartj/ehz486

sum(data$SeGluNonFast.NT2BLM>=11.1,na.rm=T)
indexNT2.high.glu <- data$SeGluNonFast.NT2BLM>=11.1
data <- data[!indexNT2.high.glu,]




#### Remove all who are currently or previously been on bpmed at time of HUNT2
indexNT2.bp.med <- data$BPMedCu.NT2BLQ1=="N\xe5" | data$BPMedCu.NT2BLQ1=="F\xf8r, men ikke n\xe5"
sum(indexNT2.bp.med, na.rm=T)
data <- data[!(indexNT2.bp.med),]
# Check that removed all who have ever taken bpmed
sum(data$indexNT2.bp.med,na.rm=T)==0

#### Sjekk om fortsatt noen som svarer ja her etter har fjernet cvd
sum(data$BPMedCu.NT2CvdQ=="Ja",na.rm=T)


#### Remove all who are currently hypertensive at time of HUNT2
# definition mean sys>140mmHg, or mean dia >90mmHg, or on bpmed (already excluded these)
indexNT2.hyp <- data$BPSystMn23.NT2BLM>140 | data$BPDiasMn23.NT2BLM>90
sum(indexNT2.hyp, na.rm=T)
data <- data[!(indexNT2.bp.med),]

### Remove all who have missing mean systolic or mean diastolic bp measurements in HUNT2 or HUNT3
index.miss.bp <- is.na(data$BPSystMn23.NT2BLM) | is.na(data$BPDiasMn23.NT2BLM) | is.na(data$BPSystMn23.NT3BLM) | is.na(data$BPDiasMn23.NT3BLM)
sum(index.miss.bp, na.rm=T)
data <- data[!index.miss.bp,]


### Remove all who have missing values for cvd at HUNT3

indexNT3.miss.CVD <- is.na(data$CarInfEv.NT3BLQ1) | is.na(data$CarAngEv.NT3BLQ1) | is.na(data$ApoplEv.NT3BLQ1)
sum(indexNT3.miss.CVD, na.rm=T)
data <- data[!indexNT3.miss.CVD,]

### Remove all who have missing values for diabetes at HUNT3
indexNT3.miss.dia <- is.na(data$DiaEv.NT3BLQ1) 
sum(indexNT3.miss.dia, na.rm=T)
data <- data[!indexNT3.miss.dia,]

### Remove all who have missing values for blood pressure medication at HUNT3
indexNT3.miss.bpmed <- is.na(data$BPMedEv.NT3BLQ1) 
sum(indexNT3.miss.bpmed, na.rm=T)
data <- data[!indexNT3.miss.dia,]


################### MISSING VALUES ############


### First check that list of people does not contain NA and is unique

### Check if have unique list of persons

# No NA in project personidentification 
sum(is.na(data$PID.108676))

# all unique 
sum(!unique(data$PID.108676)==data$PID.108676)

# all start with 108676
sum(data$PID.108676[grepl(108676,data$PID.108676)]!=data$PID.108676)

# remove the thing that is equal for everyone?
test.pid <- sub("108676","",data$PID.108676)
 
### Check colnames to find possible explanatory variables
# Look at names of the 232 variables
var.names.data <-colnames(data)
# all variables that start with Part is about participation in the specific study part


# alder, kjønn, PAI, mvpa score (hvor fysisk man er, kanskje binær, over/under anbefalt verdi),
# bmi (evt. waist circumpherence, større usikkerhet i den), 
# alkohol, røyking (smostat), totalt kolestrol, hdl-kolestrol, 
# non-fasting glucose (cut-off >11.1, ta bort disse for det er kanskje udiagnostisert diabetes), 
# egrf og kreatinin (assosiert med blodtrykk og nyresykdom),
# sosioøkonomisk status (utdannigsnivå)

# Check if missing values in birth year 
sum(is.na(data$BirthYear))

# Check if missing values in sex
sum(is.na(data$Sex))

# Check if missing values in bmi
sum(is.na(data$bmi))


# Check if missing values of PAI
sum(is.na(data$PAI.NT2))
indexNT2.miss.pai <- is.na(data$PAI.NT2) 
data <- data[!indexNT2.miss.pai,]

# Check if missing values of RecPA
sum(is.na(data$RecPA.NT2))


# Check for missing values in values 
# Missing value in all categories
indexNT2.miss.fam.bp.high <- is.na(data$BPHigFamNon.NT2BLQ2) & is.na(data$BPHigBrotEv.NT2BLQ2) & is.na(data$BPHigFathEv.NT2BLQ2) & is.na(data$BPHigChiEv.NT2BLQ2) & is.na(data$BPHigSistEv.NT2BLQ2) & is.na(data$BPHigMothEv.NT2BLQ2)
data <- data[!indexNT2.miss.fam.bp.high,]
sum(indexNT2.miss.fam.bp.high)

# Parents have high blood pressure if answered either mom or dad, 
# parents don't have high blood pressure, if both mom and dad NA and answered at least one question
indexNT2.BPHigParEv <- data$BPHigFathEv.NT2BLQ2== "Far - h\xf8yt BT" | data$BPHigMothEv.NT2BLQ2== "Mor - h\xf8yt BT" 
sum(indexNT2.BPHigParEv, na.rm=T)
data$BPHigParEv.NT2 <- indexNT2.BPHigParEv
data$BPHigParEv.NT2[is.na(indexNT2.BPHigParEv)] <-FALSE



# Check if missing values in variable measuring smoking 
sum(is.na(data$SmoStat.NT2BLQ1))
indexNT2.miss.smok <- is.na(data$SmoStat.NT2BLQ1) 
data <- data[!indexNT2.miss.smok,]

# Check if missing values in variable measuring alcohol
# Alc2W.NT2BLQ1 the total number of units variable (the sum of the ones below)
sum(is.na(data$AlcBeL2WN.NT2BLQ1))
sum(is.na(data$AlcWiL2WN.NT2BLQ1))
sum(is.na(data$AlcLiL2WN.NT2BLQ1))
indexNT2.miss.alc <- is.na(data$AlcBeL2WN.NT2BLQ1) | is.na(data$AlcWiL2WN.NT2BLQ1) | is.na(data$AlcLiL2WN.NT2BLQ1)
sum(indexNT2.miss.alc, na.rm=T)
#data <- data[!indexNT2.miss.alc,]
# SO MANY MISSING VALUES; AND ALCOHOL NOT ONE OF MAIN RISK FACTORS;
# SO WE WON'T KEEP ALCOHOL AS RISKFACTORS FOR NOW


# Checking if missing values in total cholestrol
sum(is.na(data$SeChol.NT2BLM))
indexNT2.miss.chol <- is.na(data$SeChol.NT2BLM) 
data <- data[!indexNT2.miss.chol,]

# Checking if missing values in HDL cholestrol
sum(is.na(data$SeHDLChol.NT2BLM))
indexNT2.miss.hdlchol <- is.na(data$SeHDLChol.NT2BLM) 
data <- data[!indexNT2.miss.hdlchol,]

# Checking if missing values in non-fasting glucose

sum(is.na(data$SeGluNonFast.NT2BLM))
indexNT2.miss.glu <- is.na(data$SeGluNonFast.NT2BLM)
data <- data[!indexNT2.miss.glu,]


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


# If remove people with missing values on alcohol, i am left with 14 998 participants
# If don't consider alcohol, then left with 25 556 participants







###########################
# data
na.vec <- is.na(data)
colSums(na.vec) # vector containing number of missing values for each variable in data
mean(colSums(na.vec))

# total number of missing values in data
sum(na.vec)

plot_missing(data) # A lot of missing data



############# BASIC ANALYSIS #################

# 234 variabler
# factors and num 
str(data)




###### FIND WHO IS ON BPMED AT HUNT3 AND CORRECT BLOOD PRESSURE

# Data tillegg innehåller en extra variabel från HUNT3CVD som ni kan använda i kombination 
# med en variabel från HUNT3BLQ1 för att se vilka som använder blodtrycksmedisin vid HUNT3
data.bp.med <- read.spss("Data/2019-12-19_108676_Data_tillegg.sav", to.data.frame=T)

# Extract relevant variables, namely PID and BPMedSiEffEv 
data.bp.med <- data.frame("PID.108676"= data.bp.med$PID.108676, "BPMedSiEffEv.NT3CvdQ"=data.bp.med$BPMedSiEffEv.NT3CvdQ)

# People who answered yes or no to BpMedSiEffEv
indexNT3.curr.bp.med1 <- !is.na(data.bp.med$BPMedSiEffEv.NT3CvdQ)
pid.curr.bp.med1 <- as.vector(data.bp.med$PID.108676)[indexNT3.curr.bp.med]

# People who answered yes to BPMedEv
indexNT3.curr.bp.med2 <- data$BPMedEv.NT3BLQ1=="Ja"
pid.curr.bp.med2 <- as.vector(data$PID.108676)[indexNT3.curr.bp.med2]

# people who answered yes to BPMEdEv and either yes or no to BPM
pid.curr.bp.med <- intersect(pid.curr.bp.med1,pid.curr.bp.med2)
index.curr.bp.med <- match(pid.curr.bp.med, data$PID.108676)

# Correct the blood pressure values
data$BPSystMn23.NT3BLM[index.curr.bp.med] <- data$BPSystMn23.NT3BLM[index.curr.bp.med]+15

data$BPDiasMn23.NT3BLM[index.curr.bp.med] <- data$BPDiasMn23.NT3BLM[index.curr.bp.med]+10


#### PEOPLE WITH DIABETES AT HUNT 3
# Self-reported diabetes
indexNT3.dia <- data$DiaEv.NT3BLQ1=="Ja"

# Probably undiagnosed diabetes
indexNT3.high.glu <- data$SeGluNonFast.NT3BLM>=11.1


