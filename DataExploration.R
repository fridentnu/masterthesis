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

############# BASIC ANALYSIS #################

# 232 variabler
# factors and num 
str(data)  
head(data)
summary(data)



# 106 426 observasjoner av 11 variabler
# only factors
glimpse(data.bp.med)
summary(data.bp.med)

# check meadian vs. mean to look for sign of extreme outliers


####################################

# Look at names of the 232 variables
var.names.data <-colnames(data)

# Look at names of the 11 variables
var.names.data.bp.med <- colnames(data.bp.med)

# alle variabler som started med part handler om participation i forskjellige studier/questionares

###################### Check if have unique list of persons

# No NA in project personidentification 
sum(is.na(data$PID.108676))

# all unique 
unique(data$PID.108676)

# all start with 108676
sum(data$PID.108676[grepl(108676,data$PID.108676)]!=data$PID.108676)

# remove the thing that is equal for everyone?
test.pid <- sub("108676","",data$PID.108676)

########################### MISSING VALUES ############3
# data
na.vec <- is.na(data)
colSums(na.vec) # vector containing number of missing values for each variable in data
mean(colSums(na.vec))

# total number of missing values in data
sum(na.vec)

plot_missing(data) # A lot of missing data



#### FIND WHO IS ON BPMED AT HUNT3

# Data tillegg innehåller en extra variabel från HUNT3CVD som ni kan använda i kombination 
# med en variabel från HUNT3BLQ1 för att se vilka som använder blodtrycksmedisin vid HUNT3
data.bp.med <- read.spss("Data/2019-12-19_108676_Data_tillegg.sav", to.data.frame=T)

#### data.bp.med

# Extract relevant variables, namely PID and BPMedSiEffEv 

data.bp.med <- data.frame("PID.108676"= data.bp.med$PID.108676, "BPMedSiEffEv.NT3CvdQ"=data.bp.med$BPMedSiEffEv.NT3CvdQ)

# Find list of PID of people on blodd pressure medicine at HUNT3
index.curr.bp.med <- !is.na(data.bp.med$BPMedSiEffEv.NT3CvdQ)



