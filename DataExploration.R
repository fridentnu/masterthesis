library(foreign)
#library(tibble)
library(DataExplorer)


##### OBS: HUSK Å LAGRE KILDER
# https://towardsdatascience.com/exploratory-data-analysis-8fc1cb20fd15
# https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619 Data explorer


# OBS: husk at man må bruke ctrl isteden for cmd på hurtigtastene
# use viridis colours, colour-blind and printer friendly
# save the random seed if use one!


########## LOAD DATA #############

# Data Study Cohort I är datafilen. Denna fil har inte PAI utrräknat, men återkommer med de variablerna ASAP.
data <- read.spss("Data/2019-03-26_108676_Data Study I Cohort.sav", to.data.frame=T)
# får feilmelding om duplicated levels men når jeg sjekker levels så er de unike


# Remove all variables from HUNT1
indexNT1.data <- grepl('NT1',colnames(data))
data <- data[!indexNT1.data]


# Data tillegg innehåller en extra variabel från HUNT3CVD som ni kan använda i kombination 
# med en variabel från HUNT3BLQ1 för att se vilka som använder blodtrycksmedisin vid HUNT3
data.bp.med <- read.spss("Data/2019-12-19_108676_Data_tillegg.sav", to.data.frame=T)

# Remove all variables from HUNT1
indexNT1.data.bp.med <- grepl('NT1',colnames(data.bp.med))
data.bp.med <- data.bp.med[!indexNT1.data.bp.med]

############# BASIC ANALYSIS #################

# 78 962 observasjoner av 232 variabler
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

########################### MISSING VALUES ############3
# data
na.vec <- is.na(data)
colSums(na.vec) # vector containing number of missing values for each variable in data
mean(colSums(na.vec))

# total number of missing values in data
sum(na.vec)

plot_missing(data) # A lot of missing data

### Missing values in data.bp.med
# data.bp.med
na.vec.bp.med <- is.na(data.bp.med)
colSums(na.vec.bp.med) # vector containing number of missing values for each variable in data
mean(colSums(na.vec.bp.med))

# total number of missing values in data
sum(na.vec.bp.med)
 
plot_missing(data.bp.med) # A lot of missing data






