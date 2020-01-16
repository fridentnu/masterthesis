library(foreign)
library(tibble)

# OBS: husk at man må bruke ctrl isteden for cmd på hurtigtastene
# use viridis colours, colour-blind and printer friendly
# save the random seed if use one!

    
# Data Study Cohort I är datafilen. Denna fil har inte PAI utrräknat, men återkommer med de variablerna ASAP.
data <- read.spss("Data/2019-03-26_108676_Data Study I Cohort.sav", to.data.frame=T)

# Remove all variables from HUNT1
indexNT1.data <- grepl('NT1',colnames(data))
data <- data[!indexNT1.data]


# Data tillegg innehåller en extra variabel från HUNT3CVD som ni kan använda i kombination 
# med en variabel från HUNT3BLQ1 för att se vilka som använder blodtrycksmedisin vid HUNT3
data.bp.med <- read.spss("Data/2019-12-19_108676_Data_tillegg.sav", to.data.frame=T)

# Remove all variables from HUNT1
indexNT1.data.bp.med <- grepl('NT1',colnames(data.bp.med))
data.bp.med <- data.bp.med[!indexNT1.data.bp.med]

# 78 962 observasjoner av 232 variabler
glimpse(data)

# 106 426 observasjoner av 11 variabler 
glimpse(data.bp.med)

# 232 variables
var.names.data <-colnames(data)

# 11 variables
var.names.tillegg <- colnames(data.bp.med)


# No NA in project personidentification
sum(is.na(data$PID.108676))

# all unique 
unique(data$PID.108676)


## Missing values in data

na.vec <- is.na(data)

colSums(na.vec) # vector containing number of missing values for each variable in data

mean(colSums(na.vec))

# total number of missing values in data
sum(na.vec)


### Missing values in data.bp.med
na.vec.bp.med <- is.na(data.bp.med)

colSums(na.vec.bp.med) # vector containing number of missing values for each variable in data

mean(colSums(na.vec.bp.med))

# total number of missing values in data
sum(na.vec.bp.med)


