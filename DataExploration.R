library(foreign)
library(tibble)

# OBS: husk at man må bruke ctrl isteden for cmd på hurtigtastene
# use viridis colours, colour-blind and printer friendly
# save the random seed if use one!

# Data tillegg innehåller en extra variabel från HUNT3CVD som ni kan använda i kombination 
# med en variabel från HUNT3BLQ1 för att se vilka som använder blodtrycksmedisin vid HUNT3
data.tillegg <- read.spss("Data/2019-12-19_108676_Data_tillegg.sav", to.data.frame=T)

# Data Study Cohort I är datafilen. Denna fil har inte PAI utrräknat, men återkommer med de variablerna ASAP.
data <- read.spss("Data/2019-03-26_108676_Data Study I Cohort.sav", to.data.frame=T)

# 78 962 observasjoner av 236 variabler
glimpse(data)

# 106 426 observasjoner av 15 variabler 
glimpse(data.tillegg)

summary(data[,2])


# 236 variables
var.names.data <-colnames(data)

# 15 variables
var.names.tillegg <- colnames(data.tillegg)
