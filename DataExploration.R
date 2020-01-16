library(foreign)
library(tibble)

# OBS: husk at man må bruke ctrl isteden for cmd på hurtigtastene

data.tilleg <- read.spss("Data/2019-12-19_108676_Data_tillegg.sav", to.data.frame=T)
data <- read.spss("Data/2019-03-26_108676_Data Study I Cohort.sav", to.data.frame=T)

glimpse(data)
summary(data)

var.names <-colnames(data)
