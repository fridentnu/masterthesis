library(funModeling) 
library(tidyverse) 
library(Hmisc)





# Sources
# https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619 Data explorer
# https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/
# https://r4ds.had.co.nz/exploratory-data-analysis.html 


# Load data set from DataCleaning
source("R code/DataCleaning.R")


# Overview of data set 
plot_str(df)


# More detailed overview
basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}

basic_eda(df)

describe(df)
# gmd= Gini mean distance, mean absolute difference between any pair of observations





########################## RESPONSE ##########################
# Correct the blood pressure values
BPSys3.uncorr <- df$BPSys3
BPSys3.uncorr[df.eval$BPMed3] <- df$BPSys3[df.eval$BPMed3]-15
BPDias3.uncorr <- df$BPDias3
BPDias3.uncorr[df.eval$BPMed3] <- df$BPDias3[df.eval$BPMed3]-10

# Look at response
plot_histogram(data.frame(df$BPSys3, df$BPDias3))


# Compare corrected and uncorrected blood pressure
plot_histogram(data.frame(BPSys3.uncorr,df$BPSys3))
plot_histogram(data.frame(BPDias3.uncorr,df$BPDias3))
#### ADD VERTICAL LINE HERE at 140 and 90? 




##################### EXPLANATORY VARIABLES #################

################# BASIC INFO ###############

# sex, age

################# BLOOD PRESSURE ###############

# systlic and diastolic bp at hunt2, hypertensive parents

################# LIFESTYLE ###############

# bmi, smoking, pai, recpa, educational level


################# BLOOD SAMPLES ###############

# grfe, creatinine, chol, hdl chol, blood glucose





# Continuous variables
plot_histogram(df)
plot_num(df)

# Correlation of the different continuous variables with the systolic bp from hunt3
plot_correlation(df, type="continuous")




# Categorical variables
plot_bar(df)
freq(df)


# Collection of EDA plots 
create_report(df)






# Overview of data set 
plot_str(df)

# Rename factors with special letters
levels(df$SmoStat)
levels(df$SmoStat)<- c("Aldri roeykt daglig", "Tidligere daglig roeyker", "Daglig roeyker")

levels(df$Educ)
levels(df$Educ)<- c("Grunnskole 7-10 aar, framhaldsskole, folkehoegskole", 
                    "Realskole, middelskole, yrkesskole 1-2 aarig vgs",
                    "Artium, oek.gymnas, allmennfaglig retning i vgs",
                    "Hoegskole/universitet, mindre enn 4 aar",             
                    "Hoegskole/universitet, 4 aar eller mer")



# Continuous variables
plot_histogram(df)

# Correlation of the different continuous variables with the systolic bp from hunt3
plot_correlation(df, type="continuous")


# Categorical variables
plot_bar(df)
freq(df)


# Collection of EDA plots 
#create_report(df)


