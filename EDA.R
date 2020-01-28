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


