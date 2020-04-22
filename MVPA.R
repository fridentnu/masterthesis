### Script fra Emma Ingström ###

# Based on article: http://www.sciencedirect.com/science/article/pii/S000293431500786X 

## Exercise ##

#Computing Moderate to Vigorous Physical Activity (MPVA) Score
#New variable: MVPA Score from Self-Reported Light Physical Activity
ExeLigScore_NT2 <- ordered(data.1$ExeLigDuLY.NT2BLQ1, c("Ingen", "Under 1 time", "1-2 timer", "3 timer eller mer"))
levels(ExeLigScore_NT2) <- c(0, 0.5, 1, 3)
ExeLigScore_NT2 <- as.numeric(levels(ExeLigScore_NT2)[ExeLigScore_NT2])


#New variable: MVPA Score from Self-Reported Hard Physical Activity"
ExeHarScore_NT2 <- ordered(data.1$ExeHarDuLY.NT2BLQ1, c("Ingen", "Under 1 time", "1-2 timer", "3 timer eller mer"))
levels(ExeHarScore_NT2) <-c(0, 1, 3, 6)
ExeHarScore_NT2 <- as.numeric(levels(ExeHarScore_NT2)[ExeHarScore_NT2])

#New variable: "MVPA Score from Self-Reported Physical Activity"
df <- data.frame(ExeLigScore_NT2, ExeHarScore_NT2)
MVPAScore_NT2 <- rowSums(df[,c("ExeLigScore_NT2", "ExeHarScore_NT2")], na.rm=TRUE)
Idx = is.na(df$ExeLigScore_NT2) & is.na(df$ExeHarScore_NT2)  #NA position in ExeLigScore_NT2 and ExeHarScore_NT2
MVPAScore_NT2[Idx] = NA #NA position in MVPAScore_NT2 is where ExeLigScore_NT2 AND ExeHarScore_NT2 are NA

#New variable: Meets PA recommendations
#Meeting PA recommendations is a MVPA Score >= 2.5
MeetsPARecomed_NT2 = MVPAScore_NT2 >= 2.5

