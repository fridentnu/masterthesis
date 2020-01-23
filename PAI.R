

#data <- read.spss("Data/2019-03-26_108676_Data Study I Cohort.sav", to.data.frame=T)

# PAI= personal activity intelligence

### CALCULATE PAI BASED ON LEISURE TIME PA ###

## HUNT2 ##


Sex <- data$Sex

#How has your physical activity in leisure time been during the last year?
#Average of hours of light physical activity per week in the last year: ExeLigDuLY_NT2BLQ1
ExeLigDur <- data$ExeLigDuLY.NT2BLQ1
levels(ExeLigDur) <-c(0, 30, 90, 180) #Median Exercise Duration [min]
ExeLigDur <- as.numeric(levels(ExeLigDur))[ExeLigDur] #"Minutes of Light PA per Week"
#Exercise Frequency is 1 

#Average of hours of vigorous physical activity per week in the last year: ExeHarDuLY_NT2BLQ1
ExeHarDur <- data$ExeHarDuLY.NT2BLQ1
levels(ExeHarDur) <-c(0, 30, 90, 180) #Median Exercise Duration [min]
ExeHarDur <- as.numeric(levels(ExeHarDur))[ExeHarDur] #Minutes of Vigorous PA per Week
#Exercise Frequency is 1 

ExeLigInt <- ((0.44-0.2)/0.8) #Light Exercise Intensity Corresponds to 44% of Maximum Heart Rate 
ExeHarInt <-((0.73-0.2)/0.8) #Vigorous Exercise Intensity Corresponds to 75% of Maximum Heart Rate


#Put the new variables in a data frame
df <- data.frame(Sex, ExeLigDur, ExeHarDur)

# PAI algoritm #
#Step 1: Normzalize exercise intensity with sex specific coefficients 
#Produces an Activity Score [AS] for light and vigorous physical activity.  
df$ASLig <- ifelse(Sex == "Male", (0.00001*10.1817*(exp(5.7808*ExeLigInt)-1)*ExeLigDur), (0.00001*9.8556*(exp(5.0167*ExeLigInt)-1)*ExeLigDur))
df$ASHar <- ifelse(Sex == "Male", (0.00001*10.1817*(exp(5.7808*ExeHarInt)-1)*ExeHarDur), (0.00001*9.8556*(exp(5.0167*ExeHarInt)-1)*ExeHarDur))

#Step 2: Combine AS for light and vigorous physical activity.
df$AS <- rowSums(df[, c("ASLig", "ASHar")], na.rm = TRUE)
Idx = is.na(df$ASLig) & is.na(df$ASHar) #NA Position 
df$AS[Idx] = NA

#Step 3: Calculate Health Predictive Activity Score (HPS) with sex specific cofficients 
#Cofficients HPS represents offset and decay of cardiorespiratory fitness improvement. 
df$HPS <- ifelse(data$Sex == "Male", (41.9374+9.8382*(1-exp(-df$AS))), (34.2325+9.7581*(1-exp(-df$AS))))

attach(df)
tapply(HPS, Sex, min, na.rm=T)

#Step 4: Calulate PAI
PAI_NT2 <- round(ifelse(Sex == "Male", ((100*(HPS-41.9374)/(44-41.9374))),(100*(HPS-34.2325)/(35.4-34.2325))))
PAIlevel_NT2 <- cut(PAI_NT2, breaks=c(-Inf,49, 99, Inf), 
                    labels=c("Low","Moderate", "High") ) 
detach(df)



## HUNT3 ##

#How has your physical activity in leisure time been during the last year?
#Average of hours of light physical activity per week in the last year: ExeLigDuLY_NT2BLQ2
ExeLigDur <- data$ExeLigDuLY.NT3BLQ2
levels(ExeLigDur) <-c(0, 30, 90, 180) #Median Exercise Duration [min]
ExeLigDur <- as.numeric(levels(ExeLigDur))[ExeLigDur] #"Minutes of Light PA per Week"
#Exercise Frequency is 1 

#Average of hours of vigorous physical activity per week in the last year: ExeHarDuLY_NT2BLQ2
ExeHarDur <- data$ExeHarDuLY.NT3BLQ2
levels(ExeHarDur) <-c(0, 30, 90, 180) #Median Exercise Duration [min]
ExeHarDur <- as.numeric(levels(ExeHarDur))[ExeHarDur] #Minutes of Vigorous PA per Week
#Exercise Frequency is 1 

ExeLigInt <- ((0.44-0.2)/0.8) #Light Exercise Intensity Corresponds to 44% of Maximum Heart Rate 
ExeHarInt <-((0.73-0.2)/0.8) #Vigorous Exercise Intensity Corresponds to 75% of Maximum Heart Rate


#Put the new variables in a data frame
df <- data.frame(Sex, ExeLigDur, ExeHarDur)

# PAI algoritm #
#Step 1: Normzalize exercise intensity with sex specific coefficients 
#Produces an Activity Score [AS] for light and vigorous physical activity.  
df$ASLig <- ifelse(Sex == "Male", (0.00001*10.1817*(exp(5.7808*ExeLigInt)-1)*ExeLigDur), (0.00001*9.8556*(exp(5.0167*ExeLigInt)-1)*ExeLigDur))
df$ASHar <- ifelse(Sex == "Male", (0.00001*10.1817*(exp(5.7808*ExeHarInt)-1)*ExeHarDur), (0.00001*9.8556*(exp(5.0167*ExeHarInt)-1)*ExeHarDur))

#Step 2: Combine AS for light and vigorous physical activity.
df$AS <- rowSums(df[, c("ASLig", "ASHar")], na.rm = TRUE)
Idx = is.na(df$ASLig) & is.na(df$ASHar) #NA Position 
df$AS[Idx] = NA

#Step 3: Calculate Health Predictive Activity Score (HPS) with sex specific cofficients 
#Cofficients HPS represents offset and decay of cardiorespiratory fitness improvement. 
df$HPS <- ifelse(Sex == "Male", (41.9374+9.8382*(1-exp(-df$AS))), (34.2325+9.7581*(1-exp(-df$AS))))

attach(df)
tapply(HPS, Sex, min, na.rm=T)

#Step 4: Calulate PAI
PAI_NT3 <- round(ifelse(Sex == "Male", ((100*(HPS-41.9374)/(44-41.9374))),(100*(HPS-34.2325)/(35.4-34.2325))))
PAIlevel_NT3 <- cut(PAI_NT3, breaks=c(-Inf,49, 99, Inf), 
                    labels=c("Low","Moderate", "High")) 

detach(df)

##############################################################################################

### CALCULATE PAI BASED ON EXERCISE FREQUENCY, INTENSITY AND DURATION ###

## HUNT 3 ##
#How often do you exercise? 
#ExeF_NT3BLQ1 
ExeF <- data$ExeF.NT3BLQ1 
levels(ExeF) <- c(0, 0.5, 1, 2.5, 5) #Exercise Frequency
ExeF <- as.numeric(levels(ExeF))[ExeF]  

#If you exercise as often as once or several times a week: How long do you exercise each time?
#ExeDu_NT3BLQ1
ExeDu <- data$ExeDu.NT3BLQ1
levels(ExeDu) <- c(7.5, 22.5, 45, 60) #Median Exercise Duration [Min]
ExeDu <- as.numeric(levels(ExeDu))[ExeDu]

#Calculate minutes of exercise per week
#NA on Exedu, but 0 or 0.5 on ExeF, are coded as 0 in ExeTime (i.e considered inactive)
ExeTime <- ifelse(ExeF == 0 | ExeF == 0.5, 0, ExeF*ExeDu)  

#If you exercise as often as once or several times a week:How hard do you exercise?
#ExeInt_NT3BLQ1
ExeInt <- data$ExeInt.NT3BLQ1
levels(ExeInt) <- c(0.44, 0.73, 0.83) #Low, Moderate and High Intensity Corresponds to 44%, 73% and 83% of HRmax
ExeInt <- as.numeric(levels(ExeInt))[ExeInt]
ExeInt <- (ExeInt-0.2)/0.8 #Rescaling
ExeInt <- ifelse(ExeTime == 0, 0, ExeInt) #Intensity is 0 when inactive

#Put the new variables in a data frame
df <- data.frame(Sex, ExeF, ExeDu, ExeTime, ExeInt)

# PAI algoritm #
#Step 1: Normzalize exercise intensity with sex specific coefficients 
#Produces an Activity Score (AS)   
df$AS <- ifelse(Sex == "Male", (0.00001*10.1817*(exp(5.7808*ExeInt)-1)*ExeTime), (0.00001*9.8556*(exp(5.0167*ExeInt)-1)*ExeTime))

#Step 2: Calculate Health Predictive Activity Score (HPS) with sex specific cofficients 
#Cofficients HPS represents offset and decay of cardiorespiratory fitness improvement. 
df$HPS <- ifelse(Sex == "Male", (41.9374+9.8382*(1-exp(-df$AS))), (34.2325+9.7581*(1-exp(-df$AS))))

attach(df)
tapply(HPS, Sex, min, na.rm=T)

#Step 4: Calulate PAI
PAI_NT3 <- round(ifelse(Sex == "Male", ((100*(HPS-41.9374)/(44-41.9374))),(100*(HPS-34.2325)/(35.4-34.2325))))
PAIlevel_NT3 <- cut(PAI_NT3, breaks=c(-Inf,49, 99, Inf), 
                    labels=c("Low","Moderate", "High")) 
detach(df)

