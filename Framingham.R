

source("R code/EDA.R")


# Framingham model 


BPHigMothEv <-df.total$BPHigMoth2== "Mor - h\xf8yt BT"
BPHigMothEv[is.na(df.total$BPHigMoth2)] <- FALSE

BPHigFathEv <-df.total$BPHigFath2== "Far - h\xf8yt BT"
BPHigFathEv[is.na(df.total$BPHigFath2)] <- FALSE


fram.risk.fun <- function(){
  score <- rep(0,length(df.total$PID))
  risk <- rep(0,length(df.total$PID))
  
  for(i in 1: length(df.total$PID)){
    
    # Step 1
    if(df.total$SystolicBP2[i]<110){
      score[i]=score[i]-4
    } else if(df.total$SystolicBP2[i]<114){
      score[i]=score[i]
    }else if(df.total$SystolicBP2[i]<119){
      score[i]=score[i]+2
    }else if(df.total$SystolicBP2[i]<124){
      score[i]=score[i]+4
    }else if(df.total$SystolicBP2[i]<129){
      score[i]=score[i]+6
    }else if(df.total$SystolicBP2[i]<134){
      score[i]=score[i]+8
    }else{
      score[i]=score[i]+10
    }
    
    
    # Step 2
    if(df.total$Sex[i]=="Female"){score[i]= score[i]+1}
    
    # Step 3
    if(df.total$BMI2[i]<25){
      score[i]=score[i]
    } else if(df.total$BMI2[i]<30){
      score[i]=score[i]+1
    }else{
      score[i]=score[i]+3
    }
    
    
    # Step 4
    
    if(df.total$BirthYear[i]>=1967){
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]-8
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]-3
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+3
      }else{
        score[i]=score[i]+6
      }
    } else if(df.total$BirthYear[i]>=1957){
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]-5
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]+2
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+5
      }else{
        score[i]=score[i]+7
      }
    } else if(df.total$BirthYear[i]>=1947){
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]-1
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]+3
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]+5
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+6
      }else{
        score[i]=score[i]+8
      }
    } else if(df.total$BirthYear[i]>=1937){
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]+3
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]+5
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]+7
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+8
      }else{
        score[i]=score[i]+9
      }
      
    } else if(df.total$BirthYear[i]>=1927){
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]+6
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]+8
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]+9
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+10
      }else{
        score[i]=score[i]+10
      }
    } else{
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]+10
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]+11
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]+11
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+11
      }else{
        score[i]=score[i]+11
      }
    }
    
    # Step 5
    if(df.total$Smoking2[i]=="Current"){score[i]= score[i]+1}
    
    # Step 6
    if(BPHigFathEv[i] & BPHigMothEv[i]){
      score[i]=score[i]+2
    } else if(BPHigFathEv[i]){
      score[i]=score[i]+1
    } else if(BPHigMothEv[i]){
      score[i]=score[i]+1
    }else{
      score[i]=score[i]
    }
    
    risk[i] <- switch(toString(score[i]), "-12"=0.22, "-11"=0.27, "-10"=0.31, "-9"=0.37,
                   "-8"=0.44,"-7"=0.52,"-6"=0.62,"-5"=0.73,"-4"=0.86,"-3"=1.02,"-2"=1.21,"-1"=1.43,"0"=1.69,
                   "1"=2.00,"2"=2.37,"3"=2.80,"4"=3.31,"5"=3.90,"6"=4.61,"7"=4.53,"8"=6.40,"9"=7.53,"10"=8.86,
                   "11"=10.40,"12"=12.20,"13"=14.28,"14"=16.68,"15"=19.43,"16"=22.58,"17"=26.14,"18"=30.16,"19"=34.63,"20"=39.55,
                   "21"=44.91,"22"=50.64,"23"=56.66,"24"=62.85,"25"=69.05,"26"=75.06,"27"=80.69,"28"=85.74)
  }
  
return(risk/100)
}



fram.risk <- fram.risk.fun()

mean(fram.risk)

hist(fram.risk)



######## adjust age #######
fram.risk.ad.age.fun <- function(){
  score <- rep(0,length(df.total$PID))
  risk <- rep(0,length(df.total$PID))
  
  for(i in 1: length(df.total$PID)){
    
    # Step 1
    if(df.total$SystolicBP2[i]<110){
      score[i]=score[i]-4
    } else if(df.total$SystolicBP2[i]<114){
      score[i]=score[i]
    }else if(df.total$SystolicBP2[i]<119){
      score[i]=score[i]+2
    }else if(df.total$SystolicBP2[i]<124){
      score[i]=score[i]+4
    }else if(df.total$SystolicBP2[i]<129){
      score[i]=score[i]+6
    }else if(df.total$SystolicBP2[i]<134){
      score[i]=score[i]+8
    }else{
      score[i]=score[i]+10
    }
    
    
    # Step 2
    if(df.total$Sex[i]=="Female"){score[i]= score[i]+1}
    
    # Step 3
    if(df.total$BMI2[i]<25){
      score[i]=score[i]
    } else if(df.total$BMI2[i]<30){
      score[i]=score[i]+1
    }else{
      score[i]=score[i]+3
    }
    
    
    # Step 4
    
    if((df.total$BirthYear[i]-7)>=1967){
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]-8
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]-3
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+3
      }else{
        score[i]=score[i]+6
      }
    } else if((df.total$BirthYear[i]-7)>=1957){
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]-5
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]+2
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+5
      }else{
        score[i]=score[i]+7
      }
    } else if((df.total$BirthYear[i]-7)>=1947){
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]-1
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]+3
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]+5
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+6
      }else{
        score[i]=score[i]+8
      }
    } else if((df.total$BirthYear[i]-7)>=1937){
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]+3
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]+5
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]+7
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+8
      }else{
        score[i]=score[i]+9
      }
      
    } else if((df.total$BirthYear[i]-7)>=1927){
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]+6
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]+8
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]+9
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+10
      }else{
        score[i]=score[i]+10
      }
    } else{
      if(df.total$DiastolicBP2[i]<70){
        score[i]=score[i]+10
      } else if(df.total$DiastolicBP2[i]<74){
        score[i]=score[i]+11
      }else if(df.total$DiastolicBP2[i]<79){
        score[i]=score[i]+11
      }else if(df.total$DiastolicBP2[i]<84){
        score[i]=score[i]+11
      }else{
        score[i]=score[i]+11
      }
    }
    
    # Step 5
    if(df.total$Smoking2[i]=="Current"){score[i]= score[i]+1}
    
    # Step 6
    if(BPHigFathEv[i] & BPHigMothEv[i]){
      score[i]=score[i]+2
    } else if(BPHigFathEv[i]){
      score[i]=score[i]+1
    } else if(BPHigMothEv[i]){
      score[i]=score[i]+1
    }else{
      score[i]=score[i]
    }
    
    risk[i] <- switch(toString(score[i]), "-12"=0.22, "-11"=0.27, "-10"=0.31, "-9"=0.37,
                      "-8"=0.44,"-7"=0.52,"-6"=0.62,"-5"=0.73,"-4"=0.86,"-3"=1.02,"-2"=1.21,"-1"=1.43,"0"=1.69,
                      "1"=2.00,"2"=2.37,"3"=2.80,"4"=3.31,"5"=3.90,"6"=4.61,"7"=4.53,"8"=6.40,"9"=7.53,"10"=8.86,
                      "11"=10.40,"12"=12.20,"13"=14.28,"14"=16.68,"15"=19.43,"16"=22.58,"17"=26.14,"18"=30.16,"19"=34.63,"20"=39.55,
                      "21"=44.91,"22"=50.64,"23"=56.66,"24"=62.85,"25"=69.05,"26"=75.06,"27"=80.69,"28"=85.74)
  }
  
  return(risk/100)
}


fram.risk.ad.age <- fram.risk.ad.age.fun()

mean(fram.risk.ad.age)

hist(fram.risk.ad.age)





