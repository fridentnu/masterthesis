

source("R code/EDA.R")


# Framingham model 


BPHigMothEv <-df.total$BPHigMoth2== "Mor - h\xf8yt BT"
BPHigMothEv[is.na(df.total$BPHigMoth2)] <- FALSE

BPHigFathEv <-df.total$BPHigFath2== "Far - h\xf8yt BT"
BPHigFathEv[is.na(df.total$BPHigFath2)] <- FALSE


fram.score <- function(){
  score <- rep(0,length(df.total$PID))
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
  }

return(score)
}

fram.scores <- fram.score()

mean(fram.scores)


# Need to add risk in percentage from table in framingham

