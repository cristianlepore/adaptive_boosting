create_graph <- function(gap) {
  
  result <- rep(0, 40)
  for(i in 1:length(gap)) {
    
    if(gap[i]>=-1 && gap[i]< -0.95)
      result[1]= result[1] + 1
    
    else if(gap[i]>=-0.95 && gap[i]< -0.90)
      result[2]= result[2] + 1
    
    else if(gap[i]>=-0.90 && gap[i]< -0.85)
      result[3]= result[3] + 1
    
    else if(gap[i]>=-0.85 && gap[i]< -0.80)
      result[4]= result[4] + 1
    
    else if(gap[i]>=-0.80 && gap[i]< -0.75)
      result[5]= result[5] + 1
    
    else if(gap[i]>=-0.75 && gap[i]< -0.70)
      result[6]= result[6] + 1
    
    if(gap[i]>=-0.70 && gap[i]< -0.65)
      result[7]= result[7] + 1
    
    else if(gap[i]>=-0.65 && gap[i]< -0.60)
      result[8]= result[8] + 1
    
    else if(gap[i]>=-0.60 && gap[i]< -0.55)
      result[9]= result[9] + 1
    
    else if(gap[i]>=-0.55 && gap[i]< -0.50)
      result[10]= result[10] + 1
    
    else if(gap[i]>=-0.50 && gap[i]< -0.45)
      result[11]= result[11] + 1
    
    else if(gap[i]>=-0.45 && gap[i]< -0.40)
      result[12]= result[12] + 1
    
    else if(gap[i]>=-0.40 && gap[i]< -0.35)
      result[13]= result[13] + 1
    
    else if(gap[i]>=-0.35 && gap[i]< -0.30)
      result[14]= result[14] + 1
    
    else if(gap[i]>=-0.30 && gap[i]< -0.25)
      result[15]= result[15] + 1
    
    else if(gap[i]>=-0.25 && gap[i]< -0.20)
      result[16]= result[16] + 1
    
    else if(gap[i]>=-0.20 && gap[i]< -0.15)
      result[17]= result[17] + 1
    
    else if(gap[i]>=-0.15 && gap[i]< -0.10)
      result[18]= result[18] + 1
    
    else if(gap[i]>=-0.10 && gap[i]< -0.05)
      result[19]= result[19] + 1
    
    else if(gap[i]>=-0.05 && gap[i]< 0)
      result[20]= result[20] + 1
    
    else if(gap[i]>= 0 && gap[i]< 0.05)
      result[21]= result[21] + 1
    
    else if(gap[i]>= 0.05 && gap[i]< 0.10)
      result[22]= result[22] + 1
    
    else if(gap[i]>= 0.10 && gap[i]< 0.15)
      result[23]= result[23] + 1
    
    else if(gap[i]>= 0.15 && gap[i]< 0.20)
      result[24]= result[24] + 1
    
    else if(gap[i]>=-0.20 && gap[i]< 0.25)
      result[25]= result[25] + 1
    
    else if(gap[i]>= 0.25 && gap[i]< 0.30)
      result[26]= result[26] + 1
    
    else if(gap[i]>= 0.30 && gap[i]< 0.35)
      result[27]= result[27] + 1
    
    else if(gap[i]>= 0.35 && gap[i]< 0.40)
      result[28]= result[28] + 1
    
    else if(gap[i]>= 0.40 && gap[i]< 0.45)
      result[29]= result[29] + 1
    
    else if(gap[i]>= 0.45 && gap[i]< 0.50)
      result[30]= result[30] + 1
    
    else if(gap[i]>= 0.50 && gap[i]< 0.55)
      result[31]= result[31] + 1
    
    else if(gap[i]>= 0.55 && gap[i]< 0.60)
      result[32]= result[32] + 1
    
    else if(gap[i]>= 0.60 && gap[i]< 0.65)
      result[33]= result[33] + 1
    
    else if(gap[i]>= 0.65 && gap[i]< 0.70)
      result[34]= result[34] + 1
    
    else if(gap[i]>= 0.70 && gap[i]< 0.75)
      result[35]= result[35] + 1
    
    else if(gap[i]>= 0.75 && gap[i]< 0.80)
      result[36]= result[36] + 1
    
    else if(gap[i]>= 0.80 && gap[i]< 0.85)
      result[37]= result[37] + 1
    
    else if(gap[i]>= 0.85 && gap[i]< 0.90)
      result[38]= result[38] + 1
    
    else if(gap[i]>= 0.90 && gap[i]< 0.95)
      result[39]= result[39] + 1
    
    else if(gap[i]>= 0.95 && gap[i]<= 1)
      result[40]= result[40] + 1
    
  }
  return(result)
}