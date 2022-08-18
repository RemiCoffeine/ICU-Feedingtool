day<- 4
weight <- 90
hight <- 160
insulin <-9
current_cal_total <- 1200
needed_cal <- 0
needed_prot <- 0
protein_lost<- 0
phosphate <- 1 #in mmol
Propofol <- 2

feeds_selection <-1
total_makros_needed <- list(cal=0, prot=0, AS=0,ml_feed_h =0, ml_prot_h =0, proteinpowder_day=0, vitamins= "target_met")
Ml_Per_Hour <- 0
Missing_Protein <- 0
bmi <- weight/(hight/100)^2 
ibw <- 48.4 +77*(hight/100 -1.50)
dialysis <- 0
protein_source <- 12
feedingtime <-0

possible_lost_protein<- list("CVVHD"=0.6,"Genius"=2 )


feeds  <- as.data.frame(matrix(ncol=4, nrow=12))
colnames(feeds) <- c("product_name", "kalories", "protein", "water")
feeds[1,1] <- "Fresubin"
feeds[2,1] <- "Fresubin_Energy"
feeds[3,1] <- "Propofol_2%_1mg"
feeds[4,1] <- "SK_hepatisch"
feeds[5,1] <- "SK_jejunal"
feeds[6,1] <- "SK_renal_low_protein"
feeds[7,1] <- "SK_renal_high_protein"
feeds[8,1] <- "proteinpowder"


feeds[1,2] <- 1
feeds[2,2] <- 1.5
feeds[3,2] <- .045
feeds[4,2] <- 1.3
feeds[5,2] <- 1.33
feeds[6,2] <- 2
feeds[7,2] <- 2
feeds[8,2] <- 3.6

feeds[1,3] <- 0.04
feeds[2,3] <- 0.06
feeds[3,3] <- 0
feeds[4,3] <- 0.04
feeds[5,3] <- 0.067
feeds[6,3] <- 0.03
feeds[7,3] <- 0.075
feeds[8,3] <- 0.87



feeds[9,1] <- "Olimel_57"
feeds[10,1] <- "olimel_76"
feeds[11,1] <- "Smovkabiven"
feeds[12,1] <- "Aminoplasmal"


feeds[9,2] <- 1.07
feeds[10,2] <- 0.95
feeds[11,2] <- 0.7
feeds[12,2] <- 0.4

feeds[9,3] <- 0.057
feeds[10,3] <- 0.076
feeds[11,3] <- 0.03
feeds[12,3] <- 0.1



library("dplyr") 


day1 <- function(weight, bmi,ibw){
  if (bmi <30) {
    total_makros_needed$prot <- weight*0.75
    total_makros_needed$AS <- weight*0.85
    total_makros_needed$cal <- weight *16
    return (total_makros_needed)
  }
  
  

if (between(bmi,30,50)) {
  total_makros_needed$prot <- ibw*1
  total_makros_needed$AS <- ibw*1.2
  total_makros_needed$cal <- weight *9
  return (total_makros_needed)
}

  if (bmi >50) {
    total_makros_needed$prot <- ibw*1
    total_makros_needed$AS <- ibw*1.2
    total_makros_needed$cal <- ibw *18
    return (total_makros_needed)
  }
  
}


day2 <- function(weight, insulin, hight,bmi, ibw){
  #bmi unter 30
  if (bmi <30) {
    if (insulin <= 1) {
      total_makros_needed$cal <- weight *24
      total_makros_needed$prot <- weight*1
      total_makros_needed$AS <- weight*1.2
    } 
    if ( between(insulin, 2, 4)) {
      total_makros_needed$cal <- weight *12
      total_makros_needed$prot <- weight*0.5
      total_makros_needed$AS <- weight*0.6
    }
    if (insulin > 4) {
      total_makros_needed$cal <- weight *6
      total_makros_needed$prot <- weight*0.25
      total_makros_needed$AS <- weight*0.35
    } 
    
    print("under 30")
    
    return(total_makros_needed)
  }
  
  #bmi zwischen 30-50
  if (between(bmi,30,50)) {
    if (insulin <= 1) {
      total_makros_needed$cal <- weight * 12
      total_makros_needed$prot <- ibw*1.5
      total_makros_needed$AS <- ibw*1.8
    } 
    if ( between(insulin, 2, 4)) {
      total_makros_needed$cal <- weight *6
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    }
    if (insulin > 4) {
      total_makros_needed$cal <- weight *3
      total_makros_needed$prot <- ibw*0.65
      total_makros_needed$AS <- ibw*0.8
    } 
    
    
    return(total_makros_needed)
  }
  #bmi above 50
  if (bmi >50) {
    if (insulin <= 1) {
      total_makros_needed$cal <- ibw *25
      total_makros_needed$prot <- ibw*1.5
      total_makros_needed$AS <- ibw*1.8
    } 
    if ( between(insulin, 2, 4)) {
      total_makros_needed$cal <- ibw *18
      total_makros_needed$prot <- ibw*1.2
      total_makros_needed$AS <- ibw*1.5
    }
    if (insulin > 4) {
      total_makros_needed$cal <- ibw *8
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    } 
    
    
    return(total_makros_needed)
  }
  
  
}



dayX <- function(weight, insulin, hight,bmi, ibw, current_cal_total, phosphate){
  if (bmi <30) {
    
    if ((insulin-1) <= 1) {
      total_makros_needed$cal <- current_cal_total + 4 * weight
      total_makros_needed$prot <- weight*1
      total_makros_needed$AS <- weight*1.2
      
      if (current_cal_total/weight >= 24){
        total_makros_needed$cal <- weight * 24
      }
    }
    if ( between((insulin-1), 2, 4)) {
      if (current_cal_total/weight >= 24){
        current_cal_total <- weight * 24
      }
      
      total_makros_needed$cal <- current_cal_total -(weight *4)
      total_makros_needed$prot <- weight*0.5
      total_makros_needed$AS <- weight*0.6
    }
    if ((insulin-1) > 4) {
      if (current_cal_total/weight >= 24){
        current_cal_total <- weight * 24
      }
      total_makros_needed$cal <- current_cal_total -(weight *12)
      total_makros_needed$prot <- weight*0.25
      total_makros_needed$AS <- weight*0.35
    } 
    
    if (phosphate < 0.65 ){
      total_makros_needed$cal <- weight * 6
      total_makros_needed$prot <- weight*0.25
      total_makros_needed$AS <- weight*0.35
    }
    
    
    
    return(total_makros_needed) 
  }
  #adapting with half the normal rate 
  if (between(bmi,30,50)) {
    
    if ((insulin-1) <= 1) {
      total_makros_needed$cal <- current_cal_total + 2 * weight
      total_makros_needed$prot <- ibw*1.5
      total_makros_needed$AS <- ibw*1.8
      
      if (current_cal_total/weight >= 12){
        total_makros_needed$cal <- weight * 12
      }
    }
    if ( between((insulin-1), 2, 4)) {
      if (current_cal_total/weight >= 12){
        current_cal_total <- weight * 12
      }
      
      total_makros_needed$cal <- current_cal_total -(weight *2)
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    }
    if ((insulin-1) > 4) {
      if (current_cal_total/weight >= 12){
        current_cal_total <- weight * 12
      }
      total_makros_needed$cal <- current_cal_total -(weight *6)
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    } 
    
    if (phosphate < 0.65 ){
      total_makros_needed$cal <- weight * 6
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    }
    
    
    return(total_makros_needed) 
  }
  
  
  #adapting with half the normal rate 
  if (bmi >50) {
    
    if ((insulin-1) <= 1) {
      total_makros_needed$cal <- current_cal_total + 4 * ibw
      total_makros_needed$prot <- ibw*1.5
      total_makros_needed$AS <- ibw*1.8
      
      if (current_cal_total/ibw >= 25){
        total_makros_needed$cal <- ibw * 25
      }
    }
    if ( between((insulin-1), 2, 4)) {
      if (current_cal_total/weight >= 12){
        current_cal_total <- ibw * 12
      }
      total_makros_needed$cal <- current_cal_total -(ibw *4)
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    }
    if ((insulin-1) > 4) {
      if (current_cal_total/weight >= 12){
        current_cal_total <- ibw * 12
      }
      total_makros_needed$cal <- current_cal_total -(ibw *12)
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    } 
    
    if (phosphate < 0.65 ){
      total_makros_needed$cal <- ibw * 6
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    }
    
    
    return(total_makros_needed) 
  }
  
  
}

day7_14 <- function(weight, insulin, hight,bmi, ibw, current_cal_total, phosphate){
  if (bmi <30) {
    
    if ((insulin-1) <= 1) {
      total_makros_needed$cal <- current_cal_total + 4 * weight
      total_makros_needed$prot <- weight*1
      total_makros_needed$AS <- weight*1.2
      
      if (current_cal_total/weight >= 30){
        total_makros_needed$cal <- weight * 30
      }
    }
    if ( between((insulin-1), 2, 4)) {
      if (current_cal_total/weight >= 24){
        current_cal_total <- weight * 24
      }
      
      total_makros_needed$cal <- current_cal_total -(weight *4)
      total_makros_needed$prot <- weight*0.5
      total_makros_needed$AS <- weight*0.6
    }
    if ((insulin-1) > 4) {
      if (current_cal_total/weight >= 24){
        current_cal_total <- weight * 24
      }
      total_makros_needed$cal <- current_cal_total -(weight *12)
      total_makros_needed$prot <- weight*0.25
      total_makros_needed$AS <- weight*0.35
    } 
    
    if (phosphate < 0.65 ){
      total_makros_needed$cal <- weight * 6
      total_makros_needed$prot <- weight*0.25
      total_makros_needed$AS <- weight*0.35
    }
    
    
    
    return(total_makros_needed) 
  }
  #adapting with half the normal rate 
  if (between(bmi,30,50)) {
    
    if ((insulin-1) <= 1) {
      total_makros_needed$cal <- current_cal_total + 2 * weight
      total_makros_needed$prot <- ibw*1.5
      total_makros_needed$AS <- ibw*1.8
      
      if (current_cal_total/weight >= 12){
        total_makros_needed$cal <- weight * 12
      }
    }
    if ( between((insulin-1), 2, 4)) {
      if (current_cal_total/weight >= 12){
        current_cal_total <- weight * 12
      }
      
      total_makros_needed$cal <- current_cal_total -(weight *2)
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    }
    if ((insulin-1) > 4) {
      if (current_cal_total/weight >= 12){
        current_cal_total <- weight * 12
      }
      total_makros_needed$cal <- current_cal_total -(weight *6)
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    } 
    
    if (phosphate < 0.65 ){
      total_makros_needed$cal <- weight * 6
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    }
    
    
    return(total_makros_needed) 
  }
  
  
  #adapting with half the normal rate 
  if (bmi >50) {
    
    if ((insulin-1) <= 1) {
      total_makros_needed$cal <- current_cal_total + 4 * ibw
      total_makros_needed$prot <- ibw*1.5
      total_makros_needed$AS <- ibw*1.8
      
      if (current_cal_total/ibw >= 25){
        total_makros_needed$cal <- ibw * 25
      }
    }
    if ( between((insulin-1), 2, 4)) {
      if (current_cal_total/weight >= 12){
        current_cal_total <- ibw * 12
      }
      total_makros_needed$cal <- current_cal_total -(ibw *4)
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    }
    if ((insulin-1) > 4) {
      if (current_cal_total/weight >= 12){
        current_cal_total <- ibw * 12
      }
      total_makros_needed$cal <- current_cal_total -(ibw *12)
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    } 
    
    if (phosphate < 0.65 ){
      total_makros_needed$cal <- ibw * 6
      total_makros_needed$prot <- ibw*0.75
      total_makros_needed$AS <- ibw*0.9
    }
    
    
    return(total_makros_needed) 
  }
  
  
}









ml_per_hour<- function(output, feeds, feeds_selection, feedingtime) {
  
  
  out<-(output$cal / feeds$kalories[feeds_selection])/feedingtime 
  
  return (out)
}







missing_protein<- function(dialysis,possible_lost_protein, output,hours_per_day_dialysis, feeds, feeds_selection, protein_source, feedingtime){
  if (dialysis == "1" ){ protein_lost <- (hours_per_day_dialysis * possible_lost_protein$CVVHD )}
  if (dialysis == "2" ){ protein_lost <- (hours_per_day_dialysis * possible_lost_protein$Genius )}
  
  protein_in_feed <- (output$ml_feed_h * feeds$prot[feeds_selection])*feedingtime
  needed_protein <- (output$prot +protein_lost)  - protein_in_feed 
  if (protein_source == 8) {
  output$proteinpowder_day <- needed_protein/feeds$prot[protein_source]
  }
  if ((needed_protein/feeds$prot[protein_source]) <0) {
    output$ml_prot_h <- 0
    output$proteinpowder_day <- 0
    
  }
  if (protein_source == 12) {
    needed_protein <- output$AS - protein_in_feed
    output$ml_prot_h <- (needed_protein/feeds$prot[protein_source])/feedingtime
    
  }
  
  
  return(output)
  
}


#execution

makros_needed <- function(protein_source,dialysis, hours_per_day_dialysis, possible_lost_protein,day, weight, hight, insulin, current_cal_total, phosphate, output, feeds, feeds_selection , Propofol, feedingtime =23 ){
  
  bmi <- weight/(hight/100)^2 
  ibw <- 48.4 +77*(hight/100 -1.50)
  
  if (day==0)
  {
    output <- total_makros_needed
    return(output)
  }
  
  if (day==1)
  {
    output <- day1(weight, bmi,ibw)
  }
  if (day==2)
  {
    output <- day2(weight, insulin, hight, bmi, ibw)
    print(bmi)
  }
  if (between(day,3,7)) {
    output <- dayX(weight, insulin, hight,bmi, ibw, current_cal_total, phosphate)
  }
  
  if (between(day,8,15)) {
    output <- day7_14(weight, insulin, hight,bmi, ibw, current_cal_total, phosphate)
  }
  
  if (day>15) {
    output <- dayX(weight, insulin, hight,bmi, ibw, current_cal_total, phosphate)
  }
  
  
  output$cal <- output$cal - (feeds$kalories[3]*Propofol)*weight*24
  if (output$cal < 0)
  {output$cal <-0
  output$AS <-0
  output$prot <-0
  }
  
  output$ml_feed_h<- ml_per_hour(output, feeds, feeds_selection, feedingtime)
  output<- missing_protein(dialysis,possible_lost_protein, output,hours_per_day_dialysis, feeds, feeds_selection, protein_source, feedingtime)
  if (output$cal <1500) {
    output$vitamins <- "Nicht erreicht!"
  }
  
  stats <- as.data.frame(list(BMI=bmi,IBW= ibw, Gesamtkalorienziel  = output$cal))
  
  #add proteins to caloric goal
  output$cal <- output$cal + (output$proteinpowder_day *feeds$kalories[8]) + (output$ml_prot_h *feeds$kalories[12])*feedingtime
  
  result_fooddata <- as.data.frame(output[1:3])
  result_fooddosage <- as.data.frame(output[4:6])
  result_vitamins <- as.data.frame(output[7])
 
  names(result_fooddata) <- c("Kalorien pro Tag", "Gesamtprotein pro Tag", " Aminos. pro Tag")
  names(result_fooddosage) <-c( "Flussrate Ernaehrung ml pro h", "Flussrate Aminoplasmal ml pro h", "Gramm Proteinpulver am Tag")
  names(result_vitamins)<- c("Vitaminziel")
  forShiny <- list(result_fooddata,result_fooddosage,result_vitamins, stats)
  
  return (forShiny)
}

makros_needed(protein_source,dialysis, hours_per_day_dialysis, possible_lost_protein,day, weight, hight, insulin, current_cal_total, phosphate, output, feeds, feeds_selection,Propofol)


 





