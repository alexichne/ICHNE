rm(list = ls())
.libPaths("C:/RLibrary")
require(dplyr)
require(readr)
require(tidyverse)
require(magrittr)
library(plyr)
require(lubridate)
library(markdown)
library(blastula)
library(keyring)

###  V1.2

########################################################################################################
######################## Please run email_kpi script after this one  ###################################
########################################################################################################

#database start and end date (YYMMDD)
timer_start <- Sys.time()
end_date = ymd(Sys.Date()-1)
start_date = ymd(Sys.Date()-4)
t1 <- gsub('-','',substr(end_date,3,10))

write.table_with_header <- function(x, file, header, ...){
  cat(header, '\n',  file = file)
  write.table(x, file, append = T, ...)
}
minToHour <- function(mins){
  return(paste((mins%/%60),":",(mins%%60),sep=""))
}

build_database <- function(start_date,end_date){
  j=as.numeric(end_date-start_date)
  t1 <- gsub('-','',substr(end_date,3,10))
  adb1file <- paste("Z:/Scripts/Data/SM_Data/",t1,"_adb1.csv",sep="")
  adb1 <- read.csv(adb1file)
  results1 <- paste("Z:/Scripts/Data/SM_Data/",t1,"_adb2.csv",sep="")
  results1 <- read.csv(results1)
  nuth1_1 <- paste("Z:/Scripts/Data/SM_Data/",t1,"_nuth1.csv",sep="")
  nuth1_1 <- read.csv(nuth1_1)
  #nuth1_1 <- nuth1_1 %>% select(-one_of("RecommendedAction"),-one_of("ParentPlateId"),-one_of("CompressionPlateWell"),-one_of("ParentPlateWell"))
  nuth2_1 <- paste("Z:/Scripts/Data/SM_Data/",t1,"_nuth2.csv",sep="")
  nuth2_1 <- read.csv(nuth2_1)
  #nuth2_1 <- nuth2_1 %>% select(-one_of("RecommendedAction"),-one_of("ParentPlateId"),-one_of("CompressionPlateWell"),-one_of("ParentPlateWell"))
  
  all_amp <- rbind(nuth1_1,nuth2_1)
  all_adb <- adb1
  all_results <- results1
  
  
  for (i in (1:j)){
    t_temp <- gsub('-','',substr((end_date-i),3,10))
    adb_temp <- paste("Z:/Scripts/Data/SM_Data/",t_temp,"_adb1.csv",sep="")
    adb_temp <- read.csv(adb_temp)
    results_temp <- paste("Z:/Scripts/Data/SM_Data/",t_temp,"_adb2.csv",sep="")
    results_temp <- read.csv(results_temp)
    all_adb <- rbind(all_adb,adb_temp)
    all_adb <- distinct(all_adb, CustomerId, .keep_all = T)
    all_results <- rbind(all_results,results_temp)
    all_results <- distinct(all_results, CustomerID, .keep_all = T)
    nuth1_temp <- paste("Z:/Scripts/Data/SM_Data/",t_temp,"_nuth1.csv",sep="")
    nuth1_temp <- read.csv(nuth1_temp)
    #nuth1_temp <- nuth1_temp %>% select(-one_of("RecommendedAction"),-one_of("ParentPlateId"),-one_of("CompressionPlateWell"),-one_of("ParentPlateWell"))
    nuth2_temp <- paste("Z:/Scripts/Data/SM_Data/",t_temp,"_nuth2.csv",sep="")
    nuth2_temp <- read.csv(nuth2_temp)
    #nuth2_temp <- nuth2_temp %>% select(-one_of("RecommendedAction"),-one_of("ParentPlateId"),-one_of("CompressionPlateWell"),-one_of("ParentPlateWell"))
    all_amp_temp <- rbind(nuth1_temp,nuth2_temp)
    all_amp <- rbind(all_amp,all_amp_temp)
    all_amp <- distinct(all_amp, CustomerReference, PreparationTime, .keep_all = T)
  }
  
  all_adb <- merge(all_adb,all_results,by.x ="CustomerId",by.y="CustomerID",  all.x = TRUE)
  
  # Create a new compression plate ID field in NPEX data
  # Take ID from results filename
  all_adb$CompressionPlateId=substr(all_adb$`AdbResultSourceFile`,19,26)
  
  # rename column for consistency between Amplitude and Databank
  all_amp <-  rename(all_amp, replace = c("DateCompleted" = "DateAmplitudeCompleted"))
  all_amp <- rename(all_amp, replace = c("CustomerReference" = "CustomerId"))
  
  #all_amp <- all_amp[(all_amp$CustomerId %in% all_adb$CustomerId)|all_amp$CustomerId=="NC1",]
  
  
  # Create new dataset, by merging Amplitude and Databank samples that 
  # share a barcode and compression plate
  all <- merge(all_amp,all_adb,by=c("CustomerId","CompressionPlateId"),all=TRUE)
  
  # remove useless columns
  all <- select(all,-X.x,-X.y,-X)
  
  # add retest field
  
  # format results
  all$Result[is.na(all$Result) & !is.na(all$NpexSentOn) & all$Status != "Cancelled"]<- "Invalid"
  all$Result[all$Result == "Positive SARS-CoV-2"] <- "Positive"
  all$Result[all$Result == "SARS-CoV-2 Inconclusive"] <- "Inconclusive"
  all$Result[all$Result == "SARS-CoV-2 Not Detected"] <- "Negative"
  
  
  # format dates
  all$LoginDate <- format(dmy_hm(all$LoginDate))
  all$DateAuthorised <- format(dmy_hm(all$DateAuthorised))
  all$AdbEnteredOvenOn <- format(dmy_hm(all$AdbEnteredOvenOn))
  all$AdbExitedOvenOn <- format(dmy_hm(all$AdbExitedOvenOn))
  all$DateAmplitudeCompleted <- format(dmy_hm(all$DateAmplitudeCompleted))
  all$DateCompleted <- format(dmy_hm(all$DateCompleted))
  all$PreparationTime <- format(dmy_hm(all$PreparationTime))
  all$DateStarted <- format(dmy_hm(all$DateStarted))
  all$AdbEnteredCabinet2On <- format(dmy_hm(all$AdbEnteredCabinet2On))
  all$AdbVoidConfirmedOn <- format(dmy_hm(all$AdbVoidConfirmedOn))
  all$NpexSentOn <- format(dmy_hm(all$NpexSentOn))
  all$AdbC19VoidReason <- gsub("[^0-9.]", "", substr(all$AdbC19VoidReason,1,2))
  all$QpcrInstrument <- gsub("[^0-9.]", "", substr(all$QpcrInstrument,8,9))
  
  #add time fields (all in minutes)
  all$TAT <- as.double(difftime(all$NpexSentOn,all$LoginDate,units="mins"))
  all$AmplitudeTime <- as.double(difftime(all$DateAmplitudeCompleted,all$PreparationTime,units="mins")) + 55
  all$SortingWait <- as.double(difftime(all$PreparationTime,all$AdbEnteredCabinet2On,units="mins")) - 55
  all$AuthorisationTime <- as.double(difftime(all$NpexSentOn, all$DateAmplitudeCompleted,units="mins"))
  all$OvenTime <- as.double(difftime(all$AdbExitedOvenOn,all$AdbEnteredOvenOn, units="mins"))
  all$PSP2Wait <- as.double(difftime(all$AdbEnteredCabinet2On, all$AdbExitedOvenOn, units="mins"))
  all$OvenWait <- as.double(difftime(all$AdbEnteredOvenOn,all$LoginDate, units="mins"))
  all <- arrange(all,DateAmplitudeCompleted)
  all <- distinct(all, CustomerId, PreparationTime, .keep_all = T)
  retests <- rle(sort(all$CustomerId))
  all$retests <- as.integer(retests[[1]][match(all$CustomerId, retests[[2]])]) - 1
  
  return(all)
}

add_voc <- function(db, sd, ed){
  
  # make a list with all Reflex result files for timeframe
  j=as.numeric(ed-sd)
  t1 <- gsub('-','',substr(ed,3,10))
  inputList <- list.files("Z:/Scripts/Data/VOC_results/", pattern = t1 , recursive = F, full.names = T, include.dirs = FALSE, ignore.case = TRUE)
  for (i in (1:j)){
    t_temp <- gsub('-','',substr((end_date-i),3,10))
    inputList <- c(inputList,list.files("Z:/Scripts/Data/VOC_results/", pattern = t_temp , recursive = F, full.names = T, include.dirs = FALSE, ignore.case = TRUE))
  }
  voc_data <- data.frame()
  for (i in inputList){
    x <- read.csv(i)
    voc_data <- rbind(x, voc_data)
  }
  
  voc_data <- select(voc_data, Sample, CH1.Result, Date.Tested)
  voc_data <- dplyr::rename(voc_data, CustomerId = Sample, Q493R = CH1.Result, VOCdate = Date.Tested)
  voc_data$VOCdate <- format(ymd_hms(voc_data$VOCdate))
  
  # merge results
  vocdb <- merge(db, voc_data, by = "CustomerId", all.x=T)
  vocdb$Q493R <- ifelse(is.na(vocdb$NpexSentOn),NA,vocdb$Q493R)
  vocdb$VOCdate <- ifelse(is.na(vocdb$NpexSentOn),NA,vocdb$VOCdate)
  #vocdb$VOCdate <- format(ymd_hms(vocdb$VOCdate))
  return(vocdb)
}


getInputFiles <- function(){
  
  # create list of files in the input folder
  inputList <- list.files("Z:/Scripts/Data/VOC_results/", pattern = t1 , recursive = F, full.names = T, include.dirs = FALSE, ignore.case = TRUE)
  #inputList <- paste("Input/", inputList, sep = "")
  archiveList <- inputList
  
  # for each file read and manipulate
  for(xfile in inputList) {
    currentFile <- read.csv(xfile)
    
    # put each sample set in a list
    currentFile <- select(currentFile, Sample, Date.Tested, CH1.Target, CH1.Result)
    sampleList <- split(currentFile, f = currentFile$Sample)
    
    # put useful results into output_DF
    for(xsample in sampleList){
      xsample <- xsample %>% pivot_wider(names_from = "CH1.Target", values_from = "CH1.Result")
      
      # shorten the date
      xsample$Date.Tested <- as.Date(xsample$Date.Tested)
      
      # compact tibble
      helpFunc <- function(x) x[!is.na(x)]
      xsample <- xsample %>% dplyr::summarise(across(.cols = everything(), .fns = helpFunc, .names = NULL))
      xsample <- distinct(xsample, .keep_all = FALSE)
      
      # output to global using << so we can use it later
      output_DF <<- bind_rows(xsample, output_DF)
    }
  }
  # make sure all values have something in
  output_DF[is.na(output_DF)] <<- "NA"
  output_DF <<- distinct(output_DF)
}


save_database <- function(date, db){
  t1 <- gsub('-','',substr(date,3,10))
  newDb <- subset(db, date(LoginDate) == date(date) | date(NpexSentOn) == date(date))
  filename <- paste("Z:/Scripts/Data/SM_data/",t1,"_database.csv",sep = "")
  write.csv(newDb,filename)                
}

# KPI functions 
backlog <- function(time,db){
  #number of entries in system at time
  bklg <- subset(db, LoginDate <= time)
  return(nrow(subset(bklg, NpexSentOn > time  | is.na(NpexSentOn) == T)))
}
runs <- function(st,et,db,bay){
  amplitudeUsage <- unique(select(db,QpcrInstrument,PreparationTime))
  amplitudeUsage$QpcrInstrument <- as.integer(amplitudeUsage$QpcrInstrument)
  amplitudeUsage$PreparationTime <- ymd_hms(amplitudeUsage$PreparationTime)
  amplitudeUsage <- subset(amplitudeUsage, PreparationTime-hms("0:55:0") >= st & PreparationTime-hms("0:55:0") <= et & !is.na(PreparationTime))
  return(nrow(subset(amplitudeUsage, QpcrInstrument==bay)))
}

delayed <- function(time, threshold ,db){
  #number of delayed samples in system at time
  delayed_samples <- subset(db, LoginDate <= time-hours(threshold))
  return(nrow(subset(delayed_samples, NpexSentOn > time  | is.na(NpexSentOn) == T)))
}
total_PSP1 <- function(start_time, end_time, db){
  return(nrow(subset(db, LoginDate >= start_time & LoginDate <= end_time)))
}
total_ovens <- function(start_time, end_time, db){
  return(nrow(subset(db, AdbEnteredOvenOn >= start_time & AdbEnteredOvenOn <= end_time)))
}
total_PSP2 <- function(start_time, end_time, db){
  return(nrow(subset(db, AdbEnteredCabinet2On >= start_time & AdbEnteredCabinet2On <= end_time)))
}
total_mod1 <- function(start_time, end_time, db){
  return(nrow(subset(db, DateStarted >= start_time & DateStarted <= end_time)))
}
total_resulted <- function(start_time, end_time, db){
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
total_10plus <- function(start_time, end_time, db){
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time & TAT > 600)))
}
total_15plus <- function(start_time, end_time, db){
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time & TAT > 900)))
}
total_18plus <- function(start_time, end_time, db){
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time & TAT > 1080)))
}

total_10blue <- function(start_time, end_time, db){
  db <- subset(db, substr(AdbOvenRack,1,2) == "BL")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time & TAT > 600)))
}
total_12blue <- function(start_time, end_time, db){
  db <- subset(db, substr(AdbOvenRack,1,2) == "BL")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time & TAT > 720)))
}
total_12orange <- function(start_time, end_time, db){
  db <- subset(db, substr(AdbOvenRack,1,2) == "OR")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time & TAT > 720)))
}
total_15orange <- function(start_time, end_time, db){
  db <- subset(db, substr(AdbOvenRack,1,2) == "OR")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time & TAT > 900)))
}

total_positives <- function(start_time, end_time, db){
  db <- subset(db, Result == "Positive")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
bluePositives <- function(start_time, end_time, db){
  db <- subset(db, Result == "Positive" & substr(AdbOvenRack,1,2) == "BL")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
orangePositives <- function(start_time, end_time, db){
  db <- subset(db, Result == "Positive" & substr(AdbOvenRack,1,2) == "OR")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
totalBlue <- function(start_time, end_time, db){
  db <- subset(db, substr(AdbOvenRack,1,2) == "BL")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
totalOrange <- function(start_time, end_time, db){
  db <- subset(db, substr(AdbOvenRack,1,2) == "OR")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
total_negatives <- function(start_time, end_time, db){
  db <- subset(db, Result == "Negative")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
total_inconclusives <- function(start_time, end_time, db){
  db <- subset(db, Result == "Inconclusive"| AdbVoidComments=="Inconclusive"| AdbVoidComments=="inconclusive")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
total_invalids <- function(start_time, end_time, db){
  db <- subset(db, Result == "Invalid")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
total_lab_voids <- function(start_time, end_time, db){
  db <- subset(db, AdbC19VoidReason == "10")
  db <- subset(db, AdbVoidComments!="Inconclusive" & AdbVoidComments!="inconclusive")
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
total_sq_voids <- function(start_time, end_time, db){
  db <- subset(db, AdbC19VoidReason %in% c("1","2","3","4","5","6","7","8","9"))
  return(nrow(subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)))
}
total_retests <- function(start_time,end_time, db){
  db <- subset(db,NpexSentOn >= start_time & NpexSentOn <= end_time)
  return(sum(db$retests,na.rm=T))
}
total_amp_runs <- function(start_time,end_time, db){
  db <- subset(db,DateAmplitudeCompleted >= start_time & DateAmplitudeCompleted <= end_time)
  return(length(unique(db$CompressionPlateId)))
}
total_in_time <- function(start_time,end_time, interval_s, interval_e,  db){
  db <- subset(db,NpexSentOn >= start_time & NpexSentOn <= end_time)
  return(nrow(subset(db, TAT < (60*interval_e) & TAT >= (60*interval_s))))
}
blue_in_time <- function(start_time,end_time, interval_s, interval_e,  db){
  db <- subset(db,NpexSentOn >= start_time & NpexSentOn <= end_time)
  db <- subset(db, substr(AdbOvenRack,1,2) == "BL")
  return(nrow(subset(db, TAT < (60*interval_e) & TAT >= (60*interval_s))))
}
orange_in_time <- function(start_time,end_time, interval_s, interval_e,  db){
  db <- subset(db,NpexSentOn >= start_time & NpexSentOn <= end_time)
  db <- subset(db, substr(AdbOvenRack,1,2) == "OR")
  return(nrow(subset(db, TAT < (60*interval_e) & TAT >= (60*interval_s))))
}
# determines which team was on shift.  WILL NEED UPDATING IN 2024
team <- function(sos){
  
  if (am(sos)){
    if (wday(sos) == 1|wday(sos) == 2|wday(sos) == 3){  
      if (epiweek(sos) %% 2 ==0 ) {return("B")}
      else {return("A")}
    }
    else if (wday(sos) == 5|wday(sos) == 6|wday(sos) == 7){
      if (epiweek(sos) %% 2 ==0) {return("D")}
      else {return("C")}
    }
    else if (wday(sos) == 4){
      if (epiweek(sos) %% 4 == 2|epiweek(sos) %% 4 == 3){
        if (epiweek(sos) %% 2 == 0){return("B")}
        else {return("A")}
      }
      else {
        if (epiweek(sos) %% 2 == 0) {return("D")}
        else {return("C")}
      }
    }
  }
  else if (pm(sos)){}
  if (wday(sos) == 1|wday(sos) == 2|wday(sos) == 3){  
    if (epiweek(sos) %% 2 ==0 ) {return("A")}
    else {return("B")}
  }
  else if (wday(sos) == 5|wday(sos) == 6|wday(sos) == 7){
    if (epiweek(sos) %% 2 ==0) {return("C")}
    else {return("D")}
  }
  else if (wday(sos) == 4){
    if (epiweek(sos) %% 4 == 2|epiweek(sos) %% 4 == 3){
      if (epiweek(sos) %% 2 == 0){return("A")}
      else {return("B")}
    }
    else {
      if (epiweek(sos) %% 2 == 0) {return("C")}
      else {return("D")}
    }
  }}

# VOC functions
Q493R_mt <- function(start_time, end_time, db){
  db <- subset(db, VOCdate >= start_time & VOCdate <= end_time)
  db <- subset(db, Q493R=="mt")
  return(nrow(db))
}

Q493R_wt <- function(start_time, end_time, db){
  db <- subset(db, VOCdate >= start_time & VOCdate <= end_time)
  db <- subset(db, Q493R=="wt")
  return(nrow(db))
}

Q493R_na <- function(start_time, end_time, db){
  db <- subset(db, VOCdate >= start_time & VOCdate <= end_time)
  db <- subset(db, Q493R=="No Amplification")
  return(nrow(db))
}


# CT functions
sGeneDropout <- function(start_time, end_time, db){
  db <- subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)
  db <- subset(db, Result=="Positive")
  return(nrow(subset(db, SgeneCt == -1 )))
}
nGeneDropout <- function(start_time, end_time, db){
  db <- subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)
  db <- subset(db, Result=="Positive")
  return(nrow(subset(db, NgeneCt == -1 )))
}
orfGeneDropout <- function(start_time, end_time, db){
  db <- subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)
  db <- subset(db, Result=="Positive")
  return(nrow(subset(db, Orf1abCt == -1 )))
}
medianMS2  <- function(start_time, end_time, db){
  db <- subset(db, DateAmplitudeCompleted >= start_time & Ms2Ct != -1 & NpexSentOn <= end_time & Result == "Positive")
  return(round(median(db$Ms2Ct,na.rm=TRUE),1))}
medianN  <- function(start_time, end_time, db){
  db <- subset(db, DateAmplitudeCompleted >= start_time & NgeneCt != -1 & NpexSentOn <= end_time & Result == "Positive")
  return(round(median(db$NgeneCt,na.rm=TRUE),1))}
medianS  <- function(start_time, end_time, db){
  db <- subset(db, DateAmplitudeCompleted >= start_time & SgeneCt != -1 & NpexSentOn <= end_time & Result == "Positive")
  return(round(median(db$SgeneCt,na.rm=TRUE),1))}
medianORF  <- function(start_time, end_time, db){
  db <- subset(db, DateAmplitudeCompleted >= start_time & Orf1abCt != -1 & NpexSentOn <= end_time & Result == "Positive")
  return(round(median(db$Orf1abCt,na.rm=TRUE),1))}


# TAT functions
TAT <- function(start_time, end_time, db){
  db <- subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)
  return(median(db$TAT,na.rm=TRUE))
}
meanTAT <- function(start_time, end_time, db){
  db <- subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time)
  db <- subset(db, TAT >=360)
  return(round(mean(df$TAT,na.rm=TRUE),0))
}
blueMeanTAT <- function(start_time, end_time, db){
  db <- subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time & substr(AdbOvenRack,1,2) == "BL")
  db <- subset(db, TAT >=360)
  return(round(mean(df$TAT,na.rm=TRUE),0))
}
orangeMeanTAT <- function(start_time, end_time, db){
  db <- subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time & substr(AdbOvenRack,1,2) == "OR")
  db <- subset(db, TAT >=360)
  return(round(mean(df$TAT,na.rm=TRUE),0))
}

ovenTAT <- function(start_time, end_time, db){
  db <- subset(db, AdbEnteredOvenOn >= start_time & AdbExitedOvenOn <= end_time)
  return(median(db$OvenTime,na.rm=TRUE))
}
mod1TAT <- function(start_time, end_time, db){
  db <- subset(db, AdbEnteredCabinet2On >= start_time & DateStarted <= end_time + minutes(55))
  return(median(db$SortingWait,na.rm=TRUE))
}
ampTAT <- function(start_time, end_time, db){
  db <- subset(db, DateStarted >= start_time + minutes(55) & DateAmplitudeCompleted <= end_time)
  return(median(db$AmplitudeTime,na.rm=TRUE))
}
PSP1TAT <- function(start_time, end_time, db){
  db <- subset(db, LoginDate >= start_time & AdbEnteredOvenOn <= end_time)
  return(median(db$OvenWait,na.rm=TRUE))
}
PSP2TAT <- function(start_time, end_time, db){
  db <- subset(db, AdbExitedOvenOn >= start_time & AdbEnteredCabinet2On <= end_time)
  return(median(db$PSP2Wait,na.rm=TRUE))
}
authTAT <- function(start_time, end_time, db){
  db <- subset(db, DateAmplitudeCompleted >= start_time & NpexSentOn <= end_time)
  return(median(db$AuthorisationTime,na.rm=TRUE))
}

psp1rate <- function(start_time, end_time, db){
  db$LoginDate <- ymd_hms(db$LoginDate)
  db <- subset(db, LoginDate >= start_time & LoginDate <= end_time)
  db <- subset(db, !is.na(AdbCabinet1))
  hoods <- unique(db$AdbCabinet1)
  count <- c(0)
  for (hood in hoods){
    bay <-subset(db, AdbCabinet1==hood)
    bay <- select(bay, LoginDate,LoginBy)
    bay <- unique(bay)
    bay <- bay[order(bay$LoginDate),]
    x <- as.numeric(diff(bay$LoginDate))
    count <- append(count, x)
  }
  #count <- count[count<30]
  #count <- count[count>4]
  return(median(count))
}
psp2rate <- function(start_time, end_time, db){
  db$AdbEnteredCabinet2On <- ymd_hms(db$AdbEnteredCabinet2On)
  db <- subset(db, AdbEnteredCabinet2On >= start_time & AdbEnteredCabinet2On <= end_time)
  db <- subset(db, !is.na(AdbCabinet2))
  hoods <- unique(db$AdbCabinet2)
  count <- c(0)
  for (hood in hoods){
    bay <-subset(db, AdbCabinet2==hood)
    bay <- select(bay, AdbEnteredCabinet2On,AdbEnteredCabinet2By)
    bay <- unique(bay)
    bay <- bay[order(bay$AdbEnteredCabinet2On),]
    x <- as.numeric(diff(bay$AdbEnteredCabinet2On))
    count <- append(count, x)
  }
  
  # count <- count[count<30]
  # count <- count[count>4]
  return(median(count))
}

sequenceable <- function(start_time,end_time,db){
  db <- subset(db, NpexSentOn >= start_time & NpexSentOn <= end_time  & Result == "Positive")
  db <- subset(db, Orf1abCt >0 & NgeneCt >0 & Orf1abCt <30 & NgeneCt <30)
  return(nrow(db))
}

omicron <- function(start_time,end_time,db){
  db <- subset(db,  NpexSentOn >= start_time & NpexSentOn <= end_time & Result == "Positive")
  db <- subset(db, Orf1abCt >0 & NgeneCt >0 & Orf1abCt <30 & NgeneCt <30 & SgeneCt == -1)
  return(nrow(db))
}

populate12 <- function(sd, shifts, db){
  sd <- sd + hms("08:00:00")
  ed <- sd + hms("11:59:59")
  df <- pullStats12(db,sd,ed)
  for (i in (1:shifts-1)){
    sd = sd + hm("12:0")
    ed = ed + hm("12:0")
    df <- rbind(df, pullStats12(db,sd,ed))
  }
  return(df)
}
populate24 <- function(sd, shifts, db){
  hour(sd) <- 0
  ed <- sd + hms("23:59:59")
  df <- data.frame()
  for (i in (1:shifts)){
    df <- rbind(df, pullStats(db,sd,ed))
    sd = sd + hm("24:0")
    ed = ed + hm("24:0")
  }
  return(df)
}
pullStats <- function(db,st,et){
  db <- subset(db, LoginDate <= et & LoginDate >= st | NpexSentOn <= et & NpexSentOn >= st | LoginDate <= st & NpexSentOn >= et | VOCdate >=st & VOCdate <= et)
  # Put all stats for shift into a dataframe
  df <-  data.frame(date = format(date(st),format="%d/%m/%Y"))
  days <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  df$day <- days[wday(st)]
  if (am(st)){ df$shift <- "Days"
  df$times <- "8am-8pm"}
  if (pm(st)){ df$shift <- "Nights"
  df$times <- "8pm-8am"}
  df$backlog <- backlog(et,db)
  df$delayed12 <- delayed(et,12,db)
  df$delayed24 <- delayed(et,24,db)
  df$TAT <- TAT(st,et,db)
  df$meanTAT <- meanTAT(st,et,db)
  df$PSP1 <- total_PSP1(st,et,db)
  df$ovens <- total_ovens(st,et,db)
  df$PSP2 <- total_PSP2(st,et,db)
  df$mod1 <- total_mod1(st,et,db)
  df$resulted <- total_resulted(st,et,db)
  df$runs <- total_amp_runs(st,et,db)
  df$positives <- total_positives(st,et,db)
  df$positivity <- round(df$positives * 100 / df$resulted,1)
  df$ovenTAT <- ovenTAT(st,et,db)
  df$mod1TAT <- mod1TAT(st,et,db)
  df$ampTAT <- ampTAT(st,et,db)
  df$PSP1TAT <- PSP1TAT(st,et,db)
  df$PSP2TAT <- PSP2TAT(st,et,db)
  df$authTAT <- authTAT(st,et,db)
  df$negatives <- total_negatives(st,et,db)
  df$negativity <- round(df$negatives * 100 / df$resulted,1)
  df$inconclusives <- total_inconclusives(st,et,db)
  df$invalids <- total_invalids(st,et,db)
  df$labVoids <- total_lab_voids(st,et,db) + df$invalids
  df$sqVoids <- total_sq_voids(st,et,db)
  df$labVoidPerc <- round(df$labVoids * 100 / df$resulted,1)
  df$sqVoidPerc <- round(df$sqVoids * 100 / df$resulted,1)
  df$totalVoids <- df$sqVoids + df$labVoids + df$inconclusives
  df$voidPerc <- round(df$totalVoids * 100 / df$resulted,1)
  df$retests <- total_retests(st,et,db)
  df$retestPerc <- round(df$retests * 100 / df$resulted,1)
  df$total10 <- total_10plus(st,et,db)
  df$total18 <- total_18plus(st,et,db)
  df$total15 <- total_15plus(st,et,db)
  df$total_0to6 <- total_in_time(st,et,0,6,db)
  df$total_6to8 <- total_in_time(st,et,6,8,db)
  df$total_8t010 <- total_in_time(st,et,8,10,db)
  df$total_10to12 <- total_in_time(st,et,10,12,db)
  df$total_12to14 <- total_in_time(st,et,12,14,db)
  df$total_14to16 <- total_in_time(st,et,14,16,db)
  df$total_16to18 <- total_in_time(st,et,16,18,db)
  df$total_18plus <- total_in_time(st,et,18,316,db)
  df$orange_0to6 <- orange_in_time(st,et,0,6,db)
  df$orange_6to8 <- orange_in_time(st,et,6,8,db)
  df$orange_8t010 <- orange_in_time(st,et,8,10,db)
  df$orange_10to12 <- orange_in_time(st,et,10,12,db)
  df$orange_12to14 <- orange_in_time(st,et,12,14,db)
  df$orange_14to16 <- orange_in_time(st,et,14,16,db)
  df$orange_16to18 <- orange_in_time(st,et,16,18,db)
  df$orange_18plus <- orange_in_time(st,et,18,316,db)
  df$blue_0to6 <- blue_in_time(st,et,0,6,db)
  df$blue_6to8 <- blue_in_time(st,et,6,8,db)
  df$blue_8t010 <- blue_in_time(st,et,8,10,db)
  df$blue_10to12 <- blue_in_time(st,et,10,12,db)
  df$blue_12to14 <- blue_in_time(st,et,12,14,db)
  df$blue_14to16 <- blue_in_time(st,et,14,16,db)
  df$blue_16to18 <- blue_in_time(st,et,16,18,db)
  df$blue_18plus <- blue_in_time(st,et,18,316,db)
  df$MS2 <- medianMS2(st,et,db)
  df$ORF <- medianORF(st,et,db)
  df$Ngene <- medianN(st,et,db)
  df$Sgene <- medianS(st,et,db)
  df$sDropout <- sGeneDropout(st,et,db)
  df$nDropout <- nGeneDropout(st,et,db)
  df$orfDropout <- orfGeneDropout(st,et,db)
  df$orangeTAT <- orangeMeanTAT(st,et,db)
  df$blueTAT <- blueMeanTAT(st,et,db)
  df$totalOrange <- totalOrange(st,et,db)
  df$totalBlue <- totalBlue(st,et,db)
  df$orangePos <- orangePositives(st,et,db)
  df$bluePos <- bluePositives(st,et,db)
  df$blue10 <- total_10blue(st,et,db)
  df$blue12 <- total_12blue(st,et,db)
  df$orange12 <- total_12orange(st,et,db)
  df$orange15 <- total_15orange(st,et,db)
  df$bay1 <- runs(st,et,db,1)
  df$bay2 <- runs(st,et,db,2)
  df$bay3 <- runs(st,et,db,3)
  df$bay4 <- runs(st,et,db,4)
  df$bay5 <- runs(st,et,db,5)
  df$bay6 <- runs(st,et,db,6)
  df$bay7 <- runs(st,et,db,7)
  df$bay8 <- runs(st,et,db,8)
  df$bay9 <- runs(st,et,db,9)
  df$bay10 <- runs(st,et,db,10)
  df$bay11 <- runs(st,et,db,11)
  df$bay12 <- runs(st,et,db,12)
  df$bay13 <- runs(st,et,db,13)
  df$psp1rate <- psp1rate(st,et, db)
  df$psp2rate <- psp2rate(st,et, db)
  df$sequenceable <- sequenceable(st,et,db)
  df$omicron <- omicron(st,et,db)
  df$Q493Rmt <- Q493R_mt(st,et,db)
  df$Q493Rwt <- Q493R_wt(st,et,db)
  df$Q493Rna <- Q493R_na(st,et,db)
  return(df)
}


pullStats12 <- function(db,st,et){
  db <- subset(db, LoginDate <= et & LoginDate >= st | NpexSentOn <= et & NpexSentOn >= st | LoginDate <= st & NpexSentOn >= et )
  # Put all stats for shift into a dataframe
  #df <- data.frame(date=as.character(date(st)))
  df <-  data.frame(date = format(date(st),format="%d/%m/%Y"))
  days <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  df$day <- days[wday(st)]
  df$team <- team(st)
  if (am(st)){ df$shift <- "Days"
  df$times <- "8am-8pm"}
  if (pm(st)){ df$shift <- "Nights"
  df$times <- "8pm-8am"}
  df$wip <- backlog(st,db)
  df$backlog <- backlog(et,db)
  df$delayed12 <- delayed(st,12,db)
  df$delayed24 <- delayed(st,24,db)
  df$TAT <- TAT(st,et,db)
  df$meanTAT <- meanTAT(st,et,db)
  df$PSP1 <- total_PSP1(st,et,db)
  df$ovens <- total_ovens(st,et,db)
  df$PSP2 <- total_PSP2(st,et,db)
  df$mod1 <- total_mod1(st,et,db)
  df$resulted <- total_resulted(st,et,db)
  df$runs <- total_amp_runs(st,et,db)
  df$positives <- total_positives(st,et,db)
  df$positivity <- round(df$positives * 100 / df$resulted,1)
  df$ovenTAT <- ovenTAT(st,et,db)
  df$mod1TAT <- mod1TAT(st,et,db)
  df$ampTAT <- ampTAT(st,et,db)
  df$PSP1TAT <- PSP1TAT(st,et,db)
  df$PSP2TAT <- PSP2TAT(st,et,db)
  df$authTAT <- authTAT(st,et,db)
  df$inconclusives <- total_inconclusives(st,et,db)
  df$invalids <- total_invalids(st,et,db)
  df$labVoids <- total_lab_voids(st,et,db) + df$invalids
  df$sqVoids <- total_sq_voids(st,et,db)
  df$labVoidPerc <- round(df$labVoids * 100 / df$resulted,1)
  df$sqVoidPerc <- round(df$sqVoids * 100 / df$resulted,1)
  df$totalVoids <- df$sqVoids + df$labVoids + df$inconclusives
  df$voidPerc <- round(df$totalVoids * 100 / df$resulted,1)
  df$retests <- total_retests(st,et,db)
  df$retestPerc <- round(df$retests * 100 / df$resulted,1)
  df$total10 <- total_10plus(st,et,db)
  df$total18 <- total_18plus(st,et,db)
  df$total15 <- total_15plus(st,et,db)
  df$delayed12end <- delayed(et,12,db)
  df$delayed24end <- delayed(et,24,db)
  df$orangeTAT <- orangeMeanTAT(st,et,db)
  df$blueTAT <- blueMeanTAT(st,et,db)
  df$totalOrange <- totalOrange(st,et,db)
  df$totalBlue <- totalBlue(st,et,db)
  df$orangePos <- orangePositives(st,et,db)
  df$bluePos <- bluePositives(st,et,db)
  df$bay1 <- runs(st,et,db,1)
  df$bay2 <- runs(st,et,db,2)
  df$bay3 <- runs(st,et,db,3)
  df$bay4 <- runs(st,et,db,4)
  df$bay5 <- runs(st,et,db,5)
  df$bay6 <- runs(st,et,db,6)
  df$bay7 <- runs(st,et,db,7)
  df$bay8 <- runs(st,et,db,8)
  df$bay9 <- runs(st,et,db,9)
  df$bay10 <- runs(st,et,db,10)
  df$bay11 <- runs(st,et,db,11)
  df$bay12 <- runs(st,et,db,12)
  df$bay13 <- runs(st,et,db,13)
  df$psp1rate <- psp1rate(st,et, db)
  df$psp2rate <- psp2rate(st,et, db)
  return(df)
}

update_24 <-function(df, sd){
  hour(sd) <- hms("0:0:0")
  ed <- sd + hms("23:59:59")
  db24 <- read.csv("Z:/Scripts/Statistics/24h.csv")
  write.csv(db24, "Z:/Scripts/Statistics/24h_backup.csv", row.names=F)
  newdb <- pullStats(df,sd,ed)
  db24 <- rbind(db24, newdb)
  db24 <- db24[!duplicated(db24),]
  write.csv(db24, "Z:/Scripts/Statistics/24h.csv", row.names=F)
}

update_12 <-function(df, sd){
  ed <- sd + hms("11:59:59")
  db12 <- read.csv("Z:/Scripts/Statistics/8to8.csv")
  write.csv(db12, "Z:/Scripts/Statistics/8to8_backup.csv", row.names=F)
  newdb <- pullStats12(df,sd,ed)
  db12 <- rbind(db12, newdb)
  db12 <- db12[!duplicated(db12),]
  write.csv(db12, "Z:/Scripts/Statistics/8to8.csv", row.names=F)
}

df <- build_database(start_date,end_date)
df <- add_voc(df, start_date,end_date)
write.csv(df, "Z:/Scripts/Statistics/todays_data.csv")
update_24(df, end_date)
ed <- end_date
hour(ed) <- 20
day(ed) <- day(ed)-1
update_12(df, ed)
df8to8 <- read.csv("Z:/Scripts/Statistics/8to8.csv")
df8to8$date <- dmy(df8to8$date)
df8to8Last <-  df8to8 %>% slice_tail(n=1)
rmarkdown::render("Z:/Scripts/rmd/8-to-8.Rmd","html_document", paste("Z:/Scripts/Reports/shift_reports/",gsub('-','',substr(df8to8Last$date,3,10)),"_Nights_Team_",df8to8Last$team,"_Shift_Report.html",sep=""))
ed <- ed+(hms("12:00:00"))
update_12(df, ed)
df8to8 <- read.csv("Z:/Scripts/Statistics/8to8.csv")
df8to8$date <- dmy(df8to8$date)
df8to8Last <-  df8to8 %>% slice_tail(n=1)
rmarkdown::render("Z:/Scripts/rmd/8-to-8.Rmd","html_document", paste("Z:/Scripts/Reports/shift_reports/",gsub('-','',substr(df8to8Last$date,3,10)),"_Days_Team_",df8to8Last$team,"_Shift_Report.html",sep=""))
rmarkdown::render("Z:/Scripts/rmd/KPI_report_Z.Rmd","html_document", paste("Z:/Scripts/Reports/kpi_reports/",t1,"_KPI_Report.html",sep=""))
rmarkdown::render("Z:/Scripts/rmd/heatmaps.Rmd","html_document", paste("Z:/Scripts/Reports/heatmaps/",t1,"_heatmaps.html",sep=""))


paste("Time taken to run script ",Sys.time()-timer_start)
paste("Please run email_kpi script")
