.libPaths("C:/RLibrary")
require(dplyr)
require(readr)
require(tidyverse)
require(magrittr)
require(lubridate)
require(tools)

#database start and end date (YYMMDD)

end_date = ymd(Sys.Date()-1)
start_date = ymd(Sys.Date()-7)

t1 <- gsub('-','',substr(Sys.Date()-1,3,10))

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
  nuth1_1 <- nuth1_1 %>% select(-one_of("RecommendedAction"),-one_of("ParentPlateId"),-one_of("CompressionPlateWell"),-one_of("ParentPlateWell"))
  nuth2_1 <- paste("Z:/Scripts/Data/SM_Data/",t1,"_nuth2.csv",sep="")
  nuth2_1 <- read.csv(nuth2_1)
  nuth2_1 <- nuth2_1 %>% select(-one_of("RecommendedAction"),-one_of("ParentPlateId"),-one_of("CompressionPlateWell"),-one_of("ParentPlateWell"))
  
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
    nuth1_temp <- nuth1_temp %>% select(-one_of("RecommendedAction"),-one_of("ParentPlateId"),-one_of("CompressionPlateWell"),-one_of("ParentPlateWell"))
    nuth2_temp <- paste("Z:/Scripts/Data/SM_Data/",t_temp,"_nuth2.csv",sep="")
    nuth2_temp <- read.csv(nuth2_temp)
    nuth2_temp <- nuth2_temp %>% select(-one_of("RecommendedAction"),-one_of("ParentPlateId"),-one_of("CompressionPlateWell"),-one_of("ParentPlateWell"))
    all_amp_temp <- rbind(nuth1_temp,nuth2_temp)
    all_amp <- rbind(all_amp,all_amp_temp)
    all_amp <- distinct(all_amp, CustomerReference, PreparationTime, .keep_all = T)
  }
  
  all_adb <- merge(all_adb,all_results,by.x ="CustomerId",by.y="CustomerID",  all.x = TRUE)
  
  # Create a new compression plate ID field in NPEX data
  # Take ID from results filename
  all_adb$CompressionPlateId=substr(all_adb$`AdbResultSourceFile`,19,26)
  
  # rename column for consistency between Amplitude and Databank
  all_amp <-  plyr::rename(all_amp, replace = c("DateCompleted" = "DateAmplitudeCompleted"))
  all_amp <- plyr::rename(all_amp, replace = c("CustomerReference" = "CustomerId"))
  
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
  all$AmplitudeTime <- as.double(difftime(all$DateCompleted,all$PreparationTime,units="mins")) + 55
  all$SortingWait <- as.double(difftime(all$PreparationTime,all$AdbEnteredCabinet2On,units="mins")) - 55
  all$AuthorisationTime <- as.double(difftime(all$NpexSentOn, all$DateCompleted,units="mins"))
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



old_voc <- function(db, sd, ed){
  
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
  
  voc_data <- select(voc_data, Sample, CH1.Target, CH1.Result)
  voc_data <- distinct(voc_data, Sample, CH1.Target, .keep_all = T)
  voc_data <- pivot_wider(voc_data, id_cols = Sample, names_from = CH1.Target, values_from = CH1.Result)
  voc_data <- dplyr::rename(voc_data, CustomerId = Sample)

  
  # merge results
  vocdb <- merge(db, voc_data, by = "CustomerId", all.x=T)
  vocdb$E484K <- ifelse(is.na(vocdb$NpexSentOn),NA,vocdb$E484K)
  vocdb$K417N <- ifelse(is.na(vocdb$NpexSentOn),NA,vocdb$K417N)
  vocdb$K417T <- ifelse(is.na(vocdb$NpexSentOn),NA,vocdb$K417T)
  vocdb$P681R <- ifelse(is.na(vocdb$NpexSentOn),NA,vocdb$P681R)
  
  return(vocdb)
}

catalogue <- function(){
  voc_list <- list.files(pattern = ".csv",path="Z:/Scripts/Data/innovation_lab/",full.names = TRUE)
  for(i in voc_list){
    innov <- read.csv(i)
    innov <- merge(innov, all, by = "CustomerId")
    z <- basename(file_path_sans_ext(i))
    x <- innovation(innov)
    write.csv(x, paste("Z:/Scripts/Reports/innovation_lab/",z,".csv", sep=""), row.names =  F)   
  
  }
}
sangerDB <- function(){
  sanger_list <- list.files(pattern = ".csv",path="Z:/Scripts/Reports/sanger_plate_maps/Archive/",full.names = TRUE)
  zf <- file.info(sanger_list)
  zf<-subset(zf, mtime >= start_date)
  sanger_list <- rownames(zf)
  return(sanger_list)
}

innovation <-function(innov){
  df <- select(innov, c('CustomerId','DateStarted','NpexSentOn','CompressionPlateId','Result','Ms2Ct','SgeneCt',"NgeneCt",'Orf1abCt','Position','Q493R'))
 # next line for old mutations
  # df <- select(innov, c('CustomerId','DateStarted','NpexSentOn','CompressionPlateId','Result','Ms2Ct','SgeneCt',"NgeneCt",'Orf1abCt','Position','E484K', 'P681R', 'K417N','K417T'))
  
    df['RNA_ID'] <- NA
  for (i in sanger){
    temp <- read.csv(i)
    temp <- select(temp, c('Root.Sample.ID','RNA.ID'))
    temp <- dplyr::rename(temp, CustomerId = Root.Sample.ID)
    df <- merge(temp, df, by = c("CustomerId"), all.y = T)
    df <- df %>% mutate(RNA_ID = coalesce(RNA_ID,RNA.ID)) %>% select(-RNA.ID)
    }
  df <- df %>% relocate(RNA_ID, .before = DateStarted)
  return (df)
}
  


all <- build_database(start_date,end_date)
#all <- old_voc(all, start_date,end_date)
all <- add_voc(all, start_date,end_date)
sanger <- sangerDB()
catalogue()
