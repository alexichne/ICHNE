.libPaths("C:/RLibrary") 

require(dplyr)
require(readr)
require(tidyverse)
require(magrittr)
require(lubridate)

#database start and end date (YYMMDD)

end_date = ymd(Sys.Date()-1)
start_date = ymd(Sys.Date()-4)
today = wday(Sys.Date(), label=FALSE, week_start=getOption("lubridate.week.start", 1))
#today=4
if (today == 4){
  start_date = ymd(Sys.Date()-8)
}


t1 <- gsub('-','',substr(Sys.Date()-1,3,10))
x <- character(0)
output_DF <- data.frame("Sample"=x, "Q493R"=x)


hamiltonValidate <- function(){
  voc_list <- list.files(pattern = "ICHNE",path="Z:/Scripts/Data/Hamilton_output_files/",full.names = TRUE)
  for(i in voc_list){
    x <- read.csv(i)
    if (nrow(x) < 96
    ){
      stop('File ',i,' does not contain enough rows')
      return(T)
    }
    if (ncol(x) != 2){
      stop('File ',i,' does not contain the correct number of columns.')
      return(T)
    }
  }
  
  return(F)
}

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
  
  all_adb <- merge(all_adb,all_results,by.x ="CustomerId",by.y="CustomerID",all.x = TRUE)
  
  # Create a new compression plate ID field in NPEX data
  # Take ID from results filename
  all_adb$CompressionPlateId=substr(all_adb$`AdbResultSourceFile`,19,26)
  
  # rename column for consistency between Amplitude and Databank
  all_amp <-  plyr::rename(all_amp, replace = c("DateCompleted" = "DateAmplitudeCompleted"))
  all_amp <- plyr::rename(all_amp, replace = c("CustomerReference" = "CustomerId"))
  
  #all_amp <- all_amp[(all_amp$CustomerId %in% all_adb$CustomerId)|all_amp$CustomerId=="NC1",]
  #all_amp <- subset(all_amp, DateAmplitudeCompleted !="")
  
  # Create new dataset, by merging Amplitude and Databank samples that 
  # share a barcode and compression plate
  all <- merge(all_amp,all_adb,by=c("CustomerId","CompressionPlateId"),all.y=TRUE)
  
  # remove useless columns
  all <- select(all,-X.x,-X.y,-X)
  
  # format results
  all$Result[is.na(all$Result) & !is.na(all$NpexSentOn) & all$Status != "Cancelled"]<- "Invalid"
  all$Result[all$Result == "Positive SARS-CoV-2"] <- "Positive"
  all$Result[all$Result == "SARS-CoV-2 Inconclusive"] <- "Inconclusive"
  all$Result[all$Result == "SARS-CoV-2 Not Detected"] <- "Negative"
  
  
  # format dates
  all$LoginDate <- format(dmy_hm(all$LoginDate))
  #all$DateAuthorised <- format(dmy_hm(all$DateAuthorised))
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
  all$AmplitudeTime <- as.double(difftime(all$DateCompleted,all$PreparationTime,units="mins")) + 50
  all$SortingWait <- as.double(difftime(all$PreparationTime,all$AdbEnteredCabinet2On,units="mins")) - 50
  all$AuthorisationTime <- as.double(difftime(all$NpexSentOn, all$DateCompleted,units="mins"))
  all$OvenTime <- as.double(difftime(all$AdbExitedOvenOn,all$AdbEnteredOvenOn, units="mins"))
  all$PSP2Wait <- as.double(difftime(all$AdbEnteredCabinet2On, all$AdbExitedOvenOn, units="mins"))
  all$OvenWait <- as.double(difftime(all$AdbEnteredOvenOn,all$LoginDate, units="mins"))
  
  all <- distinct(all)
  return(all)
}
save_database <- function(date, db){
  t1 <- gsub('-','',substr(date,3,10))
  newDb <- subset(db, date(LoginDate) == date(date) | date(NpexSentOn) == date(date))
  filename <- paste("Z:/Scripts/Data/SM_Data/",t1,"_database.csv",sep = "")
  write.csv(newDb,filename)                
}

catalogue <- function(){
  voc_list <- list.files(pattern = "ICHNE",path="Z:/Scripts/Data/Hamilton_output_files/",full.names = TRUE)
  for(i in voc_list){
    Plate_ID <- gsub('-','',substr(i,39,50))
    sanger(i, Plate_ID)
    
  }}
sanger <- function(hamiltonfile,Plate_ID){
  print(Plate_ID)
  map <- read_csv(hamiltonfile, show_col_types = FALSE)%>%head(92)
  map <- dplyr::rename(map, CustomerId = SampleBarcode)
  map2 <- merge(all,map,by=c("CustomerId"),all.y=TRUE)
  df <- map2[order(substring(map2$Position,1,1),as.numeric(substring(map2$Position,2,))),]
  df <- df[,c(1,2,9,10,5,3,6,4,25)]
  x=c("Sample ID","PCR Plate Barcode","Date Started","Date Tested","CH1-Cq","CH2-Cq","CH3-Cq","CH4-Cq","VOC Well")
  colnames(df) <- x
  df$`Date Tested` <- substr(as.character(df$`Date Tested`),1,19)
  df$`Date Tested` <- paste(df$`Date Tested`,"UTC",sep=" ")
  
  # Add empty rows to bottom of dataframe to be 96 rows total
  empty <- as.data.frame(matrix(nrow = 4, ncol = 14))
  df <- bind_rows(df, empty)%>%
    select(., 1:14)
  
  # Add well numbers in correct format for sanger
  Well_numbers <- c('A01', 'A02', 'A03', 'A04', 'A05', 'A06', 'A07', 'A08', 'A09', 'A10', 'A11', 'A12', 
                    'B01', 'B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B09', 'B10', 'B11', 'B12', 
                    'C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 'C10', 'C11', 'C12', 
                    'D01', 'D02', 'D03', 'D04', 'D05', 'D06', 'D07', 'D08', 'D09', 'D10', 'D11', 'D12', 
                    'E01', 'E02', 'E03', 'E04', 'E05', 'E06', 'E07', 'E08', 'E09', 'E10', 'E11', 'E12', 
                    'F01', 'F02', 'F03', 'F04', 'F05', 'F06', 'F07', 'F08', 'F09', 'F10', 'F11', 'F12', 
                    'G01', 'G02', 'G03', 'G04', 'G05', 'G06', 'G07', 'G08', 'G09', 'G10', 'G11', 'G12', 
                    'H01', 'H02', 'H03', 'H04', 'H05', 'H06', 'H07', 'H08', 'H09', 'H10', 'H11', 'H12')
  df <- bind_cols(Well_numbers, df)%>%
    select(!('VOC Well'))
  
  # Create extra columns
  df$'Root Sample ID' <- df$`Sample ID`
  df$'RNAID' <- paste(Plate_ID, df$...1, sep = "_")
  df$Result <- "Positive"
  df$`Lab ID` <- "NCL"
  df$`CH1-Target` <- "MS2"
  df$`CH2-Target` <- "N gene"
  df$`CH3-Target` <- "S gene"
  df$`CH4-Target` <- "ORF1ab"
  
  df$`CH1-Cq` <- replace(df$`CH1-Cq`, df$`CH1-Cq`==-1, 69)
  df$`CH2-Cq` <- replace(df$`CH2-Cq`, df$`CH2-Cq`==-1, 69)
  df$`CH3-Cq` <- replace(df$`CH3-Cq`, df$`CH3-Cq`==-1, 69)
  df$`CH4-Cq` <- replace(df$`CH4-Cq`, df$`CH4-Cq`==-1, 69)
  
  # Call samples as positive/negative depending on CT value
  df$`CH1-Result` <- as.character(df$`CH1-Cq` <=37)
  df$`CH2-Result` <- as.character(df$`CH2-Cq` <=37)
  df$`CH3-Result` <- as.character(df$`CH3-Cq` <=37)
  df$`CH4-Result` <- as.character(df$`CH4-Cq` <=37)
  df$`CH1-Result`[df$`CH1-Result` == "TRUE"] <- "Positive"
  df$`CH2-Result`[df$`CH2-Result` == "TRUE"] <- "Positive"
  df$`CH3-Result`[df$`CH3-Result` == "TRUE"] <- "Positive"
  df$`CH4-Result`[df$`CH4-Result` == "TRUE"] <- "Positive"
  df$`CH1-Result`[df$`CH1-Result` == "FALSE"] <- "Negative"
  df$`CH2-Result`[df$`CH2-Result` == "FALSE"] <- "Negative"
  df$`CH3-Result`[df$`CH3-Result` == "FALSE"] <- "Negative"
  df$`CH4-Result`[df$`CH4-Result` == "FALSE"] <- "Negative"
  
  df$`CH1-Cq` <- replace(df$`CH1-Cq`, df$`CH1-Cq`==69, "")
  df$`CH2-Cq` <- replace(df$`CH2-Cq`, df$`CH2-Cq`==69, "")
  df$`CH3-Cq` <- replace(df$`CH3-Cq`, df$`CH3-Cq`==69, "")
  df$`CH4-Cq` <- replace(df$`CH4-Cq`, df$`CH4-Cq`==69, "")
  
  
  #Construct new dataframe from existing columns in df
  output_df <- df %>% dplyr::select(15,16,17,18,5,19,23,6,20,24,7,21,25,8,22,26,9)
  
  # Fill empty rows
  output_df$`Root Sample ID`[is.na(output_df$`CH1-Cq`)] <- NA  
  output_df$`Root Sample ID`[is.na(output_df$`Root Sample ID`)] <- "Empty"
  output_df$`Result`[output_df$`Root Sample ID` == "Empty"] <- NA
  output_df$`Lab ID`[output_df$`Root Sample ID` == "Empty"] <- NA
  output_df$`CH1-Target`[output_df$`Root Sample ID` == "Empty"] <- NA
  output_df$`CH2-Target`[output_df$`Root Sample ID` == "Empty"] <- NA
  output_df$`CH3-Target`[output_df$`Root Sample ID` == "Empty"] <- NA
  output_df$`CH4-Target`[output_df$`Root Sample ID` == "Empty"] <- NA
  output_df$`Date Tested`[output_df$`Root Sample ID` == "Empty"] <- NA
  output_df <- output_df[order(str_sub(output_df$RNAID, start=-2)),]
  output_df <- dplyr::rename(output_df, "RNA ID" = RNAID)
  
  # Write output as plate_ID.csv 
  write.csv(output_df, file = paste("Z:/Scripts/Reports/sanger_plate_maps/",Plate_ID, ".csv", sep = ""), row.names = F)
  
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
output_Numbers <- function(){
  
  # formatted for ease of checking
  NY <- nrow(subset(output_DF,          N501Y=="mt"                               & E484K!="mt" & K417N!="mt" & K417T!="mt" & P681R!="mt"))
  EK <- nrow(subset(output_DF,          E484K=="mt"                               & N501Y!="mt" & K417N!="mt" & K417T!="mt" & P681R!="mt"))
  NY_EK <- nrow(subset(output_DF,       N501Y=="mt" & E484K=="mt"                 & K417N!="mt" & K417T!="mt" & P681R!="mt"))
  KN <- nrow(subset(output_DF,          K417N=="mt"                               & N501Y!="mt" & E484K!="mt" & K417T!="mt" & P681R!="mt"))
  KN_EK <- nrow(subset(output_DF,       K417N=="mt" & E484K=="mt"                 & N501Y!="mt" & K417T!="mt" & P681R!="mt"))
  KN_NY <- nrow(subset(output_DF,       K417N=="mt" & N501Y=="mt"                 & E484K!="mt" & K417T!="mt" & P681R!="mt"))
  KN_EK_NY <- nrow(subset(output_DF,    K417N=="mt" & E484K=="mt" & N501Y=="mt"   & K417T!="mt" & P681R!="mt"))
  KT <- nrow(subset(output_DF,          K417T=="mt"                               & N501Y!="mt" & E484K!="mt" & K417N!="mt" & P681R!="mt"))
  KT_EK <- nrow(subset(output_DF,       K417T=="mt" & E484K=="mt"                 & N501Y!="mt" & K417N!="mt" & P681R!="mt"))
  KT_EK_NY <- nrow(subset(output_DF,    K417T=="mt" & E484K=="mt" & N501Y!="mt"   & K417N!="mt" & P681R!="mt"))
  PR <- nrow(subset(output_DF,          P681R=="mt"                               & N501Y!="mt" & E484K!="mt" & K417N!="mt" & K417T!="mt"))
  PR_EK <- nrow(subset(output_DF,       P681R=="mt" & E484K=="mt"                 & N501Y!="mt" & K417N!="mt" & K417T!="mt"))
  PR_KN <- nrow(subset(output_DF,       P681R=="mt" & K417N=="mt"                 & N501Y!="mt" & E484K!="mt" & K417T!="mt"))
  PR_KT <- nrow(subset(output_DF,       P681R=="mt" & K417T=="mt"                 & N501Y!="mt" & E484K!="mt" & K417N!="mt"))
  
  OM <- nrow(subset(output_DF,          K417N=="mt" & P681R!="No Amplification"   & N501Y!="mt" & E484K!="mt" & K417T!="mt"))
  
  total <- nrow(output_DF)
  
  DHSCNumbers_DF <<- data.frame("Overall"=total,
                                "Omicron"=OM, 
                                "N501Y"=NY, 
                                "E484K"=EK,
                                "N501Y.E484K"=NY_EK, 
                                "K417N"=KN,
                                "K417N.E484K"=KN_EK,
                                "K417N.N501Y"=KN_NY,
                                "K417N.E484K.N501Y"=KN_EK_NY,
                                "K417T"=KT, 
                                "K417T.E484K"=KT_EK,
                                "K417T.E484K.N501Y"=KT_EK_NY,
                                "P681R"=PR,
                                "P681R.E484K"=PR_EK,
                                "P681R.K417N"=PR_KN,
                                "P681R.K417T"=PR_KT)
  
  DHSCNumbers_DF <<- as.data.frame(t(DHSCNumbers_DF))
  colnames(DHSCNumbers_DF) <<- c("Totals")
  
  
  total_list <<- c(total, NY, EK, NY_EK, KN, KN_EK, KN_NY, KN_EK_NY, KT, KT_EK, KT_EK_NY, PR, PR_EK, PR_KN, PR_KT, OM)
}
vocResults <- function(){
  x <- character(0)
  output_DF <- data.frame("Sample"=x, "E484K"=x, "K417N"=x, "K417T"=x, "P681R"=x, "N501Y"=x)
  getInputFiles()
  output_Numbers()
  saveData <- DHSCNumbers_DF
  write.csv(saveData, paste("Z:/Scripts/Reports/voc_results/",t1,"_VOC_totals.csv", sep=""))
}

voidReport <-function(){
  # get voids
  voids_DF <- subset(all, !is.na(AdbVoidConfirmedOn) & !is.na(NpexSentOn)& NpexSentOn<= end_date & NpexSentOn >= start_date)
  
  # get inconclusive
  inconclusive_DF <- subset(all, Result == "Inconclusive" & !is.na(NpexSentOn))
  # for con_fail change adbC19 void reason to 10
  inconclusive_DF$AdbC19VoidReason[inconclusive_DF$AdbC19VoidReason == ""] <- "10 - Other"
  inconclusive_DF$AdbVoidComments[inconclusive_DF$AdbVoidComments == ""] <- "Inconclusive"
  
  # get control failures
  confail_DF <- subset(all, Result=="Invalid" & !is.na(NpexSentOn))
  # for con_fail change adbC19 void reason to 10
  confail_DF$AdbC19VoidReason[confail_DF$AdbC19VoidReason == ""] <- "10 - Other"
  confail_DF$AdbVoidComments[confail_DF$AdbVoidComments == ""] <- "Control failure"
  
  # join everything and take out what you need
  allVoids <- rbind(voids_DF, inconclusive_DF, confail_DF)
  allVoids <- select(allVoids, c('CustomerId','AdbC19VoidReason', 'NpexSentOn','AdbVoidComments'))
  
  filepath <- paste("Z:/Scripts/Reports/weekly_voids/",t1,"_NC_voids",".csv",sep="")
  write.csv(allVoids, filepath, row.names = F)
}

q493r <- function(){
  #x <- character(0)
  #output_DF <- data.frame("Sample"=x, "Q493R"=x)
  getInputFiles()
  tot <- paste("Total reflex tests: ", nrow(output_DF))
  mut <- paste("Q493 mutant: ", nrow(subset(output_DF, Q493R == "mt")))
  wt <- paste("Q493 wildtype: ", nrow(subset(output_DF, Q493R == "wt")))
  nd <- paste("No amplification: ", nrow(subset(output_DF, Q493R == "No Amplification")))
  fileConn <- file(paste("Z:/Scripts/Reports/voc_results/",t1,"_VOC_totals.txt", sep=""))
  writeLines(c(tot,mut,wt,nd),fileConn)
  close(fileConn)
}


dhsc <-function(db, date){
  # Creates a list of positive samples and Cts for DHSC
  db$NpexSentOn <- as.Date(db$NpexSentOn)
  db <- subset(db, NpexSentOn == date)
  db <- subset(db, Result == "Positive")
  df <- select(db, c('CustomerId','DateStarted','NpexSentOn','CompressionPlateId','Ms2Ct','SgeneCt',"NgeneCt",'Orf1abCt'))
  filepath <- paste("Z:/Scripts/Reports/ct_reports/",t1,"_DHSC_NewcastlePositiveSamples",".csv",sep="")
  write.csv(df,filepath, row.names = F)
}

if (!hamiltonValidate()){
  all <- build_database(start_date,end_date)
  catalogue()
  dhsc(all,end_date)
  if (today==4){voidReport()}
  q493r()
}
