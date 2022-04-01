rm(list = ls())
.libPaths("C:/RLibrary")
require(dplyr)
require(readr)
require(tidyverse)
require(magrittr)
library(dplyr)
require(lubridate)
library(markdown)
library(blastula)
library(keyring)


KPI_addresses <- c(******************)
managers <- c(*****)
#database start and end date (YYMMDD)
end_date = ymd(Sys.Date()-1)
start_date = ymd(Sys.Date()-4)
t1 <- gsub('-','',substr(end_date,3,10))
Sys.setenv("SMTP_PASSWORD"="**********") 

#create_smtp_creds_key(id="********", user = "********", provider = NULL, host = "*******", port = ***, use_ssl = T, overwrite = T)
ed <- end_date

df8to8 <- read.csv("Z:/Scripts/Statistics/8to8.csv")
df8to8$date <- dmy(df8to8$date)
df8to8Last <-  df8to8 %>% slice_tail(n=2)
dfNight <- df8to8Last %>% slice_head(n=1)
dfDay <-  df8to8 %>% slice_tail(n=1)





success <- F
shift_address <- paste("*******", dfNight$team, "********", sep="")
nightemail <- compose_email(body="This email is sent from an unattended inbox, please do not reply.  For any queries, please contact ****************")
# email night shift report
count=1
while (success==F){
  x <- try(nightemail %>% add_attachment(file = paste("Z:/Scripts/Reports/shift_reports/",gsub('-','',substr(dfNight$date,3,10)),"_Nights_Team_",dfNight$team,"_Shift_Report.html",sep="")) %>%
             smtp_send(
               #to = "********",
               to = c(managers, shift_address),
               from = "**************",
               subject = paste("Shift Report Team ",dfNight$team," Nights 20",substr(dfNight$date,3,10), sep=""),
               credentials = creds_envvar(user = "*****************", 
                                          pass_envvar = "SMTP_PASSWORD", 
                                          provider = NULL, 
                                          host = "**************",
                                          port = ***, 
                                          use_ssl = T)),silent=F)
  #Sys.sleep(1)
  if (class(x)!="try-error"){success=T}
  print(paste("This script will keep attempting to email reports until it is successful.  This may take some time - please leave it running.  Attempt",count))
  count <- count +1
}


success <- F
# email day shift report
shift_address <- paste("**********", dfDay$team, "********", sep="")
dayemail <- compose_email(body="This email is sent from an unattended inbox, please do not reply.  For any queries, please contact *********")
while (success==F){
  x <- try(dayemail %>% add_attachment(file = paste("Z:/Scripts/Reports/shift_reports/",gsub('-','',substr(dfDay$date,3,10)),"_Days_Team_",dfDay$team,"_Shift_Report.html",sep="")) %>%
             smtp_send(
               # to = "************",
               to = c(managers, shift_address),
               from = "*************",
               subject = paste("Shift Report Team ",dfDay$team," Days 20",substr(dfDay$date,3,10), sep=""),
               credentials = creds_envvar(user = "******", 
                                          pass_envvar = "SMTP_PASSWORD", 
                                          provider = NULL, 
                                          host = "******", 
                                          port = 587, 
                                          use_ssl = T)),silent=T)
  Sys.sleep(1)
  if (class(x)!="try-error"){success=T}
  print(paste("This script will keep attempting to email reports until it is successful.  This may take some time - please leave it running.  Attempt",count))
  count <- count +1
}
success <- F
# email KPI
KPIemail <- compose_email(body="This email is sent from an unattended inbox, please do not reply.  For any queries, please contact *******************")
while (success==F){
  x <- try(KPIemail %>% add_attachment(file = paste("Z:/Scripts/Reports/kpi_reports/",t1,"_KPI_Report.html",sep="")) %>%
             smtp_send(
               to = KPI_addresses,
               from = "************",
               subject = paste("KPI report 20",substr(dfDay$date,3,10), sep=""),
               credentials = creds_envvar(user = "***********", 
                                          pass_envvar = "SMTP_PASSWORD", 
                                          provider = NULL, 
                                          host = "************", 
                                          port = ***, 
                                          use_ssl = T)),silent=T)
  Sys.sleep(1)
  if (class(x)!="try-error"){success=T}
  print(paste("This script will keep attempting to email reports until it is successful.  This may take some time - please leave it running.  Attempt",count))
  count <- count +1
}


success <- F
# email heatmaps
KPIemail <- compose_email(body="")
while (success==F){
  x <- try(KPIemail %>% add_attachment(file = paste("Z:/Scripts/Reports/heatmaps/",t1,"_heatmaps.html",sep="")) %>%
             smtp_send(
               to = c("******************"),
                 from = "*******",
               subject = paste("Amplitude heatmaps 20",substr(dfDay$date,3,10), sep=""),
               credentials = creds_envvar(user = "*********", 
                                          pass_envvar = "SMTP_PASSWORD", 
                                          provider = NULL, 
                                          host = "*************", 
                                          port = ***, 
                                          use_ssl = T)),silent=T)
  Sys.sleep(1)
  if (class(x)!="try-error"){success=T}
  print(paste("This script will keep attempting to email reports until it is successful.  This may take some time - please leave it running.  Attempt",count))
  count <- count +1
}



# email VOC
success <- F
today = wday(Sys.Date(), label=FALSE, week_start=getOption("lubridate.week.start", 1))

if (today != 4){
  VOCemail <- compose_email(body="This email is sent from an unattended inbox, please do not reply.  For any queries, please contact *************")
  while (success==F){
    x <- try(VOCemail %>% add_attachment(file=paste("Z:/Scripts/Reports/ct_reports/",t1,"_DHSC_NewcastlePositiveSamples.csv", sep="")) %>% 
               add_attachment(file = paste("Z:/Scripts/Reports/voc_results/", t1, "_VOC_totals.txt", sep="")) %>%
               smtp_send(
                 #to = "***********",
                 to = c("***********"),
                 from = "*************",
                 subject = paste("Newcastle Reflex results and Ct values 20",substr(dfDay$date,3,10), sep=""),
                 credentials = creds_envvar(user = "*********", 
                                            pass_envvar = "SMTP_PASSWORD", 
                                            provider = NULL, 
                                            host = "********", 
                                            port = ***, 
                                            use_ssl = T)),silent=T)
    Sys.sleep(1)
    if (class(x)!="try-error"){success=T}
    print(paste("This script will keep attempting to email reports until it is successful.  This may take some time - please leave it running.  Attempt",count))
    count <- count +1
  }  
}

if (today==4){
  VOCemail <- compose_email(body="This email is sent from an unattended inbox, please do not reply.  For any queries, please contact ***************")
  while (success==F){
    x <- try(VOCemail %>% add_attachment(file=paste("Z:/Scripts/Reports/ct_reports/",t1,"_DHSC_NewcastlePositiveSamples.csv", sep="")) %>% 
               add_attachment(file = paste("Z:/Scripts/Reports/voc_results/", t1, "_VOC_totals.txt", sep="")) %>%
               add_attachment(file = paste("Z:/Scripts/Reports/weekly_voids/", t1, "_NC_voids.csv", sep="")) %>%
               smtp_send(
                 to = c(*************************),
                 from = "**********",
                 subject = paste("Newcastle Reflex results, voids, and Ct values 20",substr(dfDay$date,3,10), sep=""),
                 credentials = creds_envvar(user = "*******", 
                                            pass_envvar = "SMTP_PASSWORD", 
                                            provider = NULL, 
                                            host = "*********", 
                                            port = ***, 
                                            use_ssl = T)),silent=T)
    Sys.sleep(1)
    if (class(x)!="try-error"){success=T}
    print(paste("This script will keep attempting to email reports until it is successful.  This may take some time - please leave it running.  Attempt",count))
    count <- count +1
  } 
}


