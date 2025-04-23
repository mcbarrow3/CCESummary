library(tidyverse)
setwd("C:/Users/jmsteve/OneDrive - New York State Office of Information Technology Services/R")

CCERaw <- read.csv("2019_NY_SAFIS_Trips.csv", header=TRUE, sep=",")
View(CCERaw)
CCEFiltered <- select(CCERaw, Trip.I., License.., First.Name, Middle.Name, Lname.Fisher, Name.Suffix, Corporate.Name, Submit.method)
View(CCEFiltered)
CCEFiltered$whole_name <- paste(CCEFiltered$First.Name, CCEFiltered$Middle.Name, CCEFiltered$Lname.Fisher, CCEFiltered$Name.Suffix, CCEFiltered$Corporate.Name, sep=" ")
CCEFiltered <- CCEFiltered %>% rename("public_id"="License..", "VTR_ID"="Trip.I.", "first_name"="First.Name", "mi"="Middle.Name", "last_name"="Lname.Fisher", "suffix"="Name.Suffix", "corporate_name"="Corporate.Name")
Length <- length(unique(CCEFiltered$VTR_ID))
TotaleTrips <- distinct(CCEFiltered)
View(TotaleTrips)
TotaleTrips <- TotaleTrips[c(1,2,3,4,5,6,7,9,8)] %>% mutate_if(is.factor, as.character) %>% mutate(VTR_ID = as.character(VTR_ID))

str(TotaleTrips)

EntryMode <- TotaleTrips %>% count(Submit.method, sort=TRUE)
EntryMode

library(RODBC)
ch <- odbcConnect("CMFS_prod")

sqlStr1 <- "SELECT DATA_TRIP.VTR_ID, DATA_TRIP.public_id, INFO_PUBLICID.first_name, INFO_PUBLICID.mi, INFO_PUBLICID.last_name, INFO_PUBLICID.suffix, INFO_PUBLICID.corporate_name, INFO_PUBLICID.whole_name"
sqlStr2 <- "FROM DATA_TRIP INNER JOIN INFO_PUBLICID ON DATA_TRIP.PUBLIC_ROW_ID = INFO_PUBLICID.PUBLIC_ROW_ID"
sqlStr3 <- "WHERE (((DATA_TRIP.VTR_TYPE)='STATE') AND ((DATA_TRIP.year)='2019'));"
sqlStr <- paste(sqlStr1, sqlStr2, sqlStr3, sep=" ")
NYFISH_Trips <- sqlQuery(ch, sqlStr) %>% mutate_if(is.factor, as.character)
odbcClose(ch)
View(NYFISH_Trips)

NYFISH_Trips$Submit.method <- "P"
str(NYFISH_Trips)

AllStateTrips <- bind_rows(NYFISH_Trips, TotaleTrips, .id = NULL)
View(AllStateTrips)

GroupedTrips <- AllStateTrips %>% 
  group_by(whole_name, public_id, Submit.method) %>%
  summarize(NumberVTRs = n()) %>%
  spread(Submit.method, NumberVTRs)
View(GroupedTrips)

write.csv(AllStateTrips, "C:/Users/jmsteve/OneDrive - New York State Office of Information Technology Services/R/AllStateTrips.csv")
write.csv(GroupedTrips, "C:/Users/jmsteve/OneDrive - New York State Office of Information Technology Services/R/GroupedTrips.csv")
