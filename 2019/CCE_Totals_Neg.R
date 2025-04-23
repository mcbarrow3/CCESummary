library(tidyverse)
setwd("C:/Users/jmsteve/OneDrive - New York State Office of Information Technology Services/R/CCESummary")

CCERawNeg <- read.csv("SAFIS_Neg_Trips.csv", header=TRUE, sep=",")
View(CCERawNeg)
CCEFilteredNeg <- select(CCERawNeg, Trip.I., License.., First.Name, Middle.Name, Lname.Fisher, Name.Suffix, Corporate.Name, Submit.method)
View(CCEFilteredNeg)
CCEFilteredNeg$whole_name <- paste(CCEFilteredNeg$First.Name, CCEFilteredNeg$Middle.Name, CCEFilteredNeg$Lname.Fisher, CCEFilteredNeg$Name.Suffix, CCEFilteredNeg$Corporate.Name, sep=" ")
CCEFilteredNeg <- CCEFilteredNeg %>% rename("public_id"="License..", "VTR_ID"="Trip.I.", "first_name"="First.Name", "mi"="Middle.Name", "last_name"="Lname.Fisher", "suffix"="Name.Suffix", "corporate_name"="Corporate.Name")
Length <- length(unique(CCEFilteredNeg$VTR_ID))
TotalNegTrips <- distinct(CCEFilteredNeg)
View(TotalNegTrips)
TotalNegTrips <- TotalNegTrips[c(1,2,3,4,5,6,7,9,8)] %>% mutate_if(is.factor, as.character) %>% mutate(VTR_ID = as.character(VTR_ID))

str(TotalNegTrips)

EntryMode <- TotalNegTrips %>% count(Submit.method, sort=TRUE)
EntryMode

library(RODBC)
ch <- odbcConnect("CMFS_prod")

sqlStr1 <- "SELECT DATA_TRIP.VTR_ID, DATA_TRIP.public_id, INFO_PUBLICID.first_name, INFO_PUBLICID.mi, INFO_PUBLICID.last_name, INFO_PUBLICID.suffix, INFO_PUBLICID.corporate_name, INFO_PUBLICID.whole_name"
sqlStr2 <- "FROM DATA_TRIP INNER JOIN INFO_PUBLICID ON DATA_TRIP.PUBLIC_ROW_ID = INFO_PUBLICID.PUBLIC_ROW_ID"
sqlStr3 <- "WHERE (((DATA_TRIP.DNF)=1) AND ((DATA_TRIP.year)='2019'));"
sqlStr <- paste(sqlStr1, sqlStr2, sqlStr3, sep=" ")
NYFISH_Neg_Trips <- sqlQuery(ch, sqlStr) %>% mutate_if(is.factor, as.character)

View(NYFISH_Neg_Trips)

NYFISH_Neg_Trips$Submit.method <- "P"
str(NYFISH_Neg_Trips)

sqlStr4 <- "SELECT DATA_DNF.DNF_ID, DATA_DNF.public_id, INFO_PUBLICID.first_name, INFO_PUBLICID.mi, INFO_PUBLICID.last_name, INFO_PUBLICID.suffix, INFO_PUBLICID.corporate_name, INFO_PUBLICID.whole_name"
sqlStr5 <- "FROM DATA_DNF INNER JOIN INFO_PUBLICID ON DATA_DNF.PUBLIC_ROW_ID = INFO_PUBLICID.PUBLIC_ROW_ID"
sqlStr6 <- "WHERE (((DATA_DNF.year)='2019') AND ((DATA_DNF.VTR_TYPE)='DNF'));"
sqlStrQry2 <- paste(sqlStr4, sqlStr5, sqlStr6, sep=" ")
NYFISH_DNF_Trips <- sqlQuery(ch, sqlStrQry2) %>% mutate_if(is.factor, as.character)  %>% rename("VTR_ID"="DNF_ID") %>% mutate(VTR_ID = as.character(VTR_ID))

View(NYFISH_DNF_Trips)

NYFISH_DNF_Trips$Submit.method <- "P"
str(NYFISH_DNF_Trips)

AllNegTrips <- bind_rows(NYFISH_Neg_Trips, NYFISH_DNF_Trips, TotalNegTrips, .id = NULL)
View(AllNegTrips)

SubmitMode <- AllNegTrips %>% count(Submit.method, sort=TRUE)
SubmitMode


GroupedNegTrips <- AllNegTrips %>% 
  group_by(whole_name, public_id, Submit.method) %>%
  summarize(NumberVTRs = n()) %>%
  spread(Submit.method, NumberVTRs)
View(GroupedNegTrips)

write.csv(AllNegTrips, "C:/Users/jmsteve/OneDrive - New York State Office of Information Technology Services/R/CCESummary/AllNegTrips.csv")
write.csv(GroupedNegTrips, "C:/Users/jmsteve/OneDrive - New York State Office of Information Technology Services/R/CCESummary/GroupedNegTrips.csv")
