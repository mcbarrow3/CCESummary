library(tidyverse)
library(RODBC)
library(janitor)
library(lubridate)
library(glue)

##Interested in getting list of active NY permit holders (fisher only) and submission method for reporting trips (paper/mobile/keyed using SAFIS Online), along
##with number of reports that are paper/electronic per fisher.  If entirely paper reporting, number of trips not needed.


##Read in list of active NY permits listed on SAFIS, downloaded 3/15/21.
safispermits <- read_csv("data/active_safis_ny_permits.csv", col_types = cols(.default = "c")) %>% 
  clean_names() %>% 
  select(participant_id, license_number, license_type, dealer_fisher_admin, last_name, first_name, corporate_name) %>% 
  filter(dealer_fisher_admin == 'CF' | dealer_fisher_admin == 'RPC') %>% 
  filter(license_type %in% c("PUBLIC ID", "PARTY/CHARTER PUBLIC ID", "PARTY/CHARTER  PUBLIC ID", "PARTY/CHARTER"))

#str(safispermits)
#View(safispermits)
#length(unique(safispermits$license_number))
#unique(safispermits$license_type)

safisdistinct <- safispermits %>% 
  select(public_id = "license_number", participant_id, last_name, first_name, corporate_name) %>% 
  distinct()


##Pull list of active 2021 permit holders from NYFISH and join to list of SAFIS active permits. This connects the SAFIS participant_id to public_id, so we can match
##public_id to active SAFIS accounts.
ch <- odbcConnect("CMFS_prod")
qry <- glue("SELECT INFO_PERMIT.public_id, INFO_PERMIT.year, INFO_PERMIT.last_name, INFO_PERMIT.first_name, INFO_PERMIT.corporate_name, INFO_PERMIT.permit_type, INFO_PERMIT.appl_expire_date, INFO_PERMIT.status, INFO_PERMIT.federal FROM INFO_PERMIT WHERE (((INFO_PERMIT.year)='2021'))")
NYFISHraw <- sqlQuery(ch, qry)

x <- "'2019','2020'"

fedqry <- glue("SELECT INFO_PUBLICID.public_id, INFO_PUBLICID.year, INFO_PUBLICID.fed_status FROM INFO_PUBLICID WHERE INFO_PUBLICID.year IN ({x})")
nyfish_fed <- sqlQuery(ch, fedqry)

contactqry <- glue("SELECT INFO_PUBLICID.public_id, INFO_PUBLICID.year, INFO_PUBLICID.street1, INFO_PUBLICID.street2, INFO_PUBLICID.city, INFO_PUBLICID.state, INFO_PUBLICID.zip, INFO_PUBLICID.zip_ext, INFO_PUBLICID.address_type, INFO_PUBLICID.home_phone, INFO_PUBLICID.business_phone, INFO_PUBLICID.email FROM INFO_PUBLICID WHERE (INFO_PUBLICID.year='2021')")
contact_info <- sqlQuery(ch, contactqry) %>% 
  mutate(public_id = as.character(public_id)) %>% 
  select(!year)

odbcClose(ch)

NYFISH <- NYFISHraw %>% 
  filter(permit_type != "Food Fish/Crustacea Dealer/Shipper",
         status == "Issued") %>% 
  select(public_id, last_name, first_name, corporate_name, permit_type) %>% 
  distinct() %>% 
  mutate(public_id = as.character(public_id))
  
activepermits <- left_join(NYFISH, safisdistinct, by = "public_id") %>% 
  select(participant_id, public_id, last_name = last_name.x, first_name = first_name.x, corporate_name = corporate_name.x, permit_type)

fed <- nyfish_fed %>% 
  mutate(public_id = as.character(public_id),
         year = as.character(year)) %>% 
  filter(fed_status=="1")

####Code using participant list w/ concat. account field####
##Read in list of participants with SAFIS accounts, downloaded 3/15/21.##
#participants <- read_csv("data/all_ny_safis_participants.csv", col_types = cols(.default = "c")) %>% 
#  clean_names() %>% 
#  select(participant_id, safis_username_status)

#comb <- left_join(activepermits, participants, by='participant_id') %>% 
#  mutate(newcol = str_replace_all(safis_username_status, "\\(", "."))

#colsplit <- separate(comb, newcol, c("username_participant", "status_participant"), remove = FALSE) %>% 
#  select(participant_id, username_participant, status_participant) %>% 
#  filter(username_participant != "")

#View(comb)
#str(comb)
#length(unique(comb$public_id))
#comb %>% get_dupes(public_id)


####Code using SAFIS accounts list with sep fields####

##Read in list of active SAFIS NY eTrips accounts (pre-filtered for ETRIPS and ACTIVE and downloaded 4/9/21), and match with NYFISH 2021 permit holders and participant list.
accounts <- read_csv("data/all_ny_permits_with_safis_accounts.csv", col_types = cols(.default = "c")) %>% 
  clean_names() %>% 
  select(participant_id, username, last_login)

combined <- left_join(activepermits, accounts, by = "participant_id") %>% 
  mutate(submit_method = case_when(
    is.na(username) ~ 'Paper',
    username != "" ~ 'Electronic'))

#combined %>% get_dupes(public_id)
#length(unique(combined$public_id))
#length(unique(combined$participant_id))
#combined %>% get_dupes(participant_id)


##Read in all 2019 and 2020 trips that have been entered/uploaded to SAFIS, then group by year, license #, and submit_method, summarize, and use pivot_wider to make 3 new
##columns, one for each submit_method.

alltrips <- read_csv("data/19-20_all_trips_on_safis.csv", col_types = cols(.default = "c")) %>% 
  clean_names()

alltrips$trip_start_date <- mdy(alltrips$trip_start_date) 
  
alltrips <- alltrips %>% 
  mutate(trip_year = as.character(year(trip_start_date))) %>% 
  mutate(vtr_type = case_when(
           submit_method == "U" ~ "Paper",
           submit_method == "K" ~ "eTrips_Online",
           submit_method == "M" ~ "eTrips_Mobile"
         )) %>% 
  select(trip_i_number, trip_type, public_id = license_number, last_name = lname_fisher, first_name, corporate_name, trip_start_date, trip_year, submit_method, vtr_type) %>%
  distinct() %>% 
  filter(trip_year != 2021)

trip_summary <- alltrips %>% 
  group_by(public_id, trip_year, vtr_type) %>% 
  summarize(reports = n()) %>% 
  pivot_wider(names_from = vtr_type, values_from = reports)



together <- left_join(combined, trip_summary, by = "public_id")

together_again <- left_join(together, fed, by = c("public_id"="public_id", "trip_year"="year"))

all_fields <- left_join(together_again, contact_info, by = "public_id")

writexl::write_xlsx(all_fields, "output/permit_holder_reporting_summary.xlsx")
