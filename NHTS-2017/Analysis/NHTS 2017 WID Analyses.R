# NHTS 2017 Data Importation.R
# Create R-friendly datasets for NHTS analysis

# Import Library

suppressMessages(library(tidyverse))
library(reshape2)

# Create work directory

wd="M:/Data/Requests/Alex Ghenis (WID)/NHTS2017/"
setwd(wd)

# Input NHTS 2017 files

folder = "M:/Data/HomeInterview/NHTS 2017/files/CSV Versions/"
persondir = paste0(folder,"Person_022018.csv")
householddir = paste0(folder,"Household_022018.csv")
personwtdir = paste0(folder,"Person_weights_7day.csv")
householdwtdir = paste0(folder,"Household_weights_7day.csv")
locationdir = paste0(folder,"Location_022018.csv")

person <- read.csv(persondir, header = TRUE)
household <- read.csv(householddir, header = TRUE)
personwt <- read.csv(personwtdir,header = TRUE)
householdwt <- read.csv(householdwtdir,header = TRUE)
location <- read.csv(locationdir, header = TRUE)

# Set up short files for review and abridged weight files for joining

hhead <- head(household)
phead <- head(person)
lhead <- head(location)
hwhead <- head(householdwt)
pwhead <- head(personwt)

hweight <- householdwt %>%
  select(houseid,wthhfin)

pweight <- personwt %>%
  select(houseid,personid,wtperfin)

# Select Bay Area only households

location_home <- location %>%                            # Home locations in the Bay Area
  filter(loctype==1 & statefips==6 & (cntyfips %in% c(1,13,41,55,75,81,85,95,97))) %>%
  select(-personid)

bayhh <- left_join(location_home,household,by="houseid") %>%
  left_join(.,hweight, by="houseid")                     # Join household file to Bay Area home locations and HH weight

bayper <- left_join(location_home,person, by="houseid") %>%
  left_join(.,pweight,by=c("houseid","personid")) %>% mutate(
    Person_Type=case_when(
      medcond==1 & r_age>=65   ~ "4_Disabled and is Senior",
      medcond==1 & r_age<65    ~ "3_Disabled and is not Senior",
      medcond!=1 & r_age>=65   ~ "2_Not Disabled and is Senior",
      medcond!=1 & r_age<65    ~ "1_Not Disabled and is not Senior",
      TRUE                     ~ "Unrecoded Case"
         ))

# Variable sums that WID asked for

# First totals by person type

Region_Person <- bayper %>% 
  group_by(Person_Type) %>% 
  summarize(Total=sum(wtperfin))

#DRVR

DRVR <- bayper %>%                          
  filter(r_age>=18) %>%                      #Adults only
  group_by(Person_Type,drvr) %>%
  summarize(total=sum(wtperfin)) %>%
  mutate(drvr=if_else(drvr==1,"yes","no"))   #Assume missing responses mean that people don't drive

DRVR_output <- dcast(DRVR,Person_Type ~ drvr,sum, value.var = "total")      # Convert from list to matrix format

#PTUSED

PTUSED <- bayper %>% 
  group_by(Person_Type,ptused) %>%
  summarize(total=sum(wtperfin)) %>% mutate(
    ptused=if_else(ptused<0,0L,ptused),  #Assume missing responses equate to zero public transit trips
    ptusedcat=case_when(
      ptused==0                 ~ "1_Zero",
      ptused>=1 & ptused<=10    ~ "2_1 to 10",
      ptused>=11 & ptused<=20   ~ "3_11 to 20",
      ptused>=21                ~ "4_More than 20",
      TRUE                     ~ "Unrecoded Case"
    ))
      

PTUSED_output <- dcast(PTUSED,Person_Type ~ ptusedcat,sum, value.var = "total")

#MCUSED

MCUSED <- bayper %>% 
  group_by(Person_Type,mcused) %>%
  summarize(total=sum(wtperfin)) %>% mutate(
    mcused=if_else(mcused<0,0L,mcused),  #Assume missing responses equate to zero motorcycle trips
    mcusedcat=case_when(
      mcused==0                 ~ "1_Zero",
      mcused>=1 & mcused<=10    ~ "2_1 to 10",
      mcused>=11 & mcused<=20   ~ "3_11 to 20",
      mcused>=21                ~ "4_More than 20",
      TRUE                     ~ "Unrecoded Case"
    ))


MCUSED_output <- dcast(MCUSED,Person_Type ~ mcusedcat,sum, value.var = "total")

#CARSHARE

CARSHARE <- bayper %>% 
  group_by(Person_Type,carshare) %>%
  summarize(total=sum(wtperfin)) %>% mutate(
    carshare=if_else(carshare<0,0L,carshare),  #Assume missing responses equate to zero carshare trips
    carsharecat=case_when(
      carshare==0                 ~ "1_Zero",
      carshare>=1 & carshare<=10  ~ "2_1 to 10",
      carshare>=11                ~ "3_More than 10",
      TRUE                        ~ "Unrecoded Case"
    ))


CARSHARE_output <- dcast(CARSHARE,Person_Type ~ carsharecat,sum, value.var = "total")

#RIDESHARE

RIDESHARE <- bayper %>% 
  group_by(Person_Type,rideshare) %>%
  summarize(total=sum(wtperfin)) %>% mutate(
    rideshare=if_else(rideshare<0,0L,rideshare),  #Assume missing responses equate to zero rideshare trips
    ridesharecat=case_when(
      rideshare==0                    ~ "1_Zero",
      rideshare>=1 & rideshare<=10    ~ "2_1 to 10",
      rideshare>=11 & rideshare<=20   ~ "3_11 to 20",
      rideshare>=21                   ~ "4_More than 20",
      TRUE                            ~ "Unrecoded Case"
    ))


RIDESHARE_output <- dcast(RIDESHARE,Person_Type ~ ridesharecat,sum, value.var = "total")

#DELIVER

DELIVER <- bayper %>% 
  group_by(Person_Type,deliver) %>%
  summarize(total=sum(wtperfin)) %>% mutate(
    deliver=if_else(deliver<0,0L,deliver),  #Assume missing responses equate to zero deliveries
    delivercat=case_when(
      deliver==0                  ~ "1_Zero",
      deliver>=1 & deliver<=10    ~ "2_1 to 10",
      deliver>=11 & deliver<=20   ~ "3_11 to 20",
      deliver>=21                 ~ "4_More than 20",
      TRUE                        ~ "Unrecoded Case"
    ))


DELIVER_output <- dcast(DELIVER,Person_Type ~ delivercat,sum, value.var = "total")

#SAMEPLC

SAMEPLC <- bayper %>%
  filter(sameplc!=-1) %>%                  # Remove people who traveled in the survey
  group_by(Person_Type,sameplc) %>%
  summarize(total=sum(wtperfin)) %>% mutate(
    sameplccat=case_when(
      sameplc<0   ~"Something else",
      sameplc==1	~"Personally sick",
      sameplc==2	~"Vacation or personal day",
      sameplc==3	~"Caretaking",
      sameplc==4	~"Disabled or home-bound",  
      sameplc==5	~"Worked at home (for pay)",
      sameplc==6	~"Not scheduled to work",
      sameplc==7	~"Worked around home (not for pay)",
      sameplc==8	~"Bad weather",
      sameplc==9	~"Out of country",
      sameplc==10	~"No transportation available",
      sameplc==11	~"No longer a household resident",
      sameplc==97	~"Something else",
      TRUE        ~"Unrecoded Case"
    ))

SAMEPLC_output <- dcast(SAMEPLC,Person_Type ~ sameplccat,sum, value.var = "total")

#ALT

ALT <- bayper %>% mutate(
  Public_Transportation=  if_else(alt_1==1,wtperfin,0),
  Ride_Friend=            if_else(alt_2==2,wtperfin,0),     
  Rental_Car=             if_else(alt_3==3,wtperfin,0),
  Bike=                   if_else(alt_4==4,wtperfin,0),
  Walk=                   if_else(alt_5==5,wtperfin,0),
  Taxi_Uber=              if_else(alt_6==6,wtperfin,0),
  None=                   if_else(alt_7==7,wtperfin,0)
) %>% 
  group_by(Person_Type) %>%
  summarize(Public_Transportation=sum(Public_Transportation),Ride_Friend=sum(Ride_Friend),
            Rental_Car=sum(Rental_Car),Bike=sum(Bike),Walk=sum(Walk),Taxi_Uber=sum(Taxi_Uber),None=sum(None))

#Device

DEVICE <- bayper %>% mutate(
  Motorized_Scooter=      if_else(w_scootr==6,wtperfin,0),
  Manual_Wheelchair=      if_else(w_chair==7,wtperfin,0),     
  Motorized_Wheelchair=   if_else(w_mtrchr==8,wtperfin,0)
) %>% 
  group_by(Person_Type) %>%
  summarize(Motorized_Scooter=sum(Motorized_Scooter),Manual_Wheelchair=sum(Manual_Wheelchair),Motorized_Wheelchair=
              sum(Motorized_Wheelchair))

#ADJTRAVEL

ADJTRAVEL <- bayper %>% mutate(
  Reduced_Travel=         if_else(condtrav==1,wtperfin,0),
  Ask_For_Rides=          if_else(condride==2,wtperfin,0),     
  No_Night_Driving=       if_else(condnigh==3,wtperfin,0),
  Give_Up_Driving=        if_else(condrive==4,wtperfin,0),
  Less_Transit_Use=       if_else(condpub==5,wtperfin,0),
  Special_Transportation= if_else(condspec==6,wtperfin,0),
  Reduced_Fare_Taxi=      if_else(condtax==7,wtperfin,0)
) %>% 
  group_by(Person_Type) %>%
  summarize(Reduced_Travel=sum(Reduced_Travel), Ask_For_Rides=sum(Ask_For_Rides),No_Night_Driving=
              sum(No_Night_Driving),Give_Up_Driving=sum(Give_Up_Driving),Less_Transit_Use=sum(Less_Transit_Use),
            Special_Transportation=sum(Special_Transportation),Reduced_Fare_Taxi=sum(Reduced_Fare_Taxi))

#PTMORE

PTMORE <- bayper %>% mutate(
  Infrequent_Service=           if_else(ptmore_1==1,wtperfin,0),
  Early_Late_Enough=            if_else(ptmore_2==2,wtperfin,0),     
  Unreliable=                   if_else(ptmore_3==3,wtperfin,0),
  Expensive=                    if_else(ptmore_4==4,wtperfin,0),
  No_Stops_Near_Destination=    if_else(ptmore_5==5,wtperfin,0),
  Unsafe_Street_Crossings=      if_else(ptmore_6==6,wtperfin,0),
  Weather=                      if_else(ptmore_7==7,wtperfin,0),
  Safety_Concerns=              if_else(ptmore_8==8,wtperfin,0),
  Air_Quality=                  if_else(ptmore_9==9,wtperfin,0),
  Prefer_Driving=               if_else(ptmore_10==10,wtperfin,0)
  
) %>% 
  group_by(Person_Type) %>%
  summarize(Infrequent_Service=sum(Infrequent_Service),Early_Late_Enough=sum(Early_Late_Enough),Unreliable=
              sum(Unreliable),Expensive=sum(Expensive),No_Stops_Near_Destination=sum(No_Stops_Near_Destination),
            Unsafe_Street_Crossings=sum(Unsafe_Street_Crossings),Weather=sum(Weather),Safety_Concerns=
              sum(Safety_Concerns),Air_Quality=sum(Air_Quality),Prefer_Driving=sum(Prefer_Driving))


## Household Variables

# Merge HH file with Person 1

first <- bayper %>% 
  filter(personid==1) %>%
  select(houseid,personid,Person_Type,r_age,medcond,wtperfin) %>% 
  left_join(.,bayhh,by="houseid")

# Householder summary by type

householder <- first %>% 
group_by(Person_Type) %>% 
  summarize(Total=sum(wthhfin))

#CAR

CAR <- first %>%
  group_by(Person_Type,car) %>%
  summarize(total=sum(wthhfin)) %>% mutate(
    carcat=case_when(
      car<1        ~"5_Never",
      car==1     	 ~"1_Daily",
      car==2     	 ~"2_A few times a week",
      car==3     	 ~"3_A few times a month",
      car==4     	 ~"4_A few times a year",
      car==5     	 ~"5_Never",
      TRUE         ~"Unrecoded Case"
    ))

CAR_output <- dcast(CAR,Person_Type ~ carcat,sum, value.var = "total")

#TAXI

TAXI <- first %>%
  group_by(Person_Type,taxi) %>%
  summarize(total=sum(wthhfin)) %>% mutate(
    taxicat=case_when(
      taxi==-7       ~"5_Never",
      taxi==-9       ~"6_Missing Value",    
      taxi==1     	 ~"1_Daily",
      taxi==2     	 ~"2_A few times a week",
      taxi==3     	 ~"3_A few times a month",
      taxi==4     	 ~"4_A few times a year",
      taxi==5     	 ~"5_Never",
      TRUE         ~"Unrecoded Case"
    ))

TAXI_output <- dcast(TAXI,Person_Type ~ taxicat,sum, value.var = "total")

#BUS

BUS <- first %>%
  group_by(Person_Type,bus) %>%
  summarize(total=sum(wthhfin)) %>% mutate(
    buscat=case_when(
      bus==-9       ~"6_Missing Value",    
      bus==1     	 ~"1_Daily",
      bus==2     	 ~"2_A few times a week",
      bus==3     	 ~"3_A few times a month",
      bus==4     	 ~"4_A few times a year",
      bus==5     	 ~"5_Never",
      TRUE         ~"Unrecoded Case"
    ))

BUS_output <- dcast(BUS,Person_Type ~ buscat,sum, value.var = "total")

#TRAIN

TRAIN <- first %>%
  group_by(Person_Type,train) %>%
  summarize(total=sum(wthhfin)) %>% mutate(
    traincat=case_when(
      train==-9       ~"6_Missing Value",    
      train==1     	 ~"1_Daily",
      train==2     	 ~"2_A few times a week",
      train==3     	 ~"3_A few times a month",
      train==4     	 ~"4_A few times a year",
      train==5     	 ~"5_Never",
      TRUE         ~"Unrecoded Case"
    ))

TRAIN_output <- dcast(TRAIN,Person_Type ~ traincat,sum, value.var = "total")

#PARA

PARA <- first %>%
  group_by(Person_Type,para) %>%
  summarize(total=sum(wthhfin)) %>% mutate(
    paracat=case_when(
      para %in% c(-9,-7)   ~"6_Missing Value",    
      para==1     	       ~"1_Daily",
      para==2     	       ~"2_A few times a week",
      para==3     	       ~"3_A few times a month",
      para==4     	       ~"4_A few times a year",
      para==5     	       ~"5_Never",
      TRUE                 ~"Unrecoded Case"
    ))

PARA_output <- dcast(PARA,Person_Type ~ paracat,sum, value.var = "total")

#HOMELOC_1

HOMELOC_1 <- first %>%
  group_by(Person_Type,homeloc_1) %>%
  summarize(total=sum(wthhfin)) %>% mutate(
    homeloc_1cat=case_when(
      homeloc_1 ==-1           ~"2_Cost not a top 3 reason",    
      homeloc_1==1     	       ~"1_Cost a top 3 reason",
      TRUE                   ~"Unrecoded Case"
    ))

HOMELOC_1_output <- dcast(HOMELOC_1,Person_Type ~ homeloc_1cat,sum, value.var = "total")

#HOMELOC_10

HOMELOC_10 <- first %>%
  group_by(Person_Type,homeloc_10) %>%
  summarize(total=sum(wthhfin)) %>% mutate(
    homeloc_10cat=case_when(
      homeloc_10==-1         ~"2_Public transit not a top 3 reason",    
      homeloc_10==10     	   ~"1_Public transit a top 3 reason",
      TRUE                   ~"Unrecoded Case"
    ))

HOMELOC_10_output <- dcast(HOMELOC_10,Person_Type ~ homeloc_10cat,sum, value.var = "total")

#PRMACT

PRMACT <- bayper %>%
  filter (prmact==1) %>%                                 # Primary activity is working
  group_by(Person_Type,wrktrans,carrode) %>%
  summarize(total=sum(wtperfin))%>% mutate(
    wrktranscat=case_when(
      wrktrans==-9                                       ~"9_Missing",    
      wrktrans==1     	                                 ~"4_Walk",
      wrktrans==2     	                                 ~"5_Bike",
      (wrktrans>=3 & wrktrans<=6) & carrode<=1      	   ~"1_Drive Alone",
      (wrktrans>=3 & wrktrans<=6) & carrode>1        	   ~"2_Carpool",
      wrktrans==7                                        ~"8_Other",
      wrktrans==8                                        ~"6_Motorcycle",
      wrktrans==9                                        ~"8_Other",
      wrktrans>=10  & wrktrans<=16                       ~"3_Transit",
      wrktrans==17                                       ~"7_Taxi/Uber/Lyft",
      wrktrans==18 & carrode<=1                     	   ~"1_Drive Alone",
      wrktrans==18 & carrode>1                      	   ~"2_Carpool",
      wrktrans==19                                       ~"8_Other",
      wrktrans==20                                       ~"3_Transit",
      wrktrans==97                                       ~"8_Other",
      TRUE                                               ~"Unrecoded Case"
    ))

PRMACT_output <- dcast(PRMACT,Person_Type ~ wrktranscat,sum, value.var = "total")


# Export the summaries

# Person

write.csv(Region_Person, "NHTS 2017 Regional Seniors and PWD.csv", row.names = FALSE, quote = T)
write.csv(DRVR_output, "NHTS 2017 Driver Status.csv", row.names = FALSE, quote = T)
write.csv(PTUSED_output, "NHTS 2017 Transit Trips.csv", row.names = FALSE, quote = T)
write.csv(MCUSED_output, "NHTS 2017 Motorcycle Trips.csv", row.names = FALSE, quote = T)
write.csv(CARSHARE_output, "NHTS 2017 Carshare Trips.csv", row.names = FALSE, quote = T)
write.csv(RIDESHARE_output, "NHTS 2017 Rideshare Usage.csv", row.names = FALSE, quote = T)
write.csv(DELIVER_output, "NHTS 2017 Delivery Purchases.csv", row.names = FALSE, quote = T)
write.csv(SAMEPLC_output, "NHTS 2017 Reason for No Travel.csv", row.names = FALSE, quote = T)
write.csv(ALT, "NHTS 2017 Alternative Transportation No Car Available.csv", row.names = FALSE, quote = T)
write.csv(DEVICE, "NHTS 2017 Device Usage.csv", row.names = FALSE, quote = T)
write.csv(ADJTRAVEL, "NHTS 2017 Med Condition Adjusted Travel.csv", row.names = FALSE, quote = T)
write.csv(PTMORE, "NHTS 2017 Using Public Transit More.csv", row.names = FALSE, quote = T)

# Household

write.csv(householder, "NHTS 2017 Householder.csv", row.names = FALSE, quote = T)
write.csv(CAR_output, "NHTS 2017 Car Use.csv", row.names = FALSE, quote = T)
write.csv(TAXI_output, "NHTS 2017 Taxi Use.csv", row.names = FALSE, quote = T)
write.csv(BUS_output, "NHTS 2017 Bus Use.csv", row.names = FALSE, quote = T)
write.csv(TRAIN_output, "NHTS 2017 Train Use.csv", row.names = FALSE, quote = T)
write.csv(PARA_output, "NHTS 2017 Paratransit Use.csv", row.names = FALSE, quote = T)
write.csv(HOMELOC_1_output, "NHTS 2017 Cost of Home a Factor.csv", row.names = FALSE, quote = T)
write.csv(HOMELOC_10_output, "NHTS 2017 Proximity of Home to Transit a Factor.csv", row.names = FALSE, quote = T)
write.csv(PRMACT_output, "NHTS 2017 Primary Mode to Work.csv", row.names = FALSE, quote = T)
