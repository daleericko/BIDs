### Erick Cohen
### 13 March 2019 
### Business Improvement Districts and property values Economics Thesis

#installing packages
library("tidyverse") 
library("lubridate") # for sale date calculations
library("ggthemes") # for additional ggplot themes 
library("dummies") # For creating dummy variables
library("stargazer") # For making model tables

#Loading in the dataset 
library(readr)
BID_locations <- read_csv("dc-residential-properties/Business_Improvement_Districts.csv")
View(BID_locations)

colnames(BID_locations)[colnames(BID_locations)=="OBJECTID"] <- "join_OBJECTID"

library(readr)
master_BID_distance <- read_csv("master_BID_distance.csv")
View(master_BID_distance)

#filtering out outlier prices and some cleaning
master_BID_xsv <- master_BID_distance%>% #already filtered for price in mbdistance
  filter(PRICE<1200000)%>%
  filter(PRICE>1)%>%
  filter(!is.na(PRICE))%>%
  filter(GBA>1)%>%
  filter(!is.na(GBA))


# lubridate saledate 
sale_date_l <- mdy_hms(master_BID_xsv$SALEDATE) # add this to master_xsv_nona 

# final data set with everything 
data_final <- cbind(master_BID_xsv,sale_date_l) # double check to make sure there are no NAs ; try to change inBID boolean from false to true

#Adding column to BID locations to get dates the BIDS were formed 
BID_locations$date_formed <-  c(2007,2004,1997,1997,1999,2005,2018,2012,2007,2015,2002) #Add month! 

# In Bid TRUE or FALSE
data_final$inBID <- ifelse(data_final$distance == 0,1,0)
View(data_final)
table(data_final$inBID) # BIG Problem. Not that many inside the dataset. But we can still test for spillovers. 

#data_final <-  merge(data_final, BID_locations[, c("GIS_ID", "date_formed")], by="GIS_ID",all = TRUE)
data_final <-  merge(data_final, BID_locations[, c("join_OBJECTID", "date_formed")], by="join_OBJECTID",all = TRUE)

#Creating dummy variable for property price after BID designated
test_3 <- year(data_final$sale_date_l)
test_4 <- data_final$date_formed

data_final$after_des <- ifelse((test_3 -test_4) > 0,1,0)

table(data_final$after_des)


#####
# The Cleaning Data set thing 
# This is to select only the variables I need for the model
data_final_cleaning <- data_final%>%
  select(GIS_ID,#Dum
         join_GIS_ID,
         FID,
         BATHRM,
         HF_BATHRM, #Dum
         HEAT,      #Dum 
         AC,        #Dum
         NUM_UNITS,
         ROOMS,
         BEDRM,
         AYB,     #Dum
         #YR_RMDL,
         #EYB,
         STORIES, 
         PRICE,# DEPENDANT VARIABLE LOG()
         QUALIFIED, #Dum
         #STYLE # see what this one is (some are vacant and number of stories)
         STRUCT,   #Dum
         GRADE,    #Dum
         CNDTN,    #Dum
         EXTWALL,  #Dum
         ROOF,     #Dum
         INTWALL,  #Dum
         KITCHENS,
         FIREPLACES,
         GBA,
         LANDAREA,
         SOURCE,   #Dum
         ZIPCODE,  #Dum  
         #LONGITUDE,
         #LATITUDE,
         CENSUS_BLO,
         WARD,    #Dum
         QUADRANT,#Dum
         inBID, #Dum but done
         distance,
         sale_date_l, 
         date_formed)

#####

# Making the Buffers 

make_spill <- function (i){
  data_final%>%
    filter(distance > i - 49 & distance < i)
}

for (i in seq(50,2500,50)){
  assign(paste("spill_",i,sep = ""),make_spill(i))
}

#rm(list = ls(pattern = "spill_"))

#Last dummies for the spill over model 

######
spill_match_50 <-  intersect(data_final$FID, spill_50$FID)
spill_match_100 <-  intersect(data_final$FID, spill_100$FID)
spill_match_150 <-  intersect(data_final$FID, spill_150$FID)
spill_match_200 <-  intersect(data_final$FID, spill_200$FID)
spill_match_250 <-  intersect(data_final$FID, spill_250$FID)
spill_match_300 <-  intersect(data_final$FID, spill_300$FID)
spill_match_350 <-  intersect(data_final$FID, spill_350$FID)
spill_match_400 <-  intersect(data_final$FID, spill_400$FID)
spill_match_450 <-  intersect(data_final$FID, spill_450$FID)
spill_match_500 <-  intersect(data_final$FID, spill_500$FID)
spill_match_550 <-  intersect(data_final$FID, spill_550$FID)
spill_match_600 <-  intersect(data_final$FID, spill_600$FID)
spill_match_650 <-  intersect(data_final$FID, spill_650$FID)
spill_match_700 <-  intersect(data_final$FID, spill_700$FID)
spill_match_750 <-  intersect(data_final$FID, spill_750$FID)
spill_match_800 <-  intersect(data_final$FID, spill_800$FID)
spill_match_850 <-  intersect(data_final$FID, spill_850$FID)
spill_match_900 <-  intersect(data_final$FID, spill_900$FID)
spill_match_950 <-  intersect(data_final$FID, spill_950$FID)
spill_match_1000 <-  intersect(data_final$FID, spill_1000$FID)
spill_match_1050 <-  intersect(data_final$FID, spill_1050$FID)
spill_match_1100 <-  intersect(data_final$FID, spill_1100$FID)
spill_match_1150 <-  intersect(data_final$FID, spill_1150$FID)
spill_match_1200 <-  intersect(data_final$FID, spill_1200$FID)
spill_match_1250 <-  intersect(data_final$FID, spill_1250$FID)
spill_match_1300 <-  intersect(data_final$FID, spill_1300$FID)
spill_match_1350 <-  intersect(data_final$FID, spill_1350$FID)
spill_match_1400 <-  intersect(data_final$FID, spill_1400$FID)
spill_match_1450 <-  intersect(data_final$FID, spill_1450$FID)
spill_match_1500 <-  intersect(data_final$FID, spill_1500$FID)
spill_match_1550 <-  intersect(data_final$FID, spill_1550$FID)
spill_match_1600 <-  intersect(data_final$FID, spill_1600$FID)
spill_match_1650 <-  intersect(data_final$FID, spill_1650$FID)
spill_match_1700 <-  intersect(data_final$FID, spill_1700$FID)
spill_match_1750 <-  intersect(data_final$FID, spill_1750$FID)
spill_match_1800 <-  intersect(data_final$FID, spill_1800$FID)
spill_match_1850 <-  intersect(data_final$FID, spill_1850$FID)
spill_match_1900 <-  intersect(data_final$FID, spill_1900$FID)
spill_match_1950 <-  intersect(data_final$FID, spill_1950$FID)
spill_match_2000 <-  intersect(data_final$FID, spill_2000$FID)
spill_match_2050 <-  intersect(data_final$FID, spill_2050$FID)
spill_match_2100 <-  intersect(data_final$FID, spill_2100$FID)
spill_match_2150 <-  intersect(data_final$FID, spill_2150$FID)
spill_match_2200 <-  intersect(data_final$FID, spill_2200$FID)
spill_match_2250 <-  intersect(data_final$FID, spill_2250$FID)
spill_match_2300 <-  intersect(data_final$FID, spill_2300$FID)
spill_match_2350 <-  intersect(data_final$FID, spill_2350$FID)
spill_match_2400 <-  intersect(data_final$FID, spill_2400$FID)
spill_match_2450 <-  intersect(data_final$FID, spill_2450$FID)
spill_match_2500 <-  intersect(data_final$FID, spill_2500$FID)


#######
data_final$spill_50 <- ifelse(data_final$FID %in% spill_match_50, 1,0)
data_final$spill_50_ad <- (data_final$spill_50 * data_final$after_des)  
data_final$spill_100 <- ifelse(data_final$FID %in% spill_match_100, 1,0)
data_final$spill_100_ad <- (data_final$spill_100 * data_final$after_des)  
data_final$spill_150 <- ifelse(data_final$FID %in% spill_match_150, 1,0)
data_final$spill_150_ad <- (data_final$spill_150 * data_final$after_des)  
data_final$spill_200 <- ifelse(data_final$FID %in% spill_match_200, 1,0)
data_final$spill_200_ad <- (data_final$spill_200 * data_final$after_des)
data_final$spill_250 <- ifelse(data_final$FID %in% spill_match_250, 1,0)
data_final$spill_250_ad <- (data_final$spill_250 * data_final$after_des)
data_final$spill_300 <- ifelse(data_final$FID %in% spill_match_300, 1,0)
data_final$spill_300_ad <- (data_final$spill_300 * data_final$after_des)
data_final$spill_350 <- ifelse(data_final$FID %in% spill_match_350, 1,0)
data_final$spill_350_ad <- (data_final$spill_350 * data_final$after_des)
data_final$spill_400 <- ifelse(data_final$FID %in% spill_match_400, 1,0)
data_final$spill_400_ad <- (data_final$spill_400 * data_final$after_des)
data_final$spill_450 <- ifelse(data_final$FID %in% spill_match_450, 1,0)
data_final$spill_450_ad <- (data_final$spill_450 * data_final$after_des)
data_final$spill_500 <- ifelse(data_final$FID %in% spill_match_500, 1,0)
data_final$spill_500_ad <- (data_final$spill_500 * data_final$after_des)
data_final$spill_550 <- ifelse(data_final$FID %in% spill_match_550, 1,0)
data_final$spill_550_ad <- (data_final$spill_550 * data_final$after_des)
data_final$spill_600 <- ifelse(data_final$FID %in% spill_match_600, 1,0)
data_final$spill_600_ad <- (data_final$spill_600 * data_final$after_des)
data_final$spill_650 <- ifelse(data_final$FID %in% spill_match_650, 1,0)
data_final$spill_650_ad <- (data_final$spill_650 * data_final$after_des)
data_final$spill_700 <- ifelse(data_final$FID %in% spill_match_700, 1,0)
data_final$spill_700_ad <- (data_final$spill_700 * data_final$after_des)
data_final$spill_750 <- ifelse(data_final$FID %in% spill_match_750, 1,0)
data_final$spill_750_ad <- (data_final$spill_750 * data_final$after_des)
data_final$spill_800 <- ifelse(data_final$FID %in% spill_match_800, 1,0)
data_final$spill_800_ad <- (data_final$spill_800 * data_final$after_des)
data_final$spill_850 <- ifelse(data_final$FID %in% spill_match_850, 1,0)
data_final$spill_850_ad <- (data_final$spill_850 * data_final$after_des)
data_final$spill_900 <- ifelse(data_final$FID %in% spill_match_900, 1,0)
data_final$spill_900_ad <- (data_final$spill_900 * data_final$after_des)
data_final$spill_950 <- ifelse(data_final$FID %in% spill_match_950, 1,0)
data_final$spill_950_ad <- (data_final$spill_950 * data_final$after_des)
data_final$spill_1000 <- ifelse(data_final$FID %in% spill_match_1000, 1,0)
data_final$spill_1000_ad <- (data_final$spill_1000 * data_final$after_des)
data_final$spill_1050 <- ifelse(data_final$FID %in% spill_match_1050, 1,0)
data_final$spill_1050_ad <- (data_final$spill_1050 * data_final$after_des)
data_final$spill_1100 <- ifelse(data_final$FID %in% spill_match_1100, 1,0)
data_final$spill_1100_ad <- (data_final$spill_1100 * data_final$after_des)
data_final$spill_1150 <- ifelse(data_final$FID %in% spill_match_1150, 1,0)
data_final$spill_1150_ad <- (data_final$spill_1150 * data_final$after_des)
data_final$spill_1200 <- ifelse(data_final$FID %in% spill_match_1200, 1,0)
data_final$spill_1200_ad <- (data_final$spill_1200 * data_final$after_des)
data_final$spill_1250 <- ifelse(data_final$FID %in% spill_match_1250, 1,0)
data_final$spill_1250_ad <- (data_final$spill_1250 * data_final$after_des)
data_final$spill_1300 <- ifelse(data_final$FID %in% spill_match_1300, 1,0)
data_final$spill_1300_ad <- (data_final$spill_1300 * data_final$after_des)
data_final$spill_1350 <- ifelse(data_final$FID %in% spill_match_1350, 1,0)
data_final$spill_1350_ad <- (data_final$spill_1350 * data_final$after_des)
data_final$spill_1400 <- ifelse(data_final$FID %in% spill_match_1400, 1,0)
data_final$spill_1400_ad <- (data_final$spill_1400 * data_final$after_des)
data_final$spill_1450 <- ifelse(data_final$FID %in% spill_match_1450, 1,0)
data_final$spill_1450_ad <- (data_final$spill_1450 * data_final$after_des)
data_final$spill_1500 <- ifelse(data_final$FID %in% spill_match_1500, 1,0)
data_final$spill_1500_ad <- (data_final$spill_1500 * data_final$after_des)
data_final$spill_1550 <- ifelse(data_final$FID %in% spill_match_1550, 1,0)
data_final$spill_1550_ad <- (data_final$spill_1550 * data_final$after_des)
data_final$spill_1600 <- ifelse(data_final$FID %in% spill_match_1600, 1,0)
data_final$spill_1600_ad <- (data_final$spill_1600 * data_final$after_des)
data_final$spill_1650 <- ifelse(data_final$FID %in% spill_match_1650, 1,0)
data_final$spill_1650_ad <- (data_final$spill_1650 * data_final$after_des)
data_final$spill_1700 <- ifelse(data_final$FID %in% spill_match_1700, 1,0)
data_final$spill_1700_ad <- (data_final$spill_1700 * data_final$after_des)
data_final$spill_1750 <- ifelse(data_final$FID %in% spill_match_1750, 1,0)
data_final$spill_1750_ad <- (data_final$spill_1750 * data_final$after_des)
data_final$spill_1800 <- ifelse(data_final$FID %in% spill_match_1800, 1,0)
data_final$spill_1800_ad <- (data_final$spill_1800 * data_final$after_des)
data_final$spill_1850 <- ifelse(data_final$FID %in% spill_match_1850, 1,0)
data_final$spill_1850_ad <- (data_final$spill_1850 * data_final$after_des)
data_final$spill_1900 <- ifelse(data_final$FID %in% spill_match_1900, 1,0)
data_final$spill_1900_ad <- (data_final$spill_1900 * data_final$after_des)
data_final$spill_1950 <- ifelse(data_final$FID %in% spill_match_1950, 1,0)
data_final$spill_1950_ad <- (data_final$spill_1950 * data_final$after_des)
data_final$spill_2000 <- ifelse(data_final$FID %in% spill_match_2000, 1,0)
data_final$spill_2000_ad <- (data_final$spill_2000 * data_final$after_des)
data_final$spill_2050 <- ifelse(data_final$FID %in% spill_match_2050, 1,0)
data_final$spill_2050_ad <- (data_final$spill_2050 * data_final$after_des)
data_final$spill_2100 <- ifelse(data_final$FID %in% spill_match_2100, 1,0)
data_final$spill_2100_ad <- (data_final$spill_2100 * data_final$after_des)
data_final$spill_2150 <- ifelse(data_final$FID %in% spill_match_2150, 1,0)
data_final$spill_2150_ad <- (data_final$spill_2150 * data_final$after_des)
data_final$spill_2200 <- ifelse(data_final$FID %in% spill_match_2200, 1,0)
data_final$spill_2200_ad <- (data_final$spill_2200 * data_final$after_des)
data_final$spill_2250 <- ifelse(data_final$FID %in% spill_match_2250, 1,0)
data_final$spill_2250_ad <- (data_final$spill_2250 * data_final$after_des)
data_final$spill_2300 <- ifelse(data_final$FID %in% spill_match_2300, 1,0)
data_final$spill_2300_ad <- (data_final$spill_2300 * data_final$after_des)
data_final$spill_2350 <- ifelse(data_final$FID %in% spill_match_2350, 1,0)
data_final$spill_2350_ad <- (data_final$spill_2350 * data_final$after_des)
data_final$spill_2400 <- ifelse(data_final$FID %in% spill_match_2400, 1,0)
data_final$spill_2400_ad <- (data_final$spill_2400 * data_final$after_des)
data_final$spill_2450 <- ifelse(data_final$FID %in% spill_match_2450, 1,0)
data_final$spill_2450_ad <- (data_final$spill_2450 * data_final$after_des)
data_final$spill_2500 <- ifelse(data_final$FID %in% spill_match_2500, 1,0)
data_final$spill_2500_ad <- (data_final$spill_2500 * data_final$after_des)

########

data_spillover_buffers <-  data_final%>%
  select(FID,
         PRICE,
         spill_50,
         spill_100,
         spill_150,
         spill_200,
         spill_250,
         spill_300,
         spill_350,
         spill_400,
         spill_450,
         spill_500,
         spill_550,
         spill_600,
         spill_650,
         spill_700,
         spill_750,
         spill_800,
         spill_850,
         spill_900,
         spill_950,
         spill_1000,
         spill_1050,
         spill_1100,
         spill_1150,
         spill_1200,
         spill_1250,
         spill_1300,
         spill_1350,
         spill_1400,
         spill_1450,
         spill_1500,
         spill_1550,
         spill_1600,
         spill_1650,
         spill_1700,
         spill_1750,
         spill_1800,
         spill_1850,
         spill_1900,
         spill_1950,
         spill_2000,
         spill_2050,
         spill_2100,
         spill_2150,
         spill_2200,
         spill_2250,
         spill_2300,
         spill_2350,
         spill_2400,
         spill_2450,
         spill_2500,
         spill_50_ad,
         spill_100_ad,
         spill_150_ad,
         spill_200_ad,
         spill_250_ad,
         spill_300_ad,
         spill_350_ad,
         spill_400_ad,
         spill_450_ad,
         spill_500_ad,
         spill_550_ad,
         spill_600_ad,
         spill_650_ad,
         spill_700_ad,
         spill_750_ad,
         spill_800_ad,
         spill_850_ad,
         spill_900_ad,
         spill_950_ad,
         spill_1000_ad,
         spill_1050_ad,
         spill_1100_ad,
         spill_1150_ad,
         spill_1200_ad,
         spill_1250_ad,
         spill_1300_ad,
         spill_1350_ad,
         spill_1400_ad,
         spill_1450_ad,
         spill_1500_ad,
         spill_1550_ad,
         spill_1600_ad,
         spill_1650_ad,
         spill_1700_ad,
         spill_1750_ad,
         spill_1800_ad,
         spill_1850_ad,
         spill_1900_ad,
         spill_1950_ad,
         spill_2000_ad,
         spill_2050_ad,
         spill_2100_ad,
         spill_2150_ad,
         spill_2200_ad,
         spill_2250_ad,
         spill_2300_ad,
         spill_2350_ad,
         spill_2400_ad,
         spill_2450_ad,
         spill_2500_ad)








# Adding in dummy variables

dum_HF_BATHRM <- dummy(data_final_cleaning$HF_BATHRM, sep = "_")
dum_HEAT <- dummy(data_final_cleaning$HEAT, sep = "_")
dum_AC <- dummy(data_final_cleaning$AC, sep = "_")
dum_AYB <- dummy(data_final_cleaning$AYB, sep = "_")
#dum_YR_RMDL <- dummy(data_final_cleaning$YR_RMDL, sep = "_")
dum_QUALIFIED <- dummy(data_final_cleaning$QUALIFIED, sep = "_")
dum_SOURCE <- dummy(data_final_cleaning$SOURCE, sep = "_")
dum_ZIPCODE <- dummy(data_final_cleaning$ZIPCODE, sep = "_")
dum_WARD <- dummy(data_final_cleaning$WARD, sep = "_")
dum_QUADRANT <- dummy(data_final_cleaning$QUADRANT, sep = "_")
dum_inBID <- dummy(data_final_cleaning$inBID, sep = "_")
#dum_STYLE <- dummy(data_final_cleaning$STYLE, sep = ".")
dum_STRUCT <- dummy(data_final_cleaning$STRUCT, sep = "_")
dum_GRADE <- dummy(data_final_cleaning$GRADE, sep = "_")
dum_CNDTN <- dummy(data_final_cleaning$CNDTN, sep = "_")
dum_EXTWALL <- dummy(data_final_cleaning$EXTWALL, sep = "_")
dum_ROOF <- dummy(data_final_cleaning$ROOF, sep = "_")
dum_INTWALL <- dummy(data_final_cleaning$INTWALL, sep = "_")
dum_CENSUS_BLO <- dummy(data_final_cleaning$CENSUS_BLO, sep = "_")
dum_GIS_ID <- dummy(data_final_cleaning$GIS_ID, sep = "_")

#Quantitative variables labels 

FID_var <-  data_final_cleaning$FID
BATHRM_var <- data_final_cleaning$BATHRM
# data_final_cleaning$NUM_UNITS, #not helpful
ROOMS_var <- data_final_cleaning$ROOMS
BEDRM_var <- data_final_cleaning$BEDRM
#YR_RMDL,
#EYB,
STORIES_var <- data_final_cleaning$STORIES
KITCHENS_var <- data_final_cleaning$KITCHENS
FIREPLACES_var <- data_final_cleaning$FIREPLACES
landarea_var <-  log(data_final_cleaning$LANDAREA) #not helpful
squarefoot_var <- log(data_final_cleaning$GBA)
price_var <- log(data_final_cleaning$PRICE)
#squarefoot_inBID_var <- (log(data_final_cleaning$GBA)*data_final_cleaning$inBID)
#LONGITUDE,
#LATITUDE,
#CENSUS_BLO,
distance_var <- data_final_cleaning$distance  
sale_date_l_var <- data_final_cleaning$sale_date_l 
inBID_var <- data_final_cleaning$inBID
after_des_var <- data_final_cleaning$after_des
after_InBID_designated_var <- (data_final_cleaning$inBID * data_final_cleaning$after_des)
## ADD IN CENSUS BLOCK INTERACTION WITH SALE YEAR! 



all_dummies_spill <- as.data.frame(cbind(data_final_cleaning$FID,  # CHANGED DATA_FINAL_CLEANING$FID TO DATA_SPILLOVER_BUFFERS$FID
                                         data_spillover_buffers$PRICE,
                                         dum_HF_BATHRM,
                                         dum_CENSUS_BLO,
                                         #dum_HEAT, #not helpful
                                         dum_AC,
                                         #dum_AYB,
                                         #dum_QUALIFIED,
                                         # dum_SOURCE, #not helpful
                                         dum_ZIPCODE,
                                         dum_WARD,
                                         #dum_QUADRANT, #not helpful
                                         # dum_STYLE, #not helpful
                                         #dum_STRUCT, #not helpful
                                         dum_GRADE,
                                         data_spillover_buffers$spill_50,
                                         data_spillover_buffers$spill_100,
                                         data_spillover_buffers$spill_150,
                                         data_spillover_buffers$spill_200,
                                         data_spillover_buffers$spill_250,
                                         data_spillover_buffers$spill_300,
                                         data_spillover_buffers$spill_350,
                                         data_spillover_buffers$spill_400,
                                         data_spillover_buffers$spill_450,
                                         data_spillover_buffers$spill_500,
                                         data_spillover_buffers$spill_550,
                                         data_spillover_buffers$spill_600,
                                         data_spillover_buffers$spill_650,
                                         data_spillover_buffers$spill_700,
                                         data_spillover_buffers$spill_750,
                                         data_spillover_buffers$spill_800,
                                         data_spillover_buffers$spill_850,
                                         data_spillover_buffers$spill_900,
                                         data_spillover_buffers$spill_950,
                                         data_spillover_buffers$spill_1000,
                                         data_spillover_buffers$spill_1050,
                                         data_spillover_buffers$spill_1100,
                                         data_spillover_buffers$spill_1150,
                                         data_spillover_buffers$spill_1200,
                                         data_spillover_buffers$spill_1250,
                                         data_spillover_buffers$spill_1300,
                                         data_spillover_buffers$spill_1350,
                                         data_spillover_buffers$spill_1400,
                                         data_spillover_buffers$spill_1450,
                                         data_spillover_buffers$spill_1500,
                                         data_spillover_buffers$spill_1550,
                                         data_spillover_buffers$spill_1600,
                                         data_spillover_buffers$spill_1650,
                                         data_spillover_buffers$spill_1700,
                                         data_spillover_buffers$spill_1750,
                                         data_spillover_buffers$spill_1800,
                                         data_spillover_buffers$spill_1850,
                                         data_spillover_buffers$spill_1900,
                                         data_spillover_buffers$spill_1950,
                                         data_spillover_buffers$spill_2000,
                                         data_spillover_buffers$spill_2050,
                                         data_spillover_buffers$spill_2100,
                                         data_spillover_buffers$spill_2150,
                                         data_spillover_buffers$spill_2200,
                                         data_spillover_buffers$spill_2250,
                                         data_spillover_buffers$spill_2300,
                                         data_spillover_buffers$spill_2350,
                                         data_spillover_buffers$spill_2400,
                                         data_spillover_buffers$spill_2450,
                                         data_spillover_buffers$spill_2500,
                                         dum_CNDTN,
                                         data_spillover_buffers$spill_50_ad,
                                         data_spillover_buffers$spill_100_ad,
                                         data_spillover_buffers$spill_150_ad,
                                         data_spillover_buffers$spill_200_ad,
                                         data_spillover_buffers$spill_250_ad,
                                         data_spillover_buffers$spill_300_ad,
                                         data_spillover_buffers$spill_350_ad,
                                         data_spillover_buffers$spill_400_ad,
                                         data_spillover_buffers$spill_450_ad,
                                         data_spillover_buffers$spill_500_ad,
                                         data_spillover_buffers$spill_550_ad,
                                         data_spillover_buffers$spill_600_ad,
                                         data_spillover_buffers$spill_650_ad,
                                         data_spillover_buffers$spill_700_ad,
                                         data_spillover_buffers$spill_750_ad,
                                         data_spillover_buffers$spill_800_ad,
                                         data_spillover_buffers$spill_850_ad,
                                         data_spillover_buffers$spill_900_ad,
                                         data_spillover_buffers$spill_950_ad,
                                         data_spillover_buffers$spill_1000_ad,
                                         data_spillover_buffers$spill_1050_ad,
                                         data_spillover_buffers$spill_1100_ad,
                                         data_spillover_buffers$spill_1150_ad,
                                         data_spillover_buffers$spill_1200_ad,
                                         data_spillover_buffers$spill_1250_ad,
                                         data_spillover_buffers$spill_1300_ad,
                                         data_spillover_buffers$spill_1350_ad,
                                         data_spillover_buffers$spill_1400_ad,
                                         data_spillover_buffers$spill_1450_ad,
                                         data_spillover_buffers$spill_1500_ad,
                                         data_spillover_buffers$spill_1550_ad,
                                         data_spillover_buffers$spill_1600_ad,
                                         data_spillover_buffers$spill_1650_ad,
                                         data_spillover_buffers$spill_1700_ad,
                                         data_spillover_buffers$spill_1750_ad,
                                         data_spillover_buffers$spill_1800_ad,
                                         data_spillover_buffers$spill_1850_ad,
                                         data_spillover_buffers$spill_1900_ad,
                                         data_spillover_buffers$spill_1950_ad,
                                         data_spillover_buffers$spill_2000_ad,
                                         data_spillover_buffers$spill_2050_ad,
                                         data_spillover_buffers$spill_2100_ad,
                                         data_spillover_buffers$spill_2150_ad,
                                         data_spillover_buffers$spill_2200_ad,
                                         data_spillover_buffers$spill_2250_ad,
                                         data_spillover_buffers$spill_2300_ad,
                                         data_spillover_buffers$spill_2350_ad,
                                         data_spillover_buffers$spill_2400_ad,
                                         data_spillover_buffers$spill_2450_ad,
                                         data_spillover_buffers$spill_2500_ad)) ###




all_quantitative<- as.data.frame(cbind(data_final_cleaning$FID,
                                       BATHRM_var,
                                       # data_final_cleaning$NUM_UNITS, #not helpful
                                       ROOMS_var, 
                                       BEDRM_var,
                                       #YR_RMDL,
                                       #EYB,
                                       STORIES_var,
                                       KITCHENS_var,
                                       FIREPLACES_var,
                                       # data_final_cleaning$LANDAREA, #not helpful
                                       #distance_var,
                                       sale_date_l_var,
                                       #landarea_var,
                                       squarefoot_var,
                                       price_var,
                                       #squarefoot_inBID_var,
                                       after_des_var,
                                       after_InBID_designated_var))


#dum_EXTWALL, #not helpful
#dum_ROOF,
# dum_INTWALL, #not helping
#dum_GIS_ID))
dim(all_dummies_spill)
dim(all_quantitative)


data_final_model_withjoin <- full_join(all_dummies_spill,all_quantitative)

#remove V1 used to join the dfs

data_final_model <- subset(data_final_model_withjoin, select = -(V1))
data_final_model <- data.frame(data_final_model)

data_final_model$price_log <- data_final_model_withjoin$price_var
data_final_model <- subset(data_final_model, select = -c(price_var,V2))

# Removing observations with high Cooks Distance

#data_final_model <- data_final_model[-c(15484),]

model_BID <- lm(data_final_model$price_log ~.,data_final_model)

dim(data_final_model)

summary(model_BID)

stargazer(model_BID, title = "Results",out = "results.txt")

plot(model_BID)
###