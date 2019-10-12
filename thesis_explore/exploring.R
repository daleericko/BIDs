### Erick Cohen
### 29 January 2019 
### Business Improvement Districts and property values Economics Thesis

#installing packages
library("tidyverse") 
library("lubridate") # for sale date calculations
library("ggthemes") # for additional ggplot themes 
library("dummies") # For creating dummy variables
library("broom") # For making model tables

### Datasets section
#Loading in the dataset on DC Housing from Kaggle
#library(readr)
housing_DC <- read_csv("Documents/Coding/R Projects/thesis_explore/dc-residential-properties/DC_Properties.csv")
dim(housing_DC)
#View(housing_DC)

#Loading in dataset on BID locations from DC Open data (See if you can read it through an API)
library(readr)
BID_locations <- read_csv("Documents/Coding/R Projects/thesis_explore/dc-residential-properties/Business_Improvement_Districts.csv")
View(BID_locations) 

#Loading in dataset with location in or outside of BID labeled. 
library(readr)
DC_Prop_BID_Join <- read_csv("DC_Prop_BID_Join.csv")
View(DC_Prop_BID_Join)
test_4 <-  (DC_Prop_BID_Join$PRICE == 0)
table(test_4)

View(DC_Prop_BID_join)


#Lodaing in dataset with BID distances (No NA's)
library(readr)
master_BID_distance <- read_csv("Documents/Coding/R Projects/thesis_explore/master_BID_distance.csv")
View(master_BID_distance)
dim(master_BID_distance)

View(master_BID_distance)

#filtering out outlier prices
master_BID_xsv <- master_BID_distance%>% #already filtered for price in mbdistance
  filter(PRICE<1200000)%>%
  filter(PRICE>1)%>%
  filter(!is.na(PRICE))%>%
  filter(GBA>1)%>%
  filter(!is.na(GBA))%>%
  filter(LANDAREA >1)%>%
  filter(!is.na(LANDAREA))

dim(master_BID_xsv)  


# lubridate saledate 
sale_date_l <- mdy_hms(master_BID_xsv$SALEDATE) # add this to master_xsv_nona 
#sale_date_l
# final data set with everything 
data_final <- cbind(master_BID_xsv,sale_date_l) # double check to make sure there are no NAs ; try to change inBID boolean from false to true
#View(data_final)
#Adding column to BID locations to get dates the BIDS were formed 
BID_locations$date_formed <-  c(2007,2004,1997,1997,1999,2005,2018,2012,2007,2015,2002)

# Plotting saledate 
sale_date_hist <-  ggplot(data_final,
                          aes(sale_date_l))+
  geom_histogram(fill= "red")+
  labs(title = "DC Property Sale Dates",
       x= "Sale Date")
rm(sale_date_hist)

sale_date_hist +theme_economist()


### Identifying properties in spillover areas 

ggplot(data_final,
       aes(distance))+
  geom_histogram(fill="royal blue")+
  labs(title = "Distance to BID",
       y=waiver(),
       x= "Distance (meters)")



### Model
# ln P ~ alpha + B1Z1 + B2 Z*t-5 + B3 Z*t-2 + B4 Z*t0 + B5 Z*t+5 + gammaX1 + deltaX2 + nuX3 + E 
# Where X1 = Vector of property characteristics , X2 = Buffer zones ( at what point does it stop mattering plot residuals), X3= Vector of neighborhood char



# Starting to make the model vectors
# In Bid TRUE or FALSE
data_final$inBID <- ifelse(is.na(data_final$GIS_ID),0,1)
View(BID_locations)
sum(data_final$inBID) # BIG Problem. Not that many inside the dataset. But we can still test for spillovers. 

# Adding date the BID was formed
data_final <-  merge(data_final, BID_locations[, c("GIS_ID", "date_formed")], by="GIS_ID",all = TRUE)
View(data_final)
dim(data_final)


# Mutate rows to add boleans for t-5, t-2, and t+5 

data_final <- data_final%>%
  mutate(years_since_des = year(sale_date_l)- date_formed)

# Dummies for time BID designation; (I don't have enough observations for this)
data_final$t_minus_five <- ifelse(data_final$years_since_des >=-5 & data_final$years_since_des<=0, 1,0)
data_final$t_minus_two <- ifelse(data_final$years_since_des >=-2 & data_final$years_since_des<=0, 1,0)
# data_final$t_at_zero <- ifelse(data_final$years_since_des ==0, 1,0) # Not enough
data_final$t_plus_five <- ifelse(data_final$years_since_des <=5 & data_final$years_since_des>=0, 1,0)



View(data_final)
dim(data_final)

### Building the model with data_final dataset 

#data final for the model filtering out columns not needed

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
         date_formed) #Dum but done 

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
#dum_CENSUS_BLO <- dummy(data_final_cleaning$CENSUS_BLO, sep = "_")
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
squarefoot_inBID_var <- (log(data_final_cleaning$GBA)*data_final_cleaning$inBID)
#LONGITUDE,
#LATITUDE,
#CENSUS_BLO,
distance_var <- data_final_cleaning$distance  ### NO SPILLOVER!
sale_date_l_var <- data_final_cleaning$sale_date_l 
inBID_var <- data_final_cleaning$inBID
#data_final_cleaning$years_since_des, #not helpful
t_minus_five_var <- data_final_cleaning$t_minus_five #Dum but done
t_minus_two_var <- data_final_cleaning$t_minus_two #Dum but done
#t_at_zero_var <- data_final_cleaning$t_at_zero #Dum but done
#t_plus_two_var <- data_final_cleaning$t_plus_two #Dum but done
t_plus_five_var <- data_final_cleaning$t_plus_five   #Dum but done 



all_dummies <- as.data.frame(cbind(data_final_cleaning$FID,
                               dum_HF_BATHRM,
                               #dum_HEAT, #not helpful
                                dum_AC,
                                #dum_AYB,
                                #dum_QUALIFIED,
                               # dum_SOURCE, #not helpful
                                dum_ZIPCODE,
                                dum_WARD,
                                #dum_QUADRANT, #not helpful
                                #dum_inBID,
                               # dum_STYLE, #not helpful
                                #dum_STRUCT, #not helpful
                                dum_GRADE,
                                dum_CNDTN)) ###
                                #dum_EXTWALL, #not helpful
                                #dum_ROOF,
                               # dum_INTWALL, #not helping
                                #dum_GIS_ID))
dim(all_dummies)
dim(all_quantitative)

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
                                       #LONGITUDE,
                                       #LATITUDE,
                                       #CENSUS_BLO,
                                       #distance_var,
                                       sale_date_l_var,
                                       #landarea_var,
                                       #squarefoot_var,
                                       price_var,
                                       squarefoot_inBID_var,
                                       #inBID_var,
                                       #data_final_cleaning$years_since_des, #not helpful
                                       (data_final_cleaning$inBID* t_minus_five_var), #V9
                                       (data_final_cleaning$inBID*t_minus_two_var), #V10
                                       (data_final_cleaning$inBID*t_at_zero_var), #V11
                                       (data_final_cleaning$inBID*t_plus_two_var), #V12
                                       (data_final_cleaning$inBID*t_plus_five_var))) #V13
                                       
data_final_model_withjoin <- full_join(all_dummies,all_quantitative)

#remove V1 used to join the dfs

data_final_model$price_log <- data_final_model_withjoin$price_var

data_final_model <- subset(data_final_model_withjoin, select = -(V1))

data_final_model <- data.frame(data_final_model)


model_BID <- lm(data_final_model$price_var ~.,data_final_model)

plot(log(data_final$PRICE),data_final_model$V5)

model_BID$coefficients

summary(model_BID)

out <- tidy(model_BID)

kable(out)

### 

#Testing Spill over by creating buffers 

rm(dist_10,dist_20,dist_30, dist_40, dist_50)
dist_10 <- data_final%>%
  filter(distance >1 & distance <10)
dist_20 <- data_final%>%
  filter(distance > 11 & distance < 20)
dist_30 <- data_final%>%
  filter(distance > 21 & distance < 30)
dist_40 <- data_final%>%
  filter(distance > 31 & distance < 40)
dist_50 <- data_final%>%
  filter(distance > 41 & distance < 50)
dist_60 <- data_final%>%
  filter(distance > 51 & distance < 60)
dist_70 <- data_final%>%
  filter(distance > 61 & distance < 70)
dist_80 <- data_final%>%
  filter(distance > 71 & distance < 80)
dist_90 <- data_final%>%
  filter(distance > 81 & distance < 90)

# Making a function and for loop to make buffer areas 
make_dist <- function (i){
  data_final%>%
    select(distance,PRICE)%>%
    filter(distance > i - 49 & distance < i)
}


for (i in seq(50,5500,50)){
  assign(paste("dist_",i,sep = ""),make_dist(i))
}





#rm(list=ls(pattern = "dist_"))


# for loop to print the object names
for (i in seq(50,2000,50)){
  median((paste("(dist_",i,")",sep = "")))
  i=i
}


rm(median_prices)
median_prices <- c(median(dist_50$PRICE),
                   median(dist_100$PRICE),
                   median(dist_150$PRICE),
                   median(dist_200$PRICE),
                   median(dist_250$PRICE),
                   median(dist_300$PRICE),
                   median(dist_350$PRICE),
                   median(dist_400$PRICE),
                   median(dist_450$PRICE),
                   median(dist_500$PRICE),
                   median(dist_550$PRICE),
                   median(dist_600$PRICE),
                   median(dist_650$PRICE),
                   median(dist_700$PRICE),
                   median(dist_750$PRICE),
                   median(dist_800$PRICE),
                   median(dist_850$PRICE),
                   median(dist_900$PRICE),
                   median(dist_950$PRICE),
                   median(dist_1000$PRICE),
                   median(dist_1050$PRICE),
                   median(dist_1100$PRICE),
                   median(dist_1200$PRICE),
                   median(dist_1250$PRICE),
                   median(dist_1300$PRICE),
                   median(dist_1350$PRICE),
                   median(dist_1400$PRICE),
                   median(dist_1450$PRICE),
                   median(dist_1500$PRICE),
                   median(dist_1550$PRICE),
                   median(dist_1600$PRICE),
                   median(dist_1650$PRICE),
                   median(dist_1700$PRICE),
                   median(dist_1750$PRICE),
                   median(dist_1800$PRICE),
                   median(dist_1850$PRICE),
                   median(dist_1900$PRICE),
                   median(dist_1950$PRICE),
                   median(dist_2000$PRICE),
                   median(dist_2050$PRICE),
                   median(dist_2100$PRICE),
                   median(dist_2150$PRICE),
                   median(dist_2200$PRICE),
                   median(dist_2250$PRICE),
                   median(dist_3000$PRICE),
                   median(dist_3050$PRICE),
                   median(dist_3100$PRICE),
                   median(dist_3150$PRICE),
                   median(dist_3200$PRICE),
                   median(dist_3250$PRICE),
                   median(dist_3300$PRICE),
                   median(dist_3050$PRICE),
                   median(dist_3100$PRICE),
                   median(dist_3150$PRICE),
                   median(dist_3200$PRICE),
                   median(dist_3250$PRICE),
                   median(dist_3300$PRICE),
                   median(dist_3350$PRICE),
                   median(dist_3400$PRICE),
                   median(dist_3450$PRICE),
                   median(dist_3400$PRICE),
                   median(dist_3450$PRICE),
                   median(dist_3500$PRICE),
                   median(dist_3550$PRICE),
                   median(dist_3600$PRICE),
                   median(dist_3650$PRICE),
                   median(dist_3700$PRICE),
                   median(dist_3750$PRICE),
                   median(dist_3800$PRICE),
                   median(dist_3850$PRICE),
                   median(dist_3900$PRICE),
                   median(dist_3950$PRICE),
                   median(dist_4000$PRICE),
                   median(dist_4050$PRICE),
                   median(dist_4100$PRICE),
                   median(dist_4150$PRICE),
                   median(dist_4200$PRICE),
                   median(dist_4250$PRICE),
                   median(dist_4300$PRICE),
                   median(dist_4050$PRICE),
                   median(dist_4100$PRICE),
                   median(dist_4150$PRICE),
                   median(dist_4200$PRICE),
                   median(dist_4250$PRICE),
                   median(dist_4300$PRICE),
                   median(dist_4350$PRICE),
                   median(dist_4400$PRICE),
                   median(dist_4450$PRICE),
                   median(dist_4400$PRICE),
                   median(dist_4450$PRICE),
                   median(dist_4500$PRICE),
                   median(dist_4550$PRICE),
                   median(dist_4600$PRICE),
                   median(dist_4650$PRICE),
                   median(dist_4700$PRICE),
                   median(dist_4750$PRICE),
                   median(dist_4800$PRICE),
                   median(dist_4850$PRICE),
                   median(dist_4900$PRICE),
                   median(dist_4950$PRICE),
                   median(dist_5000$PRICE),
                   median(dist_5050$PRICE),
                   median(dist_5100$PRICE),
                   median(dist_5150$PRICE),
                   median(dist_5200$PRICE),
                   median(dist_5250$PRICE),
                   median(dist_5300$PRICE),
                   median(dist_5350$PRICE),
                   median(dist_5400$PRICE),
                   median(dist_5450$PRICE),
                   median(dist_5400$PRICE),
                   median(dist_5450$PRICE),
                   median(dist_5500$PRICE))
                     
rm(distances_BIDS)
med_distances_BIDS <- c(median(dist_50$distance),
                        median(dist_100$distance),
                        median(dist_150$distance),
                        median(dist_200$distance),
                        median(dist_250$distance),
                        median(dist_300$distance),
                        median(dist_350$distance),
                        median(dist_400$distance),
                        median(dist_450$distance),
                        median(dist_500$distance),
                        median(dist_550$distance),
                        median(dist_600$distance),
                        median(dist_650$distance),
                        median(dist_700$distance),
                        median(dist_750$distance),
                        median(dist_800$distance),
                        median(dist_850$distance),
                        median(dist_900$distance),
                        median(dist_950$distance),
                        median(dist_1000$distance),
                        median(dist_1050$distance),
                        median(dist_1100$distance),
                        median(dist_1200$distance),
                        median(dist_1250$distance),
                        median(dist_1300$distance),
                        median(dist_1350$distance),
                        median(dist_1400$distance),
                        median(dist_1450$distance),
                        median(dist_1500$distance),
                        median(dist_1550$distance),
                        median(dist_1600$distance),
                        median(dist_1650$distance),
                        median(dist_1700$distance),
                        median(dist_1750$distance),
                        median(dist_1800$distance),
                        median(dist_1850$distance),
                        median(dist_1900$distance),
                        median(dist_1950$distance),
                        median(dist_2000$distance),
                        median(dist_2050$distance),
                        median(dist_2100$distance),
                        median(dist_2150$distance),
                        median(dist_2200$distance),
                        median(dist_2250$distance),
                        median(dist_3000$distance),
                        median(dist_3050$distance),
                        median(dist_3100$distance),
                        median(dist_3150$distance),
                        median(dist_3200$distance),
                        median(dist_3250$distance),
                        median(dist_3300$distance),
                        median(dist_3050$distance),
                        median(dist_3100$distance),
                        median(dist_3150$distance),
                        median(dist_3200$distance),
                        median(dist_3250$distance),
                        median(dist_3300$distance),
                        median(dist_3350$distance),
                        median(dist_3400$distance),
                        median(dist_3450$distance),
                        median(dist_3400$distance),
                        median(dist_3450$distance),
                        median(dist_3500$distance),
                        median(dist_3550$distance),
                        median(dist_3600$distance),
                        median(dist_3650$distance),
                        median(dist_3700$distance),
                        median(dist_3750$distance),
                        median(dist_3800$distance),
                        median(dist_3850$distance),
                        median(dist_3900$distance),
                        median(dist_3950$distance),
                        median(dist_4000$distance),
                        median(dist_4050$distance),
                        median(dist_4100$distance),
                        median(dist_4150$distance),
                        median(dist_4200$distance),
                        median(dist_4250$distance),
                        median(dist_4300$distance),
                        median(dist_4050$distance),
                        median(dist_4100$distance),
                        median(dist_4150$distance),
                        median(dist_4200$distance),
                        median(dist_4250$distance),
                        median(dist_4300$distance),
                        median(dist_4350$distance),
                        median(dist_4400$distance),
                        median(dist_4450$distance),
                        median(dist_4400$distance),
                        median(dist_4450$distance),
                        median(dist_4500$distance),
                        median(dist_4550$distance),
                        median(dist_4600$distance),
                        median(dist_4650$distance),
                        median(dist_4700$distance),
                        median(dist_4750$distance),
                        median(dist_4800$distance),
                        median(dist_4850$distance),
                        median(dist_4900$distance),
                        median(dist_4950$distance),
                        median(dist_5000$distance),
                        median(dist_5050$distance),
                        median(dist_5100$distance),
                        median(dist_5150$distance),
                        median(dist_5200$distance),
                        median(dist_5250$distance),
                        median(dist_5300$distance),
                        median(dist_5350$distance),
                        median(dist_5400$distance),
                        median(dist_5450$distance),
                        median(dist_5400$distance),
                        median(dist_5450$distance),
                        median(dist_5500$distance))

plot(med_distances_BIDS,median_prices)
# should it be log of all prices 


# Second Buffer model for spillover effects 

make_spill <- function (i){
  data_final%>%
    filter(distance > i - 49 & distance < i)
}

for (i in seq(50,5500,50)){
  assign(paste("spill_",i,sep = ""),make_spill(i))
}

# rm(list = ls(pattern = "spill_"))


#Last dummies for the spill over model 

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
spill_match_2550 <-  intersect(data_final$FID, spill_2550$FID)
spill_match_2600 <-  intersect(data_final$FID, spill_2600$FID)
spill_match_2650 <-  intersect(data_final$FID, spill_2650$FID)
spill_match_2700 <-  intersect(data_final$FID, spill_2700$FID)
spill_match_2750 <-  intersect(data_final$FID, spill_2750$FID)
spill_match_2800 <-  intersect(data_final$FID, spill_2800$FID)
spill_match_2850 <-  intersect(data_final$FID, spill_2850$FID)
spill_match_2900 <-  intersect(data_final$FID, spill_2900$FID)
spill_match_2950 <-  intersect(data_final$FID, spill_2950$FID)
spill_match_3000 <-  intersect(data_final$FID, spill_3000$FID)
spill_match_3050 <-  intersect(data_final$FID, spill_3050$FID)
spill_match_3100 <-  intersect(data_final$FID, spill_3100$FID)
spill_match_3150 <-  intersect(data_final$FID, spill_3150$FID)
spill_match_3200 <-  intersect(data_final$FID, spill_3200$FID)
spill_match_3250 <-  intersect(data_final$FID, spill_3250$FID)
spill_match_3300 <-  intersect(data_final$FID, spill_3300$FID)
spill_match_3350 <-  intersect(data_final$FID, spill_3350$FID)
spill_match_3400 <-  intersect(data_final$FID, spill_3400$FID)
spill_match_3450 <-  intersect(data_final$FID, spill_3450$FID)
spill_match_3500 <-  intersect(data_final$FID, spill_3500$FID)
spill_match_3550 <-  intersect(data_final$FID, spill_3550$FID)
spill_match_3600 <-  intersect(data_final$FID, spill_3600$FID)
spill_match_3650 <-  intersect(data_final$FID, spill_3650$FID)
spill_match_3700 <-  intersect(data_final$FID, spill_3700$FID)
spill_match_3750 <-  intersect(data_final$FID, spill_3750$FID)
spill_match_3800 <-  intersect(data_final$FID, spill_3800$FID)
spill_match_3850 <-  intersect(data_final$FID, spill_3850$FID)
spill_match_3900 <-  intersect(data_final$FID, spill_3900$FID)
spill_match_3950 <-  intersect(data_final$FID, spill_3950$FID)
spill_match_4000 <-  intersect(data_final$FID, spill_4000$FID)
spill_match_4050 <-  intersect(data_final$FID, spill_4050$FID)
spill_match_4100 <-  intersect(data_final$FID, spill_4100$FID)
spill_match_4150 <-  intersect(data_final$FID, spill_4150$FID)
spill_match_4200 <-  intersect(data_final$FID, spill_4200$FID)
spill_match_4250 <-  intersect(data_final$FID, spill_4250$FID)
spill_match_4300 <-  intersect(data_final$FID, spill_4300$FID)
spill_match_4350 <-  intersect(data_final$FID, spill_4350$FID)
spill_match_4400 <-  intersect(data_final$FID, spill_4400$FID)
spill_match_4450 <-  intersect(data_final$FID, spill_4450$FID)
spill_match_4500 <-  intersect(data_final$FID, spill_4500$FID)
spill_match_4550 <-  intersect(data_final$FID, spill_4550$FID)
spill_match_4600 <-  intersect(data_final$FID, spill_4600$FID)
spill_match_4650 <-  intersect(data_final$FID, spill_4650$FID)
spill_match_4700 <-  intersect(data_final$FID, spill_4700$FID)
spill_match_4750 <-  intersect(data_final$FID, spill_4750$FID)
spill_match_4800 <-  intersect(data_final$FID, spill_4800$FID)
spill_match_4850 <-  intersect(data_final$FID, spill_4850$FID)
spill_match_4900 <-  intersect(data_final$FID, spill_4900$FID)
spill_match_4950 <-  intersect(data_final$FID, spill_4950$FID)
spill_match_5000 <-  intersect(data_final$FID, spill_5000$FID)
spill_match_5050 <-  intersect(data_final$FID, spill_5050$FID)
spill_match_5100 <-  intersect(data_final$FID, spill_5100$FID)
spill_match_5150 <-  intersect(data_final$FID, spill_5150$FID)
spill_match_5200 <-  intersect(data_final$FID, spill_5200$FID)
spill_match_5250 <-  intersect(data_final$FID, spill_5250$FID)
spill_match_5300 <-  intersect(data_final$FID, spill_5300$FID)
spill_match_5350 <-  intersect(data_final$FID, spill_5350$FID)
spill_match_5400 <-  intersect(data_final$FID, spill_5400$FID)
spill_match_5450 <-  intersect(data_final$FID, spill_5450$FID)
spill_match_5500 <-  intersect(data_final$FID, spill_5500$FID)





data_final$spill_50 <- ifelse(data_final$FID %in% spill_match_50, 1,0)
data_final$spill_100 <- ifelse(data_final$FID %in% spill_match_100, 1,0)
data_final$spill_150 <- ifelse(data_final$FID %in% spill_match_150, 1,0)
data_final$spill_200 <- ifelse(data_final$FID %in% spill_match_200, 1,0)
data_final$spill_250 <- ifelse(data_final$FID %in% spill_match_250, 1,0)
data_final$spill_300 <- ifelse(data_final$FID %in% spill_match_300, 1,0)
data_final$spill_350 <- ifelse(data_final$FID %in% spill_match_350, 1,0)
data_final$spill_400 <- ifelse(data_final$FID %in% spill_match_400, 1,0)
data_final$spill_450 <- ifelse(data_final$FID %in% spill_match_450, 1,0)
data_final$spill_500 <- ifelse(data_final$FID %in% spill_match_500, 1,0)
data_final$spill_550 <- ifelse(data_final$FID %in% spill_match_550, 1,0)
data_final$spill_600 <- ifelse(data_final$FID %in% spill_match_600, 1,0)
data_final$spill_650 <- ifelse(data_final$FID %in% spill_match_650, 1,0)
data_final$spill_700 <- ifelse(data_final$FID %in% spill_match_700, 1,0)
data_final$spill_750 <- ifelse(data_final$FID %in% spill_match_750, 1,0)
data_final$spill_800 <- ifelse(data_final$FID %in% spill_match_800, 1,0)
data_final$spill_850 <- ifelse(data_final$FID %in% spill_match_850, 1,0)
data_final$spill_900 <- ifelse(data_final$FID %in% spill_match_900, 1,0)
data_final$spill_950 <- ifelse(data_final$FID %in% spill_match_950, 1,0)
data_final$spill_1000 <- ifelse(data_final$FID %in% spill_match_1000, 1,0)
data_final$spill_1050 <- ifelse(data_final$FID %in% spill_match_1050, 1,0)
data_final$spill_1100 <- ifelse(data_final$FID %in% spill_match_1100, 1,0)
data_final$spill_1150 <- ifelse(data_final$FID %in% spill_match_1150, 1,0)
data_final$spill_1200 <- ifelse(data_final$FID %in% spill_match_1200, 1,0)
data_final$spill_1250 <- ifelse(data_final$FID %in% spill_match_1250, 1,0)
data_final$spill_1300 <- ifelse(data_final$FID %in% spill_match_1300, 1,0)
data_final$spill_1350 <- ifelse(data_final$FID %in% spill_match_1350, 1,0)
data_final$spill_1400 <- ifelse(data_final$FID %in% spill_match_1400, 1,0)
data_final$spill_1450 <- ifelse(data_final$FID %in% spill_match_1450, 1,0)
data_final$spill_1500 <- ifelse(data_final$FID %in% spill_match_1500, 1,0)
data_final$spill_1550 <- ifelse(data_final$FID %in% spill_match_1550, 1,0)
data_final$spill_1600 <- ifelse(data_final$FID %in% spill_match_1600, 1,0)
data_final$spill_1650 <- ifelse(data_final$FID %in% spill_match_1650, 1,0)
data_final$spill_1700 <- ifelse(data_final$FID %in% spill_match_1700, 1,0)
data_final$spill_1750 <- ifelse(data_final$FID %in% spill_match_1750, 1,0)
data_final$spill_1800 <- ifelse(data_final$FID %in% spill_match_1800, 1,0)
data_final$spill_1850 <- ifelse(data_final$FID %in% spill_match_1850, 1,0)
data_final$spill_1900 <- ifelse(data_final$FID %in% spill_match_1900, 1,0)
data_final$spill_1950 <- ifelse(data_final$FID %in% spill_match_1950, 1,0)
data_final$spill_2000 <- ifelse(data_final$FID %in% spill_match_2000, 1,0)
data_final$spill_2050 <- ifelse(data_final$FID %in% spill_match_2050, 1,0)
data_final$spill_2100 <- ifelse(data_final$FID %in% spill_match_2100, 1,0)
data_final$spill_2150 <- ifelse(data_final$FID %in% spill_match_2150, 1,0)
data_final$spill_2200 <- ifelse(data_final$FID %in% spill_match_2200, 1,0)
data_final$spill_2250 <- ifelse(data_final$FID %in% spill_match_2250, 1,0)
data_final$spill_2300 <- ifelse(data_final$FID %in% spill_match_2300, 1,0)
data_final$spill_2350 <- ifelse(data_final$FID %in% spill_match_2350, 1,0)
data_final$spill_2400 <- ifelse(data_final$FID %in% spill_match_2400, 1,0)
data_final$spill_2450 <- ifelse(data_final$FID %in% spill_match_2450, 1,0)
data_final$spill_2500 <- ifelse(data_final$FID %in% spill_match_2500, 1,0)


View(data_final)
dim(data_final)


# Second model to test for spill overs while holding everything else constant 



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
         spill_2500)


all_dummies_spill <- as.data.frame(cbind(data_final_cleaning$FID,
                                         data_spillover_buffers$PRICE,
                                   dum_HF_BATHRM,
                                   #dum_HEAT, #not helpful
                                   dum_AC,
                                   #dum_AYB,
                                   #dum_QUALIFIED,
                                   # dum_SOURCE, #not helpful
                                   dum_ZIPCODE,
                                   dum_WARD,
                                   #dum_QUADRANT, #not helpful
                                   #dum_inBID,
                                   # dum_STYLE, #not helpful
                                   #dum_STRUCT, #not helpful
                                   dum_GRADE,
                                   dum_CNDTN,
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
                                   data_spillover_buffers$spill_2500)) ###
#dum_EXTWALL, #not helpful
#dum_ROOF,
# dum_INTWALL, #not helping
#dum_GIS_ID))


all_quantitative_spill <- as.data.frame(cbind(data_final_cleaning$FID,
                                       BATHRM_var,
                                       # data_final_cleaning$NUM_UNITS, #not helpful
                                       ROOMS_var, 
                                       BEDRM_var,
                                       #YR_RMDL,
                                       #EYB,
                                       STORIES_var,
                                       KITCHENS_var,
                                       FIREPLACES_var,
                                       squarefoot_var,
                                       # data_final_cleaning$LANDAREA, #not helpful
                                       #LONGITUDE,
                                       #LATITUDE,
                                       #CENSUS_BLO,
                                       #distance_var,
                                       sale_date_l_var))
                                       #inBID_var,
                                       #data_final_cleaning$years_since_des, #not helpful
                                       #(data_final_cleaning$inBID* t_minus_five_var), #V9
                                      # (data_final_cleaning$inBID*t_minus_two_var), #V10
                                       #(data_final_cleaning$inBID*t_at_zero_var), #V11
                                       #(data_final_cleaning$inBID*t_plus_five_var))) #V12



data_spill_model_withjoin <- full_join(all_dummies_spill,all_quantitative_spill)



#remove V1 used to join the dfs
data_spill_model <- subset(data_spill_model_withjoin, select = -(V1))

data_spill_model <- data.frame(data_spill_model)

spillover_model <- lm(log(data_spill_model$V2)~.,data_spill_model)

summary(spillover_model)
###