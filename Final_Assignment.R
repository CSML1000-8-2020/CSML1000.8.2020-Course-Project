# Load Libraries
library(knitr)
library(arules)
library(arulesViz)
library(dplyr)
library(data.table)
library(shinydashboard)
library(shiny)
library(ggplot2)
library(lubridate)
library(Hmisc)
library(funModeling) 
library(tidyverse)
library(survival)
library(broom)

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)


# Load in vacation dataset
raw = read.csv("./input/acme-travel-vacation.csv", header=TRUE, sep="\t", na.strings=c(""))
dataset <- raw
# Load in airport dataset
raw_airport = read.csv("./input/cities_IATA_long_lat.csv", header=TRUE)
dataset_airport <- raw_airport


# Remove the following columns
dataset$PACKAGE_ID <- NULL
dataset$ACCOM_SIZE_PAID <- NULL
dataset$ACCOM_LEVEL_PAID <- NULL
dataset$ACCOM_TYPE_RECEIVED <- NULL
dataset$ACCOM_SIZE_RECEIVED <- NULL
dataset$ACCOM_LEVEL_RECEIVED <- NULL
dataset$STATUS <- NULL
dataset$RATE_HEADER_ID <- NULL
dataset$RATE_CODE <- NULL
dataset$LIST_PRICE <- NULL
dataset$MARKET <- NULL
dataset$RATE_GRP <- NULL
dataset$CXL_DATE <- NULL
dataset$SEND_DATE <- NULL
dataset$BKG_ID <- NULL
dataset$AGENCY <- NULL
dataset$PACKAGE_TYPE_INDICATOR <- NULL
dataset$ACCOM_TYPE_PAID <- NULL
dataset$TRUE_ORIGIN <- NULL
dataset$MAIN_FLIGHT_ORIGIN <- NULL
#dataset$MAIN_FLIGHT_DESTINATION <- NULL
#dataset$MAIN_FLIGHT_DESTINATION <- NULL
dataset$INBOUND_FLIGHT_NUMBER <- NULL
dataset$INBOUND_FEEDER_FLIGHT_NUMBER <- NULL
dataset$INBOUND_TRAVEL_CLASS <- NULL
dataset$OUTBOUND_FLIGHT_NUMBER <- NULL
dataset$OUTBOUND_FEEDER_FLIGHT_NUMBER <- NULL
dataset$OUTBOUND_TRAVEL_CLASS <- NULL
dataset$BKG_TYPE <- NULL
dataset$SURCHARGES_AND_TAXES <- NULL
dataset$ANCILLARY_REVENUE <- NULL
#dataset$TOTAL_COST <- NULL
dataset$SOURCE <- NULL

# Make certain adjustments to the data in the following columns
dataset$START_DATE <- as.Date(as.character(dataset$START_DATE), format="%Y%m%d")
# BKG_DATE
dataset$BKG_DATE <- as.Date(as.character(dataset$BKG_DATE), format="%Y%m%d")
# ACCOMMODATION_STAR_RATING
dataset$ACCOMMODATION_STAR_RATING <- gsub("\\*", "", dataset$ACCOMMODATION_STAR_RATING)
dataset$ACCOMMODATION_STAR_RATING <- as.factor(dataset$ACCOMMODATION_STAR_RATING)

# TTE - Time between vacation start and booking date
dataset$TTE <- dataset$START_DATE - dataset$BKG_DATE
dataset$TTE <- as.numeric(dataset$TTE)
# Price 
dataset$Price_PerNight <- dataset$REVENUE / dataset$PARTY_SIZE / dataset$LENGTH_OF_STAY
# Cost_PerNight 
dataset$Cost_PerNight <- dataset$TOTAL_COST / dataset$PARTY_SIZE / dataset$LENGTH_OF_STAY
# Day of week of booking
dataset$Wday_BookingDate <- weekdays(as.Date(dataset$BKG_DATE))
dataset$Wday_BookingDate <- as.factor(dataset$Wday_BookingDate)
# Day of week of start of stay
dataset$Wday_StartDate <- weekdays(as.Date(dataset$START_DATE))
dataset$Wday_StartDate <- as.factor(dataset$Wday_StartDate)

#########################################################################################
#########################################################################################
All_2019 <- dataset[dataset$START_DATE < as.Date("2020-01-01") & 
                          dataset$START_DATE > as.Date("2018-12-31") #&
                          # dataset$DESTINATION=="CANCUN" &
                          # dataset$LENGTH_OF_STAY >= 1 &
                          # dataset$LENGTH_OF_STAY <= 14 &
                          # dataset$MARGIN > 0
                          ,]

All_2018 <- dataset[dataset$START_DATE < as.Date("2019-01-01") & 
                         dataset$START_DATE > as.Date("2017-12-31") #&
                         # #dataset$DESTINATION=="CANCUN" &
                         # dataset$LENGTH_OF_STAY >= 1 &
                         # dataset$LENGTH_OF_STAY <= 14 &
                         # dataset$MARGIN > 0
                       ,]

All_2017 <- dataset[dataset$START_DATE < as.Date("2018-01-01") & 
                         dataset$START_DATE > as.Date("2016-12-31") #&
                         # dataset$DESTINATION=="CANCUN" &
                         # dataset$LENGTH_OF_STAY >= 1 &
                         # dataset$LENGTH_OF_STAY <= 14 &
                         # dataset$MARGIN > 0
                       ,]

All_2016 <- dataset[dataset$START_DATE < as.Date("2017-01-01") & 
                         dataset$START_DATE > as.Date("2015-12-31") #&
                         # dataset$DESTINATION=="CANCUN" &
                         # dataset$LENGTH_OF_STAY >= 1 &
                         # dataset$LENGTH_OF_STAY <= 14 &
                         # dataset$MARGIN > 0
                       ,]

All_2015 <- dataset[dataset$START_DATE < as.Date("2016-01-01") & 
                         dataset$START_DATE > as.Date("2014-12-31") #&
                         # dataset$DESTINATION=="CANCUN" &
                         # dataset$LENGTH_OF_STAY >= 1 &
                         # dataset$LENGTH_OF_STAY <= 14 &
                         # dataset$MARGIN > 0
                       ,]

# Save the New Dataset
dataset_flt <- dataset
write.csv(dataset_flt,'dataset_flt.csv')
write.csv(All_2019,'Vacation_2019.csv')
write.csv(All_2018,'Vacation_2018.csv')
write.csv(All_2017,'Vacation_2017.csv')
write.csv(All_2016,'Vacation_2016.csv')
write.csv(All_2015,'Vacation_2015.csv')

# Graph Histogram of Popular Destinations per Booking
tmp <- dataset %>%
  group_by(DESTINATION) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
tmp <- head(tmp, n=10)
tmp
tmp %>%
  ggplot(aes(x=reorder(DESTINATION,n), y=n))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

# Graph Histogram of Popular Start Date of Bookings
tmp <- All_2019 %>%
  group_by(START_DATE) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
tmp <- head(tmp, n=10)
tmp
tmp %>%
  ggplot(aes(x=reorder(START_DATE,n), y=n))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

# Graph Histogram of Popular Destinations per Booking
tmp <- All_2019 %>%
  group_by(HOTEL_CJAIN_AFFILIATION) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
tmp <- head(tmp, n=10)
tmp
tmp %>%
  ggplot(aes(x=reorder(HOTEL_CJAIN_AFFILIATION,n), y=n))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()




# Commenting out 2015 & 2016 as not enough data
# km_2015 <- survfit(Surv(All_2015$TTE)~1)
# summary(km_2015)
# plot(km_2015,conf.int=FALSE, mark.time=TRUE,main="Kaplan-Meier estimate of the Pre Booking 2015", xlab="Pre Booking in Days", ylab="Estimated probability")
# 
# km_2016 <- survfit(Surv(All_2016$TTE)~1)
# summary(km_2016)
# plot(km_2016,conf.int=FALSE, mark.time=TRUE,main="Kaplan-Meier estimate of the Pre Booking 2016", xlab="Pre Booking in Days", ylab="Estimated probability")

km_2017 <- survfit(Surv(All_2017$TTE)~1)
summary(km_2017)
plot(km_2017,conf.int=FALSE, mark.time=TRUE,main="Kaplan-Meier estimate of the Pre Booking 2017", xlab="Pre Booking in Days", ylab="Estimated probability")

km_2018 <- survfit(Surv(All_2018$TTE)~1)
summary(km_2018)
plot(km_2018,conf.int=FALSE, mark.time=TRUE,main="Kaplan-Meier estimate of the Pre Booking 2018", xlab="Pre Booking in Days", ylab="Estimated probability")

km_2019 <- survfit(Surv(All_2019$TTE)~1)
summary(km_2019)
plot(km_2019,conf.int=FALSE, mark.time=TRUE,main="Kaplan-Meier estimate of the Pre Booking 2019", xlab="Pre Booking in Days", ylab="Estimated probability")




##########################################################################
##########################################################################
# Get the list of all the DESTINATION from the Raw data set
Name_of_Dest <- levels(dataset$DESTINATION)
Name_of_Dest
# Get the count of the DESTINATION from the Raw data set
Num_of_Dest <- length(Name_of_Dest)
Num_of_Dest
# Values for the TTE Dataframe
df_TTE <- NULL      # DataFrame
TTE_v <- NULL       # TTE Value of interest
dest_dest_v <- NULL # Destination to match in the airports.csv for map data
dest_airport_v <- NULL# Main Destination airport to match with file as Destination names may not match

for(i in 1:Num_of_Dest) {
  # Get Destination Label
  dest_dest <- Name_of_Dest[i]
  dest_dest
  Destination_num <- dataset[dataset$DESTINATION==dest_dest,]
  dest_airport <- as.character(Destination_num$MAIN_FLIGHT_DESTINATION[1])
  dest_airport
  # Count the number of rows in the data frame
  cnt <- nrow(Destination_num)
  # Perform the Kaplan-Meier estimate of the Pre Booking 
  km_curv <- survfit(Surv(Destination_num$TTE)~1)
  res <- summary(km_curv)
  # Create a data frame with only the Destination and TTE
  cols <- lapply(c(2:6), function(x) Destination_num[x])
  # Extract the columns you want
  cols <- lapply(c(2,6) , function(x) res[x])
  # Combine the columns into a data frame
  tbl <- do.call(data.frame, cols)
  # Extract values at % Required TODO: We could use this as a variable for more dynamic app?
  tbl_5pct <- tbl[tbl$surv < 0.05,]
  # Get the TTE as the first value in the subset
  TTE <- tbl_5pct$time[1]
  TTE_v <- c(TTE_v, TTE)
  dest_dest_v <- c(dest_dest_v, dest_dest)
  dest_airport_v <- c(dest_airport_v, dest_airport)
}
# dest_dest_v <- as.factor(dest_dest_v)
dest_airport_v <- as.character(dest_airport_v)
dest_dest_v <- as.character(dest_dest_v)
TTE_v <- as.character(TTE_v)
df_TTE <- data.frame("Destination" = dest_dest_v,"IATA" = dest_airport_v, "TTE" = TTE_v)
write.csv(df_TTE,'TTE_5pct.csv')

##########################################################################
##########################################################################
# Merge airport data for the dataset
# Airport File Raw
raw_airport = read.csv("./input/cities_IATA_long_lat.csv", header=TRUE)
df_IATA <- raw_airport
# Airport Code and ETA
TTE_aiport = read.csv("./input/TTE_5pct.csv", header=TRUE)
df_IATA_TTE <- TTE_aiport
# Merge two datasets
mydata <- merge(df_IATA,df_IATA_TTE,by="IATA")
mydata$X <- NULL
mydata$Destination <- NULL
write.csv(mydata,'IATA_TTE_5pct.csv')
##########################################################################
##########################################################################

# km_curv <- survfit(Surv(Destination_num$TTE)~1)
# km_res <- summary(km_curv)
# 
# str(Destination_num)
# 
# cox <- coxph(Surv(Destination_num$TTE)~1 + Destination_num$DESTINATION + 
#                Destination_num$PROPERTY_ID + 
#                Destination_num$PARTY_SIZE +
#                Destination_num$MAIN_FLIGHT_DESTINATION +
#                Destination_num$START_DATE +
#                Destination_num$LENGTH_OF_STAY #+
#                # Destination_num$BKG_DATE +
#                # Destination_num$REVENUE +
#                # Destination_num$TOTAL_COST +
#                # Destination_num$MARGIN +
#                # Destination_num$ACCOMMODATION_STAR_RATING +
#                # Destination_num$HOTEL_CJAIN_AFFILIATION + 
#                # Destination_num$Price_PerNight +
#                # Destination_num$Cost_PerNight
#                # 
#              )
# cox_res <- summary(cox)




##########################################################################
##########################################################################

ggplot(data = All_2019) +
  geom_bar(mapping = aes(x=START_DATE))

ggplot(data = All_2018) +
  geom_bar(mapping = aes(x=START_DATE))

ggplot(data = All_2017) +
  geom_bar(mapping = aes(x=START_DATE))

ggplot(data = All_2019) +
  geom_bar(mapping = aes(x=TTE))

ggplot(data = All_2018) +
  geom_bar(mapping = aes(x=TTE))

ggplot(data = All_2017) +
  geom_bar(mapping = aes(x=TTE))

ggplot(data = All_2019) +
  geom_point(mapping = aes(x=TTE, y=ACCOMMODATION_STAR_RATING)) 

f <- ggplot(All_2019, aes(TTE, ACCOMMODATION_STAR_RATING))
f + geom_jitter() 

ggplot(data = All_2018) +
  geom_point(mapping = aes(x=TTE, y=ACCOMMODATION_STAR_RATING))

ggplot(data = All_2017, mapping = aes(x = ACCOMMODATION_STAR_RATING, y = TTE)) + 
  geom_boxplot(mapping = aes(group = cut_width(ACCOMMODATION_STAR_RATING, 0.1)))

ggplot(data = All_2018, mapping = aes(x = ACCOMMODATION_STAR_RATING, y = Price_PerNight)) + 
  geom_boxplot(mapping = aes(group = cut_width(ACCOMMODATION_STAR_RATING, 0.1)))

ggplot(data = All_2019, mapping = aes(x = ACCOMMODATION_STAR_RATING, y = TTE)) + 
  geom_boxplot(mapping = aes(group = cut_width(ACCOMMODATION_STAR_RATING, 0.1)))


All_2018$ACCOMMODATION_STAR_RATING <- as.factor(All_2018$ACCOMMODATION_STAR_RATING) 
ggplot(data = All_2018) +
  geom_point(mapping = aes(x= TTE, y = Price_PerNight))
str(All_2018)

str(All_2019)
ggplot(data = All_2019) +
  geom_point(mapping = aes(x= TTE, y = Cost_PerNight, colour = ACCOMMODATION_STAR_RATING))


d <- ggplot(data=All_2019, aes(x=START_DATE, y=TTE,
                                  colour=ACCOMMODATION_STAR_RATING)) 
d + geom_point() + 
  geom_smooth(fill=NA, size=1.2)
