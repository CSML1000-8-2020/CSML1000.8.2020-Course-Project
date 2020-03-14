# Load Libraries
# library(knitr)
# library(arules)
# library(arulesViz)
library('dplyr')
library('data.table')
# library(shinydashboard)
# library(shiny)
library(ggplot2)
library(lubridate)
library(Hmisc)
library(funModeling) 
library(tidyverse)
library(survival)
library(DataExplorer)
library(glue)

# Load files needed for selecting products
#aisles = read.csv("./input/aisles.csv")
raw = read.csv("./input/acme-travel-vacation.csv", header=TRUE, sep="\t", na.strings=c(""))
dataset <- raw

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
dataset$MAIN_FLIGHT_DESTINATION <- NULL
dataset$MAIN_FLIGHT_DESTINATION <- NULL
dataset$INBOUND_FLIGHT_NUMBER <- NULL
dataset$INBOUND_FEEDER_FLIGHT_NUMBER <- NULL
dataset$INBOUND_TRAVEL_CLASS <- NULL
dataset$OUTBOUND_FLIGHT_NUMBER <- NULL
dataset$OUTBOUND_FEEDER_FLIGHT_NUMBER <- NULL
dataset$OUTBOUND_TRAVEL_CLASS <- NULL
dataset$BKG_TYPE <- NULL
dataset$SURCHARGES_AND_TAXES <- NULL
dataset$ANCILLARY_REVENUE <- NULL
dataset$TOTAL_COST <- NULL
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
summary(dataset$Price_PerNight)
# Day of week of booking
dataset$Wday_BookingDate <- weekdays(as.Date(dataset$BKG_DATE))
dataset$Wday_BookingDate <- as.factor(dataset$Wday_BookingDate)
# Day of week of start of stay
dataset$Wday_StartDate <- weekdays(as.Date(dataset$START_DATE))
dataset$Wday_StartDate <- as.factor(dataset$Wday_StartDate)

head(dataset,25)
names(dataset)
str(dataset)
tail(dataset)

#########################################################################################
#########################################################################################
CANCUN_2019 <- dataset[dataset$BKG_DATE <= as.Date("2020-01-01") & 
                          dataset$BKG_DATE >= as.Date("2018-12-31") &
                          dataset$DESTINATION=="CANCUN" &
                          dataset$LENGTH_OF_STAY >= 1 &
                          dataset$LENGTH_OF_STAY <= 14 &
                          dataset$MARGIN > 0
                          ,]

CANCUN_2018 <- dataset[dataset$BKG_DATE <= as.Date("2019-01-01") & 
                         dataset$BKG_DATE >= as.Date("2017-12-31") &
                         dataset$DESTINATION=="CANCUN" &
                         dataset$LENGTH_OF_STAY >= 1 &
                         dataset$LENGTH_OF_STAY <= 14 &
                         dataset$MARGIN > 0
                       ,]

CANCUN_2017 <- dataset[dataset$BKG_DATE <= as.Date("2018-01-01") & 
                         dataset$BKG_DATE >= as.Date("2016-12-31") &
                         dataset$DESTINATION=="CANCUN" &
                         dataset$LENGTH_OF_STAY >= 1 &
                         dataset$LENGTH_OF_STAY <= 14 &
                         dataset$MARGIN > 0
                       ,]

CANCUN_2016 <- dataset[dataset$BKG_DATE <= as.Date("2017-01-01") & 
                         dataset$BKG_DATE >= as.Date("2015-12-31") &
                         dataset$DESTINATION=="CANCUN" &
                         dataset$LENGTH_OF_STAY >= 1 &
                         dataset$LENGTH_OF_STAY <= 14 &
                         dataset$MARGIN > 0
                       ,]

CANCUN_2015 <- dataset[dataset$BKG_DATE <= as.Date("2016-01-01") & 
                         dataset$BKG_DATE >= as.Date("2014-12-31") &
                         dataset$DESTINATION=="CANCUN" &
                         dataset$LENGTH_OF_STAY >= 1 &
                         dataset$LENGTH_OF_STAY <= 14 &
                         dataset$MARGIN > 0
                       ,]

# Save the New Dataset
dataset_flt <- dataset
write.csv(dataset_flt,'dataset_flt.csv')
write.csv(CANCUN_2019,'CANCUN_2019.csv')
write.csv(CANCUN_2018,'CANCUN_2018.csv')
write.csv(CANCUN_2017,'CANCUN_2017.csv')
write.csv(CANCUN_2016,'CANCUN_2016.csv')
write.csv(CANCUN_2015,'CANCUN_2015.csv')

# Graph Histogram of Popular Destinations per Booking
tmp <- dataset %>%
  group_by(DESTINATION) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp
tmp %>%
  ggplot(aes(x=reorder(DESTINATION,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()


# EDA - Complete Dataset
glimpse1 <- glimpse(dataset)
df_status1 <- df_status(dataset)
freq1 <- freq(dataset)
profiling_num1 <- profiling_num(dataset)
plot1 <- plot_num(dataset)
describe1 <- describe(dataset)

# EDA - 2015
glimpse_2015 <- glimpse(CANCUN_2015)
df_status_2015 <- df_status(CANCUN_2015)
freq_2015 <- freq(CANCUN_2015)
profiling_num_2015 <- profiling_num(CANCUN_2015)
plot_2015 <- plot_num(CANCUN_2015)
describe_2015 <- describe(CANCUN_2015)

# EDA - 2016
glimpse_2016 <- glimpse(CANCUN_2016)
df_status_2016 <- df_status(CANCUN_2016)
freq_2016 <- freq(CANCUN_2016)
profiling_num_2016 <- profiling_num(CANCUN_2016)
plot_2016 <- plot_num(CANCUN_2016)
describe_2016 <- describe(CANCUN_2016)

# EDA - 2017
glimpse_2017 <- glimpse(CANCUN_2017)
df_status_2017 <- df_status(CANCUN_2017)
freq_2017 <- freq(CANCUN_2017)
profiling_num_2017 <- profiling_num(CANCUN_2017)
plot_2017 <- plot_num(CANCUN_2017)
describe_2017 <- describe(CANCUN_2017)

# EDA - 2018
glimpse_2018 <- glimpse(CANCUN_2018)
df_status_2018 <- df_status(CANCUN_2018)
freq_2018 <- freq(CANCUN_2018)
profiling_num_2018 <- profiling_num(CANCUN_2018)
plot_2018 <- plot_num(CANCUN_2018)
describe_2018 <- describe(CANCUN_2018)

# EDA - 2019
glimpse_2019 <- glimpse(CANCUN_2019)
df_status_2019 <- df_status(CANCUN_2019)
freq_2019 <- freq(CANCUN_2019)
profiling_num_2019 <- profiling_num(CANCUN_2019)
plot_2019 <- plot_num(CANCUN_2019)
describe_2019 <- describe(CANCUN_2019)

# Plotting interesting features
f <- ggplot(CANCUN_2019, aes(Price_PerNight, ACCOMMODATION_STAR_RATING))
f + geom_jitter() +
  coord_cartesian(xlim = c(-10,2500))

d <- ggplot(data=CANCUN_2019, aes(x=START_DATE, y=TTE,
                          colour=ACCOMMODATION_STAR_RATING)) 
d + geom_point() + 
  geom_smooth(fill=NA, size=1.2)


km_2015 <- survfit(Surv(CANCUN_2015$TTE)~1)
summary(km_2015)
plot(km_2015,conf.int=FALSE, mark.time=TRUE,main="Kaplan-Meier estimate of the Pre Booking 2015", xlab="Pre Booking in Days", ylab="Estimated probability")

km_2016 <- survfit(Surv(CANCUN_2016$TTE)~1)
summary(km_2016)
plot(km_2016,conf.int=FALSE, mark.time=TRUE,main="Kaplan-Meier estimate of the Pre Booking 2016", xlab="Pre Booking in Days", ylab="Estimated probability")

km_2017 <- survfit(Surv(CANCUN_2017$TTE)~1)
summary(km_2017)
plot(km_2017,conf.int=FALSE, mark.time=TRUE,main="Kaplan-Meier estimate of the Pre Booking 2017", xlab="Pre Booking in Days", ylab="Estimated probability")

km_2018 <- survfit(Surv(CANCUN_2018$TTE)~1)
summary(km_2018)
plot(km_2018,conf.int=FALSE, mark.time=TRUE,main="Kaplan-Meier estimate of the Pre Booking 2018", xlab="Pre Booking in Days", ylab="Estimated probability")

km_2019 <- survfit(Surv(CANCUN_2019$TTE)~1)
summary(km_2019)
plot(km_2019,conf.int=FALSE, mark.time=TRUE,main="Kaplan-Meier estimate of the Pre Booking 2019", xlab="Pre Booking in Days", ylab="Estimated probability")


d <- ggplot(data=CANCUN_2018, aes(x=TTE, y=MARGIN,
                                   colour=ACCOMMODATION_STAR_RATING)) 
d + geom_point() + 
  geom_smooth(fill=NA, size=1.2)

raw %>% filter(LENGTH_OF_STAY >= 1 & LENGTH_OF_STAY < 14)
# Profiling the data input
head(raw)
str(raw)


basic_eda <- function(data)
{
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}

basic_eda(raw)
plot_str(raw)
plot_intro(raw)
plot_missing(raw)
plot_bar(raw)
plot_histogram(raw)
plot_qq(raw, sampled_rows = 1000L)
plot_correlation(na.omit(raw), maxcat = 10L)
plot_boxplot(raw, by = "START_DATE")
