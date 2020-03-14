install.packages("lubridate")
install.packages("glue")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("lattice")
install.packages("DataExplorer")
library(lubridate)
library(funModeling)
library(lattice)
library(DataExplorer)
library(tidyverse)
library(glue)

dataset_raw = read.csv("acme-travel-vacation.csv", header=TRUE, sep="\t", na.strings=c(""))
raw = dataset_raw %>% select('BKG_ID', 'PROPERTY_ID', 'HOTEL_CJAIN_AFFILIATION', 'BKG_DATE', 'DESTINATION', 'PARTY_SIZE', 'START_DATE', 'LENGTH_OF_STAY', 'REVENUE', 'MARGIN','ACCOMMODATION_STAR_RATING')

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
