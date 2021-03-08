### Tree Observatory Project Cleaning/Zeroing/Drift Synchronizing Code ###
## American sycamore (Platanus occidentalis) Analysis at The Morton Arboretum ##

# Loads packages
library(tidyverse)
library(lubridate)

## Read-in ##
# Reads in TO data (modify, assumes downloaded from Google Drive -> .csv)
SF_Raw <- read_csv("D:/Zelda/College/Research/Sap Flow/GitHub/RawSF_MortonTO.CSV", 
    skip = 17)
#View(SF_Raw)

# Creates a data frame for the raw data and renames relevant columns
data.frame(SF_Raw)
colnames(SF_Raw)
names(SF_Raw)[3] <- "Unc_Out"
names(SF_Raw)[4] <- "Unc_In"
names(SF_Raw)[5] <- "Cor_Out"
names(SF_Raw)[6] <- "Cor_In"
names(SF_Raw)[7] <- "Sap_Flow_Out"
names(SF_Raw)[8] <- "Sap_Flow_In"

# Assigns variable types for each column
SF_Raw$Date <- as.Date(SF_Raw$Date, format = "%d/%m/%Y")
SF_Raw$Time <- as_hms(SF_Raw$Time)
SF_Raw$Unc_Out <- as.numeric(SF_Raw$Unc_Out)
SF_Raw$Unc_In <- as.numeric(SF_Raw$Unc_In)
SF_Raw$Cor_Out <- as.numeric(SF_Raw$Cor_Out)
SF_Raw$Cor_In <- as.numeric(SF_Raw$Cor_In)
SF_Raw$Sap_Flow_Out <- as.numeric(SF_Raw$Sap_Flow_Out)
SF_Raw$Sap_Flow_In <- as.numeric(SF_Raw$Sap_Flow_In)
#sapply(SF_Raw, class)

# Subsets to relevant columns only
SF1 <- select(SF_Raw, Date, Time, Unc_Out, Unc_In, 
              Cor_Out, Cor_In, Sap_Flow_Out, Sap_Flow_In)
#View(SF1)

# Create datetime column using date and time data
SF1$Datetime <- paste(SF1$Date, SF1$Time)
SF1$Datetime <- as_datetime(SF1$Datetime)

## Error/outlier removal ##
# Removes fragments in the middle of the data (induced to NAs by class change)
SF2 <- na.omit(SF1)

# Removes wintertime error outputs
SF3 <- SF2[SF2$Cor_Out != -22.220,]
SF4 <- SF3[SF3$Cor_In != -22.220,]
SF5 <- SF4[SF4$Cor_Out < 100,]

# Removes 2018 winter outliers
  # Note: this will need to be turned into a function eventually
SF52018 <- SF5[SF5$Date > "2018-11-30" & SF5$Date < "2019-04-01",]
#boxplot.stats(SF52018$Cor_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -5.8500 and upper whisker: 1.7090
#boxplot.stats(SF52018$Cor_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -9.3500 and upper whisker: 1.7080
#boxplot.stats(SF52018$Unc_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -4.385 and upper whisker: -0.011
#boxplot.stats(SF52018$Unc_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -6.4100 and upper whisker: -0.0110
W12018 <- SF52018[SF52018$Cor_Out > -5.800 & SF52018$Cor_Out < 1.7090,]
W22018 <- W12018[W12018$Cor_In > -9.3500 & W12018$Cor_Out < 1.7080,]
W32018 <- W22018[W22018$Unc_Out > -4.385 & W22018$Cor_Out < -0.011,]
W42018 <- W32018[W32018$Unc_In > -6.4100 & W32018$Cor_Out < -0.0110,]

# Removes 2019 winter outliers
SF52019 <- SF5[SF5$Date > "2019-11-30" & SF5$Date < "2020-04-01",]
#boxplot.stats(SF52019$Cor_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -3.116 and upper whisker: 1.801
#boxplot.stats(SF52019$Cor_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -5.060 and upper whisker: -0.985
#boxplot.stats(SF52019$Unc_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -2.803 and upper whisker: 0.042
#boxplot.stats(SF52019$Unc_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -3.929 and upper whisker: -1.570
W12019 <- SF52019[SF52019$Cor_Out > -3.116 & SF52019$Cor_Out < 1.801,]
W22019 <- W12019[W12019$Cor_In > -5.060 & W12019$Cor_Out < -0.985,]
W32019 <- W22019[W22019$Unc_Out > -2.803 & W22019$Cor_Out < 0.042,]
W42019 <- W32019[W32019$Unc_In > -3.929 & W32019$Cor_Out < -1.570,]

# Combines dataframes into one document again
  # You will need to repeat this process at the end of the 2020-2021 winter
  # Note: winter is from December 1st - March 31st for outlier correction
GS2018 <- SF5[SF5$Date < "2018-12-01",]
GS2019 <- SF5[SF5$Date > "2019-03-31" & SF5$Date < "2019-12-01",]
GS2020 <- SF5[SF5$Date > "2020-03-31",]
SF6 <- rbind(GS2018, W42018, GS2019, W42019, GS2020)

## Zeroing ##
#SFZero <- SF6[SF6$Date > "2019-03-31" & SF6$Date < "2019-05-01",]
#summary(SFZero)
  # Cor_Out mean: -1.335, Cor_In: -3.249, Unc_Out: -1.772, Unc_In: -3.061
  # Sap_Flow_Out: -0.01286, Sap_Flow_In: -0.03513

# Zeroes data using April 2019 without outliers
SF6$Cor_OutZ <- SF6$Cor_Out+1.335
SF6$Cor_InZ <- SF6$Cor_In+3.249
SF6$Unc_OutZ <- SF6$Unc_Out+1.772
SF6$Unc_InZ <- SF6$Unc_In+3.061
SF6$Sap_Flow_OutZ <- SF6$Sap_Flow_Out+0.01286
SF6$Sap_Flow_InZ <- SF6$Sap_Flow_In+0.03513

# Trims data down to only zeroed data
SF7 <- select(SF6, Date, Time, Datetime, Unc_OutZ, Unc_InZ, 
              Cor_OutZ, Cor_InZ, Sap_Flow_OutZ, Sap_Flow_InZ)
#View(SF7)

## Drift synchronization ##
# Drift vs. forestry plot: 1hr20min offset - 4800 seconds
# March 21, 2018 - July 2, 2019 = 468 days
# 4800 sec/468 days = 10.25 secs/day (.1709 min/day, .002849 hrs/day)
# 10/.1709 = 58.51 (advances 1 10 min break every 58.5 days)
# September 3rd, 2020 -> should be synchronized

# Adds 58.5 days to the start date
Start58 <- as_datetime("2018-03-22 00:10:00") + hours(1404)

# Adds 58.5 days until Sep 3, 2020
counter <- 1
Start58
Flag <- capture.output(repeat{
  x <- counter
  print(as_datetime(Start58 + hours(1404*x)))
  counter <- counter + 1
  if (counter > 14){
    break
  }
})

# Trims Flag to something comparable to SF7's Datetime column
FlagDF <- as.data.frame(Flag)
FlagDF <- data.frame(lapply(FlagDF, as.character), stringsAsFactors=FALSE)
FlagList <- as.list(FlagDF$Flag)
FlagList1 <- str_sub(FlagList, 6, -5)
FlagList1a <- as_datetime(FlagList1)

# FlagCol is TRUE wherever the data needs to be lagged
FlagCol <- SF7$Datetime %in% FlagList1a
SF7$FlagCol <- FlagCol

# In progress: lag() sap flow data whenever FlagCol is TRUE
# In progress: select for relevant columns only