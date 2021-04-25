### Daily Harmonic Code ###
## American sycamore (Platanus occidentalis) Tweets at The Morton Arboretum ##
# This script will make a basic harmonic to the TO data. These standard harmonics
# will be useful for estimating if individual days are consistent with the 
# previous year's sap flow values.
# Inputs: Newly downloaded TO data (into Cleaning, then Drift_Sync) -> SFTO
  # Use SFTOH for outlier removal in the growing season (Cleaning_Harmonic)
# Outputs: daily harmonics along sliding windows for determining if new data 
  # is in line with expectations (in progress)

# Reads-in, cleans, and drift synchronizes TO data
# Modify this for your system
# SFTO: outlier removal only in the winter; SFTOH: outlier removal everywhere
source('D:/Zelda/College/Research/Sap Flow/GitHub/Cleaning_MortonTO.R')
source('D:/Zelda/College/Research/Sap Flow/GitHub/Drift_Sync_MortonTO.R')
source('D:/Zelda/College/Research/Sap Flow/GitHub/Cleaning_Harmonic_MortonSyc.R')
#View(SFTO)

# Loads packages
library(tidyverse)
library(forecast)
library(lubridate)
library(HarmonicRegression)
library(runner)

## Daily Harmonic ##
# Generates a matrix with just sap flow data (currently subsetted while I learn)
SFTO1a <- SFTO[SFTO$Datetime>"2018-07-01"&SFTO$Datetime<"2018-07-16",]
SFTO1 <- select(SFTO1a, Unc_Out1)
SFTO1 <- as.matrix.data.frame(SFTO1)

# Creates a harmonic regression (~24 hours)
SFHar <- harmonic.regression(as.numeric(SFTO1), 1:length(SFTO1), Tau = 144)
#View(SFHar$fit.vals)

# Takes fit values and generates a data frame with SFTO1a
SFTO1a$HarFit <- paste(SFHar$fit.vals)
SFTO1a$HarFit <- as.numeric(SFTO1a$HarFit)
SFTO2 <- as.data.frame(SFTO1a)
#View(SFTO2)

# Plots harmonic regression (daily)
ggplot(SFTO2, aes(x = Datetime))+
  geom_line(aes(y=SFTO2$HarFit, colour = "Harmonic"))+
  geom_line(aes(y=SFTO2$Unc_Out1, colour = "Sap Flow"))+
  scale_y_continuous(limits = c(-5, 20), 
                     breaks = seq(-5, 20, by = 5), 
                     ylab("Sap Flow Out (cm/hr)"))

## Sliding Window ##
# Generates variables
SFTOSW <- as.data.frame(SFTOH)
SFTOSW <- na.omit(SFTOSW)
SFTOSW1 <- SFTOSW[SFTOSW$Datetime>"2018-07-01"&SFTOSW$Datetime<"2018-08-01",]
UC <- SFTOSW1$Unc_Out
Dt <- SFTOSW1$Date

# Creates a harmonic regression within a sliding window
  # Doesn't generate the output I'm looking for
Har <- function(x){as.numeric(as.character(harmonic.regression(UC, 1:length(Dt), Tau = 144)))}
SFHSWR <- runner(x=UC, k="7 days", lag = 1, idx=Dt, f = Har)

# Visualizes output
View(SFHSWR)
plot(SFHSWR)

# Could I embed runner inside harmonic.regression to get the output I want?
# Generates variables
UniDt <- unique(as.Date(SFTOSW1$Date))
UniCt <- days(1)
Strt <- as.Date("2018-07-01", origin="1970-01-01")

# Iterative function (sliding window)
Runlen <- seq.Date(Strt + 3*UniCt, as.Date("2018-08-01", origin="1970-01-01"), 1)

# Runs harmonic regression
RegList <- lapply(Runlen, function(x){
  harmonic.regression(UC, 1:length(SFTOSW1$Date), Tau = 144)})

# Generates data frame
RegDF <- as.data.frame(RegList)

# Visualizes output
  # Fit.vals 0-15 for one month - knit together?
  # Compare to actual harmonic data? 
View(RegDF)
plot(RegDF$fit.vals)

# Try GAM instead?
# Auto arima?