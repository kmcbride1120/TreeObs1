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
SFTO1aSW <- as.data.frame(SFTO1a)
UC <- SFTO1aSW$Unc_Out
Dt <- SFTO1aSW$Date

# Creates a harmonic regression within a sliding window (???)
Har <- function(x){as.numeric(as.character(harmonic.regression(as.numeric(UC), 1:length(Dt)), Tau = 144))}
SFHSW <- runner(x=UC, k="1 days", lag = 1, idx=Dt, f = Har)

# Visualizes output
View(SFHSW)
plot(SFHSW)