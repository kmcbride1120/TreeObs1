### Basic Seasonal Harmonic Code ###
## American sycamore (Platanus occidentalis) Tweets at The Morton Arboretum ##
# This script will make a basic, seasonal-timescale harmonic to the TO data.
  # Inputs: Newly downloaded TO data (into Cleaning, then Drift_Sync) -> SFTO
    # This data may require additional cleaning to exclude growing season outliers
  # Outputs: daily and seasonal harmonics (in progress)

# Loads packages
library(tidyverse)
library(forecast)
library(lubridate)
library(HarmonicRegression)

# Reads-in, cleans, and drift synchronizes TO data
  # Modify this according to your system and what you need to work with
source('D:/Zelda/College/Research/Sap Flow/GitHub/Cleaning_MortonTO.R')
source('D:/Zelda/College/Research/Sap Flow/GitHub/Drift_Sync_MortonTO.R')
#View(SFTO1a)

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

#Seasonal subsetting
SFTO3 <- SFTO[SFTO$Datetime<"2019-03-22",]
SFTO3a <- select(SFTO3, Unc_Out1)
SFTO3a <- as.matrix.data.frame(SFTO3a)

# Harmonic regression (seasonal?)
SFHar3 <- harmonic.regression(as.numeric(SFTO3a), 1:length(SFTO3a), Tau = 52560)

# Takes fit values and generates a data frame with SFTO3
SFTO3$HarFit <- paste(SFHar3$fit.vals)
SFTO3$HarFit <- as.numeric(SFTO3$HarFit)
SFTO4 <- as.data.frame(SFTO3)
#View(SFTO2)

# Plots harmonic regression (seasonal)
ggplot(SFTO4, aes(x = Datetime))+
  geom_line(aes(y=HarFit, colour = "Harmonic"))+
  geom_line(aes(y=Unc_Out1, colour = "Sap Flow"))+
  scale_y_continuous(limits = c(-5, 20), 
                     breaks = seq(-5, 20, by = 5), 
                     ylab("Sap Flow Out (cm/hr)"))