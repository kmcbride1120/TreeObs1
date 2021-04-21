### Seasonal Harmonic Code ###
## American sycamore (Platanus occidentalis) Tweets at The Morton Arboretum ##
# This script will make a basic harmonic to the TO data. These standard harmonics
  # will be useful for estimating the start, end, and peak of the growing season,
  # but do not seem to accurately estimate the overall shape of the data.
  # Inputs: Newly downloaded TO data (into Cleaning, then Drift_Sync) -> SFTO
    # Use SFTOH for outlier removal in the growing season (Cleaning_Harmonic)
  # Outputs: basic seasonal harmonics for estimation of season start and end

# Issues: 1) Missing data, 2) Poor fit
  # Outlier removal within the growing season?
  # Interpolation? (na.interp() from forecast, na.approx() from zoo)

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

## Seasonal Harmonic ##
#Subsets for March 21, 2018-March 21, 2019
SFTO3 <- SFTO[SFTO$Datetime<"2019-03-22",]
SFTO3a <- select(SFTO3, Unc_Out1)
SFTO3a <- as.matrix.data.frame(SFTO3a)

# Seasonal harmonic regression (52560 = yearly observations if no missing data)
SFHar3 <- harmonic.regression(as.numeric(SFTO3a), 1:length(SFTO3a), Tau = 52560)

# Takes fit values and generates a data frame with SFTO3
SFTO3$HarFit <- paste(SFHar3$fit.vals)
SFTO3$HarFit <- as.numeric(SFTO3$HarFit)
SFTO4 <- as.data.frame(SFTO3)
#View(SFTO2)

# Plots seasonal harmonic regression
ggplot(SFTO4, aes(x = Datetime))+
  geom_line(aes(y=HarFit, colour = "Harmonic"))+
  geom_line(aes(y=Unc_Out1, colour = "Sap Flow"))+
  scale_y_continuous(limits = c(-5, 25), 
                     breaks = seq(-5, 25, by = 5), 
                     ylab("Sap Flow Out (cm/hr)"))
