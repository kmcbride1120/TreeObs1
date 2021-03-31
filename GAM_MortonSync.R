### Generalized Additive Model (Daily) Code ###
## American sycamore (Platanus occidentalis) Tweets at The Morton Arboretum ##
# This script will create a generalized additive model (on a daily scale?) 
  # for the TO data. A standard harmonic curve did not provide a good estimation
  # of the data, but may still be necessary in order to estimate the start, end,
  # and peak of the growing season. This model will provide closer estimations of
  # daily sap flow values, allowing for the detection of unusual events.
    # This script is modified from Simpson, G. 2018. Modelling 
    # Palaeoecological Time Series Using Generalised Additive Models. Frontiers 
    # in Ecology and Evolution 6. (Annotated code available in supplement.)
  # Inputs: Cleaned TO data (Cleaning -> DriftSync -> Cleaning_Harmonic)
  # Outputs: GAM model across the growing season(s)

# Loads packages
library(mgcv)
library(scam)
library(tidyverse)
library(lubridate)
#library(gratia)

# Imports cleaned TO data from with outliers removed
source('D:/Zelda/College/Research/Sap Flow/GitHub/Cleaning_Harmonic_MortonSyc.R')
#View(SFTOH)

# Check for autocorrelation (will be highly autocorrelated); remove NAs
SFTOH1 <- na.omit(SFTOH)
acf(SFTOH1$Unc_Out1, lag.max= 10, plot = FALSE)

# Transform datetime to a numeric value for model fitting
SFTOH2 <- transform(SFTOH1, DT_Num = lubridate::year(Datetime)*10000 + 
                      lubridate::month(Datetime)*100 + 
                      lubridate::day(Datetime)) 
SFTOH2a <- transform(SFTOH2, DT_Num = DT_Num*10000 + 
                       lubridate::hour(Datetime)*100 + 
                       lubridate::minute(Datetime))

# Creates a variable with the year and one with the month of each observation
SFTOH2b <- transform(SFTOH2a, Yr_Num = lubridate::year(Datetime))
SFTOH2c <- transform(SFTOH2b, Mon_Num = lubridate::month(Datetime))

# Subsets data by year (gamm() limits the size of the dataset)
  # Expand this as more years of data are added or write a function
SFc18 <- SFTOH2c[SFTOH2c$Yr_Num=="2018",]
SFc19 <- SFTOH2c[SFTOH2c$Yr_Num=="2019",]
SFc20 <- SFTOH2c[SFTOH2c$Yr_Num=="2020",]
SFc21 <- SFTOH2c[SFTOH2c$Yr_Num=="2021",]

# Fit model with gamm() plus autocorrelation process
model1 <- gam(Unc_Out1 ~ s(DT_Num, k = 150), data = SFTOH2a, method = "REML")
#model2 <- gamm(Unc_Out1 ~ s(DT_Num, bs = "cc", k = 12), correlation = corCAR1(), method = "REML", data = SFc19m)
#plot(model2$gam)
  # Data may be too big for gamm() -> grouping by day?
  # Memory limitation issues
    # Data is autocorrelated, but can we make a gam() work just for predictions?

# Visualize model
plot(model1, shade = TRUE)

# Check diagnositics and size of k using gam.check()
gam.check(model1)

# Check if trend is significant using summary()
summary(model1)

# Point-wise or simultaneous intervals using confint()
#confint(model1)

# Periods of change using fderiv()
#fderiv(model1)

# Predictions
#predict(model1)
