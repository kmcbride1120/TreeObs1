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

## Basic Seasonal GAMs - all data and daily sums
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

# Generates day of year column
SFTOH2d <- transform(SFTOH2c, DOY = lubridate::yday(Datetime))

# Creates one entry per day by summing up daily sap flow for Unc_Out
UOSum <- as.data.frame(aggregate(SFTOH2d$Unc_Out, by=list(SFTOH2d$Date), FUN=sum))
names(UOSum)[1] <- "Date"
names(UOSum)[2] <- "Unc_Out_Sum"

# Joins daily sum to previous data.frame
SFTOH2e <- merge(SFTOH2d, UOSum, all.x = TRUE, all.y = TRUE)

# Subsets data by year (gamm() limits the size of the dataset)
  # Expand this as more years of data are added or write a function
SFd18 <- SFTOH2e[SFTOH2e$Yr_Num=="2018",]
SFd19 <- SFTOH2e[SFTOH2e$Yr_Num=="2019",]
SFd20 <- SFTOH2e[SFTOH2e$Yr_Num=="2020",]
SFd21 <- SFTOH2e[SFTOH2e$Yr_Num=="2021",]

# Fit model with gamm() plus autocorrelation process, grouped by DOY (2018)
model18 <- gamm(Unc_Out1 ~ s(DT_Num, bs = "cc", k = 100), correlation = corCAR1(form = ~ 1 | DOY), method = "REML", data = SFd18)
plot(model18$gam, shade = TRUE)
# Same model with daily sums
model18a <- gamm(Unc_Out_Sum ~ s(DOY, bs = "cc", k = 10), correlation = corCAR1(form = ~ 1 | DOY), method = "REML", data = SFd18)
plot(model18a$gam, shade = TRUE)

# 2019 model
model19 <- gamm(Unc_Out1 ~ s(DT_Num, bs = "cc", k = 100), correlation = corCAR1(form = ~ 1 | DOY), method = "REML", data = SFd19)
plot(model19$gam, shade = TRUE)
# Same model with daily sums
model19a <- gamm(Unc_Out_Sum ~ s(DOY, bs = "cc", k = 10), correlation = corCAR1(form = ~ 1 | DOY), method = "REML", data = SFd19)
plot(model19a$gam, shade = TRUE)

# 2020 model
model20 <- gamm(Unc_Out1 ~ s(DT_Num, bs = "cc", k = 100), correlation = corCAR1(form = ~ 1 | DOY), method = "REML", data = SFd20)
plot(model20$gam, shade = TRUE)
  # Consider adjusting cleaning strength for this year - outlier (reinstallation)
# Same model with daily sums
model20a <- gamm(Unc_Out_Sum ~ s(DOY, bs = "cc", k = 10), correlation = corCAR1(form = ~ 1 | DOY), method = "REML", data = SFd20)
plot(model20a$gam, shade = TRUE)

# Check diagnositics and size of k using gam.check()
gam.check(model20$gam)

# Check if trend is significant using summary()
summary(model20$gam)

# Point-wise or simultaneous intervals using confint()
#confint(model20$gam)

## GAMs + Environmental Data
# Import Morton Arboretum weather data
  # This is weather data taken every thirty minutes for 2018-2019
  # Contact Newton for more recent data? New data may need cleaning
Met2019c <- read_csv("D:/Zelda/College/Research/Sap Flow/Spreadsheets/Met2019c.csv", 
                     col_types = cols(Datetime = col_datetime(format = "%d/%m/%Y %H:%M")))

# Merges weather data with sap flow data from basic GAM (earlier in document)
SFW <- merge(SFTOH2e, Met2019c, all.x = TRUE, all.y = FALSE)

# Subsets data by year
# Expand this as more years of data are added or write a function
SFW18 <- SFW[SFW$Yr_Num=="2018",]
SFW19 <- SFW[SFW$Yr_Num=="2019",]
SFW20 <- SFW[SFW$Yr_Num=="2020",]
SFW21 <- SFW[SFW$Yr_Num=="2021",]

# 2018 GAM with environmental variables and sap flow sums
model18b <- gamm(Unc_Out1 ~ s(DOY) + s(Temp)+ s(VPD) + s(SR), correlation = corCAR1(form = ~ 1 | DOY), method = "REML", data = SFW18)
plot(model18b$gam, shade = TRUE)
summary(model18b$gam)

## Predictions