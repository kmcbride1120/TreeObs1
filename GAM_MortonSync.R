### Generalized Additive Model Code ###
## American sycamore (Platanus occidentalis) Tweets at The Morton Arboretum ##
# This script will create a generalized additive model on a seasonal scale
  # for the TO data. A standard harmonic curve did not provide a good estimation
  # of the data, but may still be necessary in order to estimate the start, end,
  # and peak of the growing season. This model will provide closer estimations of
  # daily sap flow values, allowing for the detection of unusual events.
    # This script is modified from Simpson, G. 2018. Modelling 
    # Palaeoecological Time Series Using Generalised Additive Models. Frontiers 
    # in Ecology and Evolution 6. (Annotated code available in supplement.)
  # Inputs: Cleaned TO data (Cleaning -> DriftSync -> Cleaning_Harmonic)
  # Outputs: GAM model across the growing season(s)
  # Special thanks to Christy Rollinson and Lucien Fitzpatrick!
    # Overfitting? Ask yourself if there is a biological/ecological explanation!
      # Look at R^2 (with penalty)

# Loads packages
library(mgcv)
library(scam)
library(tidyverse)
library(lubridate)
library(zoo)
#library(gratia)

# Imports cleaned TO data from with outliers removed
source('D:/Zelda/College/Research/Sap Flow/GitHub/Cleaning_Harmonic_MortonSyc.R')
#View(SFTOH)

## Basic Seasonal GAMs - all data and daily sums
# Check for autocorrelation (will be highly autocorrelated); remove NAs
SFTOH1 <- na.omit(SFTOH)
acf(SFTOH1$Unc_Out1, lag.max= 10, plot = FALSE)

# Transform datetime to a numeric value for model fitting
  # Better options: strptime and Julian day
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

# Fit model with gamm() plus autocorrelation process, grouped by DOY (all data)
  # K chosen to be 12 (months in a year)
  # Random term added for multiple years
    # Simultaneously too much and not enough data - can miss long-term trends
# 2018 model
#model18 <- gamm(Unc_Out1 ~ s(DT_Num, bs = "cc", k = 12), correlation = corCAR1(form = ~ 1 | DOY), method = "REML", data = SFd18)
#plot(model18$gam, shade = TRUE)
# Same model with daily sums
model18a <- gamm(Unc_Out_Sum ~ s(DOY, bs = "cc", k = 12), correlation = corCAR1(form = ~ 1 | DOY), method = "REML", data = SFd18)
#plot(model18a$gam, shade = TRUE)

# Model with all years of data, no environmental information
#modelay <- gamm(Unc_Out_Sum ~ s(DOY, bs = "cc", k = 12), correlation = corCAR1(form = ~ 1 | DOY), random=list(DOY=~1), method = "REML", data = SFTOH2e)
# Check diagnositics and size of k using gam.check()
#gam.check(modelay$gam)
# Check if trend is significant using summary()
#summary(modelay$gam)

## GAMs + Environmental Data
# Import Morton Arboretum weather data
  # This is weather data taken every thirty minutes for 2018-2019
  # Contact Newton for more recent data? New data may need cleaning
Met2019c <- read_csv("D:/Zelda/College/Research/Sap Flow/Spreadsheets/Met2019c.csv", 
                     col_types = cols(Datetime = col_datetime(format = "%d/%m/%Y %H:%M")))

# Generates rolling averages for environmental GAM
  # 30 minute observations - 48 weather observations/day - 336/week
Met2019c$VPDR <- zoo::rollapply(data=zoo(Met2019c$VPD), width=336, mean, fill=NA, partial=TRUE, align="right")
Met2019c$TempR <- zoo::rollapply(data=zoo(Met2019c$Temp), width=336, mean, fill=NA, partial=TRUE, align="right")
Met2019c$SRR <- zoo::rollapply(data=zoo(Met2019c$SR), width=336, mean, fill=NA, partial=TRUE, align="right")

# Changes classes of rolling average columns to numeric
Met2019c$VPDR <- as.numeric(Met2019c$VPDR)
Met2019c$TempR <- as.numeric(Met2019c$TempR)
Met2019c$SRR <- as.numeric(Met2019c$SRR)
#sapply(Met2019c, class)

# Merges weather data with sap flow data from basic GAM (earlier in document)
SFW <- merge(SFTOH2e, Met2019c, all.x = TRUE, all.y = FALSE)
SFW <- na.omit(SFW)

# Environmental data GAMs
# Generates GAM with VPD, SR, and Temp rolling averages
modelea <- gamm(Unc_Out1 ~ (DOY*VPDR*SRR*TempR), correlation = corCAR1(form = ~ 1 | DOY), random=list(DOY=~1), method = "REML", data = SFW)
#plot(modelea$gam, shade = TRUE, all.terms = TRUE)
# Assesses GAM
  # Good model? Check residuals for homoskedacity using resid()
    # Residuals normally distributed? Predicted vs. observed linear?
    # Predicted vs residuals horizontal? Always do a full suite!
summary(modelea$gam)
MEAR <- residuals.gam(modelea$gam)
plot(MEAR)
gam.check(modelea$gam, k.rep=12)
# Looks at ANOVA outputs
  # Higher R^2 value than the 2-variable model
anova.gam(modelea$gam)

# Generates GAM with only Temp and SR rolling averages
modeleb <- gamm(Unc_Out1 ~ (DOY*SRR*TempR), correlation = corCAR1(form = ~ 1 | DOY), random=list(DOY=~1), method = "REML", data = SFW)
#plot(modeleb$gam, shade = TRUE, all.terms = TRUE)
# Assesses GAM
summary(modeleb$gam)
MEBR <- residuals.gam(modeleb$gam)
plot(MEBR)
gam.check(modeleb$gam, k.rep=12)
# Looks at ANOVA outputs
  # Perform variable reduction to get a good model
    # Sig with all three: TempR, DOY:TempR, TempR:SRR, DOY:TempR:SRR
anova.gam(modeleb$gam)

## Predictions
# 3-variable model
  # *100 is based on the scale of ME18aP
MEAP <- predict(modelea$gam, type="response", newdata=SFW, se.fit = TRUE)
Fitea <- as.data.frame(MEAP$fit*100)

# Forms point-wise confidence intervals
Uea <- MEAP$fit*100 + (2 * MEAP$se.fit*100)
Lea <- MEAP$fit*100 - (2 * MEAP$se.fit*100)

# Plots data
ggplot(SFW, aes(x=SFW$Datetime))+
  geom_line(data=Fitea, y=MEAP$fit*100, colour = "red")+
  geom_ribbon(aes(ymin = Lea, ymax = Uea), alpha = 0.2, fill = "red")+
  geom_point(aes(y=SFW$Unc_Out_Sum))+
  scale_x_datetime(xlab("Date"), 
                   date_breaks = "6 months",
                   date_minor_breaks = "1 month")+
  scale_y_continuous(ylab("Uncorrected Sap Flow Daily Sum"))

# 2-variable model
MEBP <- predict(modeleb$gam, newdata=SFW, type="response", se.fit = TRUE)
Fiteb <- as.data.frame(MEBP$fit*100)

# Forms point-wise confidence intervals
Ueb <- MEBP$fit*100 + (2 * MEBP$se.fit*100)
Leb <- MEBP$fit*100 - (2 * MEBP$se.fit*100)

# Plots data
ggplot(SFW, aes(x=SFW$Datetime))+
  geom_line(data=Fiteb, y=MEBP$fit*100, colour = "blue")+
  geom_ribbon(aes(ymin = Leb, ymax = Ueb), alpha = 0.2, fill = "blue")+
  geom_point(aes(y=SFW$Unc_Out_Sum))+
  scale_x_datetime(xlab("Date"), 
                   date_breaks = "6 months",
                   date_minor_breaks = "1 month")+
  scale_y_continuous(ylab("Uncorrected Sap Flow Daily Sum"))

# Sap flow 2018 only model
ME18aP <- predict(model18a$gam, type="response", newdata=SFW, se.fit = TRUE)
Fit18a <- as.data.frame(ME18aP$fit)

# Forms point-wise confidence intervals (can also try simultaneous)
U18a <- ME18aP$fit + (2 * ME18aP$se.fit)
L18a <- ME18aP$fit - (2 * ME18aP$se.fit)

# Plots data
ggplot(SFW, aes(x=SFW$Datetime))+
  geom_line(data=Fit18a, y=ME18aP$fit, colour = "chartreuse4")+
  geom_ribbon(aes(ymin = L18a, ymax = U18a), alpha = 0.2, fill = "chartreuse4")+
  geom_point(aes(y=SFW$Unc_Out_Sum))+
  scale_x_datetime(xlab("Date"), 
    date_breaks = "6 months",
    date_minor_breaks = "1 month")+
  scale_y_continuous(ylab("Uncorrected Sap Flow Daily Sum"))