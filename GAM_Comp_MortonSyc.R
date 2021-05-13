### Generalized Additive Model Comparison Code ###
## American sycamore (Platanus occidentalis) Tweets at The Morton Arboretum ##
# This script will compare recent data to GAM-derived predictions. 
  # There are a number of ways to improve this method. We can turn precipitation into
    # a binary variable and incorporate it into the model, switch to using daily sums
    # or averages of environmental variables vs. daily sap flow sums in the model, or
    # use a simultaneous confidence interval instead of a point-wise CI.
  # See GAM_MortonSync for more information on the models
  # Inputs: current weather and sap flow data (post-cleaning script)
  # Outputs: identification of potentially unusal days for manual review

# Loads packages
library(mgcv)
library(scam)
library(tidyverse)
library(lubridate)
library(zoo)

# Imports cleaned TO data from with outliers removed
  # Make sure you're putting the right .csv into Cleaning_MortonTO
source('D:/Zelda/College/Research/Sap Flow/GitHub/Cleaning_Harmonic_MortonSyc.R')

## Data preparation
# Generates day of year column
SFTOH1 <- na.omit(SFTOH)
SFTOH1 <- transform(SFTOH1, DOY = lubridate::yday(Datetime))

# Creates one entry per day by summing up daily sap flow for Unc_Out
UOSum <- as.data.frame(aggregate(SFTOH1$Unc_Out, by=list(SFTOH1$Date), FUN=sum))
names(UOSum)[1] <- "Date"
names(UOSum)[2] <- "Unc_Out_Sum"

# Joins daily sum to previous data.frame
SFTOH2e <- merge(SFTOH1, UOSum, all.x = TRUE, all.y = TRUE)

# Import Morton Arboretum weather data
# This is weather data taken every thirty minutes for 2018-2019
# Contact Newton for more recent data?
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

# Merges weather data with sap flow data
SFW <- merge(SFTOH2e, Met2019c, all.x = TRUE, all.y = FALSE)
SFW <- na.omit(SFW)

## Model 1: Day of Year, Vapor Pressure Deficit, Solar Radiation, and Temperature
modelea <- gamm(Unc_Out1 ~ (DOY*VPDR*SRR*TempR), correlation = corCAR1(form = ~ 1 | DOY), random=list(DOY=~1), method = "REML", data = SFW)

# Generates prediction based on available data
  # *100 is based on the scale of response with DOY only
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

## Model 2: DOY, SR, and Temp
modeleb <- gamm(Unc_Out1 ~ (DOY*SRR*TempR), correlation = corCAR1(form = ~ 1 | DOY), random=list(DOY=~1), method = "REML", data = SFW)

# Generates prediction using available data (same scale as above)
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

## Comparing data to predictions
# Selects columns of interest
SFWComp <- select(SFW, Date, Unc_Out_Sum)

# Adds confidence intervals for models 1 and 2 to dataframe
SFWCompa <- cbind(SFWComp, Uea)
SFWCompb <- cbind(SFWCompa, Lea)
SFWCompc <- cbind(SFWCompb, Ueb)
SFWCompd <- cbind(SFWCompc, Leb)

# Limits to one observation per day
SFWComp1a <- unique(as.Date(SFWCompd$Date))
SFWComp1b <- unique(SFWComp$Unc_Out_Sum)

# Generates an average upper and lower bound for each day
SFWComp1c <- SFWCompd %>%
  group_by(Date) %>%
  summarize(Ueamean = mean(Uea))
SFWComp1d <- SFWCompd %>%
  group_by(Date) %>%
  summarize(Leamean = mean(Lea))
SFWComp1e <- SFWCompd %>%
  group_by(Date) %>%
  summarize(Uebmean = mean(Ueb))
SFWComp1f <- SFWCompd %>%
  group_by(Date) %>%
  summarize(Lebmean = mean(Leb))

# Combine dataframes
SFWComp2a <- cbind.data.frame(SFWComp1a, SFWComp1b)
names(SFWComp2a)[1] <- "Date"
names(SFWComp2a)[2] <- "Unc_Out_Sum"
SFWComp2b <- merge(SFWComp2a, SFWComp1c)
SFWComp2c <- merge(SFWComp2b, SFWComp1d)
SFWComp2d <- merge(SFWComp2c, SFWComp1e)
SFWComp2 <- merge(SFWComp2d, SFWComp1f)

# Model 1 (VPD, SR, Temp): compares daily sap flow sums to confidence interval
AComp <- as.data.frame(ifelse(SFWComp2$Unc_Out_Sum<SFWComp2$Ueamean&SFWComp2$Unc_Out_Sum>SFWComp2$Leamean, print("Within predictions."), print("Potentially unusual day.")))
names(AComp)[1] <- "Prediction"

# Generates data.frame with comparison output
ASFComp <- cbind(SFWComp2a, AComp)
View(ASFComp)

# Model 2 (SR, Temp): Compares daily sap flow sums to confidence interval
BComp <- as.data.frame(ifelse(SFWComp2$Unc_Out_Sum<SFWComp2$Uebmean&SFWComp2$Unc_Out_Sum>SFWComp2$Lebmean, print("Within predictions."), print("Potentially unusual day.")))
names(BComp)[1] <- "Prediction"

# Generates data.frame with comparison output
BSFComp <- cbind(SFWComp2a, BComp)
View(BSFComp)
