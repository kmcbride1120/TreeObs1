### Harmonic Cleaning Code ###
## American sycamore (Platanus occidentalis) Tweets at The Morton Arboretum ##
# This script provides outlier removal during the growing season for modeling.
  # This script is not a long-term solution. Please turn this into a function
    # when the data begins to get overwhelming or seek out alternative methods
    # of outlier removal. Currently requires updating every season.
  # Inputs: Newly downloaded TO data (into Cleaning, then Drift_Sync) -> SFTO
  # Outputs: TO data with outliers removed from the growing season for use in
    # harmonic forecasting

# Reads-in, cleans, and drift synchronizes TO data
# Modify these according to your system and what you need to work with
source('D:/Zelda/College/Research/Sap Flow/GitHub/Cleaning_MortonTO.R')
source('D:/Zelda/College/Research/Sap Flow/GitHub/Drift_Sync_MortonTO.R')
#View(SFTO)

# Removes pre-2018 growing season outliers
W2017 <- SFTO[SFTO$Date < "2018-05-12",]
#boxplot.stats(W2017$Cor_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -8.073 and upper whisker: 3.777
#boxplot.stats(W2017$Cor_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -4.912 and upper whisker: 3.969
#boxplot.stats(W2017$Unc_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -4.6820 and upper whisker: 2.1850
#boxplot.stats(W2017$Unc_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -2.661 and upper whisker: 2.478
W12017 <- W2017[W2017$Cor_Out > -8.073 & W2017$Cor_Out < 3.777,]
W22017 <- W12017[W12017$Cor_In > -4.912 & W12017$Cor_Out < 3.969,]
W32017 <- W22017[W22017$Unc_Out > -4.6820 & W22017$Cor_Out < 2.1850,]
W42017 <- W32017[W32017$Unc_In > -2.661 & W32017$Cor_Out < 2.478,]

# Removes 2018 growing season outliers
GSH2018 <- SFTO[SFTO$Date > "2018-05-11" & SFTO$Date < "2018-12-01",]
#boxplot.stats(GSH2018$Cor_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -8.3800 and upper whisker: 39.4950
#boxplot.stats(GSH2018$Cor_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -7.6740 and upper whisker: 31.1220
#boxplot.stats(GSH2018$Unc_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -4.849 and upper whisker: 22.851
#boxplot.stats(GSH2018$Unc_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -4.259 and upper whisker: 18.188
GSH12018 <- GSH2018[GSH2018$Cor_Out > -8.3800 & GSH2018$Cor_Out < 39.4950,]
GSH22018 <- GSH12018[GSH12018$Cor_In > -7.6740 & GSH12018$Cor_Out < 31.1220,]
GSH32018 <- GSH22018[GSH22018$Unc_Out > -4.849 & GSH22018$Cor_Out < 22.851,]
GSH42018 <- GSH32018[GSH32018$Unc_In > -4.259 & GSH32018$Cor_Out < 18.188,]

# Removes 2019 growing season outliers
GSH2019 <- SFTO[SFTO$Date > "2019-03-31" & SFTO$Date < "2019-12-01",]
#boxplot.stats(GSH2019$Cor_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -5.0840 and upper whisker: 13.8490
#boxplot.stats(GSH2019$Cor_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -8.5770 and upper whisker: 15.1040
#boxplot.stats(GSH2019$Unc_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -2.9420 and upper whisker: 8.0090
#boxplot.stats(GSH2019$Unc_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -4.781 and upper whisker: 8.919
GSH12019 <- GSH2019[GSH2019$Cor_Out > -5.0840 & GSH2019$Cor_Out < 13.8490,]
GSH22019 <- GSH12019[GSH12019$Cor_In > -8.5770 & GSH12019$Cor_Out < 15.1040,]
GSH32019 <- GSH22019[GSH22019$Unc_Out > -2.9420 & GSH22019$Cor_Out < 8.0090,]
GSH42019 <- GSH32019[GSH32019$Unc_In > -4.781 & GSH32019$Cor_Out < 8.919,]

GSH2020 <- SFTO[SFTO$Date > "2020-03-31" & SFTO$Date < "2020-12-01",]
#boxplot.stats(GSH2020$Cor_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -2.908 and upper whisker: 19.323
#boxplot.stats(GSH2020$Cor_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -3.175 and upper whisker: 17.554
#boxplot.stats(GSH2020$Unc_Out, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -1.683 and upper whisker: 11.179
#boxplot.stats(GSH2020$Unc_In, coef = 1.5, do.conf = TRUE, do.out = FALSE)
# Lower whisker: -1.656 and upper whisker: 10.338
GSH12020 <- GSH2020[GSH2020$Cor_Out > -2.908 & GSH2020$Cor_Out < 19.323,]
GSH22020 <- GSH12020[GSH12020$Cor_In > -3.175 & GSH12020$Cor_Out < 17.554,]
GSH32020 <- GSH22020[GSH22020$Unc_Out > -1.683 & GSH22020$Cor_Out < 11.179,]
GSH42020 <- GSH32020[GSH32020$Unc_In > -1.656 & GSH32020$Cor_Out < 10.338,]

# Combines dataframes into one document again
SFH2018 <- SFTO[SFTO$Date > "2018-11-30" & SFTO$Date < "2019-04-01",]
SFH2019 <- SFTO[SFTO$Date > "2019-11-30" & SFTO$Date < "2020-04-01",]
SFH2020 <- SFTO[SFTO$Date > "2020-11-30" & SFTO$Date < "2021-04-01",]
GSH2021 <- SFTO[SFTO$Date > "2021-03-31",]
SFTOH <- rbind(W42017, GSH42018, SFH2018, GSH42019, SFH2019, GSH42020, SFH2020, GSH2021)

# Clears workspace except for cleaned data
rm(list = setdiff(ls(), c("SFTOH", "SFTO")))