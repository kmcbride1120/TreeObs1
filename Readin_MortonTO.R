### Tree Observatory Project Read-in Script ###
## American sycamore (Platanus occidentalis) Analysis at The Morton Arboretum ##
# This script reads in data. You will need to alter it for your circumstances.
  # The TO data read in here stretches from March 21, 2018 to November 30, 2019.
  # Known winter error codes, such as -22.220 and 500, were manually removed.
  # The data was zeroed using the average values from Nov 27-Dec 1, 2019
    # where sap flow should theoretically be 0.
      # This TO file should be considered as an alternate to the updated
        # TO data from Cleaning and Drift_Sync
  # A small amount of date drift was incurred due to the lack of a hub. The data
    # was manually delayed by one window every 58, then 59 days in order to
    # allow for accurate comparison to the forestry plot data.
  # Contact Newton Tran for information about forestry plot data cleaning.


# Loads Tree Observatory data through November 30, 2020
  # This has been manually zeroed, cleaned, and corrected for drift in Excel
library(readr)
Nov2020 <- read_csv("D:/Zelda/College/Research/Sap Flow/Spreadsheets/Nov2020.csv", 
         col_types = cols(Corrected_In = col_number(), 
             Corrected_Out = col_number(), Date = col_date(format = "%d/%m/%Y"), 
             Datetime = col_datetime(format = "%d/%m/%Y %H:%M"), 
             Sap_Flow_In = col_number(), Sap_Flow_Out = col_number(), 
             Time = col_time(format = "%H:%M:%S"), 
             Uncorrected_In = col_number(), Uncorrected_Out = col_number()))
#View(Nov2020)

# Loads forestry plot and environmental data through 2019
library(readr)
DT5 <- read_csv("D:/Zelda/College/Research/Sap Flow/Spreadsheets/DT5.csv", 
     col_types = cols(Corr_In_Ave = col_number(), 
         Corr_Out_Ave = col_number(), Corrected_In_T1 = col_number(), 
         Corrected_In_T2 = col_number(), Corrected_In_T3 = col_number(), 
         Corrected_In_T4 = col_number(), Corrected_In_T5 = col_number(), 
         Corrected_Out_T1 = col_number(), Corrected_Out_T2 = col_number(), 
         Corrected_Out_T3 = col_number(), Corrected_Out_T4 = col_number(), 
         Corrected_Out_T5 = col_number(), Date = col_date(format = "%d/%m/%Y"), 
         Datetime = col_datetime(format = "%d/%m/%Y %H:%M"), 
         Rain = col_number(), SR = col_number(), 
         Temp = col_number(), Time = col_time(format = "%H:%M:%S"), 
         Uncorrected_In_T1 = col_number(), Uncorrected_In_T2 = col_number(), 
         Uncorrected_In_T3 = col_number(), Uncorrected_In_T4 = col_number(), 
         Uncorrected_In_T5 = col_number(), Uncorrected_Out_T1 = col_number(), 
         Uncorrected_Out_T2 = col_number(), Uncorrected_Out_T3 = col_number(), 
         Uncorrected_Out_T4 = col_number(), Uncorrected_Out_T5 = col_number(), 
         VPD = col_number()))
#View(DT5)
