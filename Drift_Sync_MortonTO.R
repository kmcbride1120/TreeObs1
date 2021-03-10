## Drift synchronization - To Be Added to Cleaning Script##
  # There is a slight time drift on tree observatory data, which is fixed here
  # Inputs: SF7 - from freshly downloaded tree observatory data
  # Outputs: SFTO, containing corrected tree observatory data ready to analyze
    # Rationale: Drift vs. forestry plot: 1hr20min offset - 4800 seconds
    # March 21, 2018 - July 2, 2019 = 468 days
    # 4800 sec/468 days = 10.25 secs/day (.1709 min/day, .002849 hrs/day)
    # 10/.1709 = 58.51 (advances 1 10 min break every 58.5 days)
    # September 3rd, 2020 -> should be synchronized

# Run the cleaning code first! This should output SF7
source('D:/Zelda/College/Research/Sap Flow/GitHub/Cleaning_MortonTO.R')

# Adds 58.5 days to the start date
Start58 <- as_datetime("2018-03-22 00:10:00") + hours(1404)

# Adds 58.5 days until Sep 3, 2020
counter <- 1
Start58
Flag <- capture.output(repeat{
  x <- counter
  print(as_datetime(Start58 + hours(1404*x)))
  counter <- counter + 1
  if (counter > 14){
    break
  }
})

# Trims Flag to something comparable to SF7's Datetime column
FlagDF <- as.data.frame(Flag)
FlagDF <- data.frame(lapply(FlagDF, as.character), stringsAsFactors=FALSE)
FlagList <- as.list(FlagDF$Flag)
FlagList1 <- str_sub(FlagList, 6, -5)
FlagList1a <- as_datetime(FlagList1)

# FlagCol is TRUE wherever the data needs to be lagged
FlagCol <- SF7$Datetime %in% FlagList1a
SF7$FlagCol <- FlagCol

# Generates a cumulative sum of TRUEs in a new column
SF8 <- as_tibble(SF7)
SF8a <- SF8 %>%
  mutate(CountCol1 = cumsum(FlagCol == "TRUE"))

# Use subset to divide up data based on cumulative sum of TRUES
# Note: this method can CERTAINLY be simplified and will need to be adjusted 
# if the range of synchronization increases
SF90 <- SF8a[SF8a$CountCol1==0,]
SF91 <- SF8a[SF8a$CountCol1==1,]
SF92 <- SF8a[SF8a$CountCol1==2,]
SF93 <- SF8a[SF8a$CountCol1==3,]
SF94 <- SF8a[SF8a$CountCol1==4,]
SF95 <- SF8a[SF8a$CountCol1==5,]
SF96 <- SF8a[SF8a$CountCol1==6,]
SF97 <- SF8a[SF8a$CountCol1==7,]
SF98 <- SF8a[SF8a$CountCol1==8,]
SF99 <- SF8a[SF8a$CountCol1==9,]
SF910 <- SF8a[SF8a$CountCol1==10,]
SF911 <- SF8a[SF8a$CountCol1==11,]
SF912 <- SF8a[SF8a$CountCol1==12,]

# Use lag() on sap flow data based on cumulative sum of TRUEs
  # This method results in a large amount of missing data at each lag point
    # If I have time, I'll try to get a better method to work
SF90a <- SF90 %>% 
  mutate(Cor_Out1 = Cor_OutZ)
SF90b <- SF90a %>% 
  mutate(Cor_In1 = Cor_InZ)
SF90c <- SF90b %>%
  mutate(Unc_Out1 = Unc_OutZ)
SF90d <- SF90c %>%
  mutate(Unc_In1 = Unc_InZ)
SF90e <- SF90d %>%
  mutate(Sap_Flow_Out1 = Sap_Flow_OutZ)
SF90f <- SF90e %>%
  mutate(Sap_Flow_In1 = Sap_Flow_InZ)
#View(SF90f)
SF91a <- SF91 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 1))
SF91b <- SF91a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 1))
SF91c <- SF91b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 1))
SF91d <- SF91c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 1))
SF91e <- SF91d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 1))
SF91f <- SF91e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 1))
#View(SF91f)
SF92a <- SF92 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 2))
SF92b <- SF92a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 2))
SF92c <- SF92b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 2))
SF92d <- SF92c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 2))
SF92e <- SF92d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 2))
SF92f <- SF92e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 2))
#View(SF92f)
SF93a <- SF93 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 3))
SF93b <- SF93a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 3))
SF93c <- SF93b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 3))
SF93d <- SF93c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 3))
SF93e <- SF93d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 3))
SF93f <- SF93e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 3))
#View(SF93f)
SF94a <- SF94 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 4))
SF94b <- SF94a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 4))
SF94c <- SF94b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 4))
SF94d <- SF94c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 4))
SF94e <- SF94d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 4))
SF94f <- SF94e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 4))
#View(SF94f)
SF95a <- SF95 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 5))
SF95b <- SF95a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 5))
SF95c <- SF95b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 5))
SF95d <- SF95c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 5))
SF95e <- SF95d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 5))
SF95f <- SF95e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 5))
#View(SF95f)
SF96a <- SF96 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 6))
SF96b <- SF96a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 6))
SF96c <- SF96b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 6))
SF96d <- SF96c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 6))
SF96e <- SF96d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 6))
SF96f <- SF96e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 6))
#View(SF96f)
SF97a <- SF97 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 7))
SF97b <- SF97a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 7))
SF97c <- SF97b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 7))
SF97d <- SF97c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 7))
SF97e <- SF97d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 7))
SF97f <- SF97e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 7))
#View(SF97f)
SF98a <- SF98 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 8))
SF98b <- SF98a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 8))
SF98c <- SF98b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 8))
SF98d <- SF98c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 8))
SF98e <- SF98d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 8))
SF98f <- SF98e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 8))
#View(SF98f)
SF99a <- SF99 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 9))
SF99b <- SF99a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 9))
SF99c <- SF99b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 9))
SF99d <- SF99c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 9))
SF99e <- SF99d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 9))
SF99f <- SF99e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 9))
#View(SF99f)
SF910a <- SF910 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 10))
SF910b <- SF910a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 10))
SF910c <- SF910b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 10))
SF910d <- SF910c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 10))
SF910e <- SF910d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 10))
SF910f <- SF910e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 10))
#View(SF910f)
SF911a <- SF911 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 11))
SF911b <- SF911a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 11))
SF911c <- SF911b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 11))
SF911d <- SF911c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 11))
SF911e <- SF911d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 11))
SF911f <- SF911e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 11))
#View(SF911f)
SF912a <- SF912 %>% 
  mutate(Cor_Out1 = lag(Cor_OutZ, 12))
SF912b <- SF912a %>% 
  mutate(Cor_In1 = lag(Cor_InZ, 12))
SF912c <- SF912b %>%
  mutate(Unc_Out1 = lag(Unc_OutZ, 12))
SF912d <- SF912c %>%
  mutate(Unc_In1 = lag(Unc_InZ, 12))
SF912e <- SF912d %>%
  mutate(Sap_Flow_Out1 = lag(Sap_Flow_OutZ, 12))
SF912f <- SF912e %>%
  mutate(Sap_Flow_In1 = lag(Sap_Flow_InZ, 12))
#View(SF912f)

# Combine subsetted data
SF10 <- rbind(SF90f, SF91f, SF92f, SF93f, SF94f, SF95f, SF96f, 
              SF97f, SF98f, SF99f, SF910f, SF911f, SF912f)
#View(SF10)

# Select for relevant columns only
SF10 <- as.data.frame(SF10)
SFTO <- select(SF10, Date, Time, Datetime, Unc_Out1, Unc_In1, 
               Cor_Out1, Cor_In1, Sap_Flow_Out1, Sap_Flow_In1)
#View(SFTO)

# Cleanup line: clears workspace except for SF7 - proceed to Drift_Sync
rm(list = setdiff(ls(), "SFTO"))
