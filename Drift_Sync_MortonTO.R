## Drift synchronization - To Be Added to Cleaning Script##

source('D:/Zelda/College/Research/Sap Flow/GitHub/Cleaning_MortonTO.R')
# Run Cleaning first!

Start58 <- as_datetime("2018-03-22 00:10:00") + hours(1404)
# Adds 58.5 days to the start date

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
# Adds 58.5 days until Sep 3, 2020

FlagDF <- as.data.frame(Flag)
FlagDF <- data.frame(lapply(FlagDF, as.character), stringsAsFactors=FALSE)
FlagList <- as.list(FlagDF$Flag)
FlagList1 <- str_sub(FlagList, 6, -5)
FlagList2 <- as.data.frame(FlagList1)
FlagList2$FlagList1 <- as_datetime(FlagList2$FlagList1)
FlagList3 <- as.list(FlagList2)
#lapply(FlagList3, class)
#View(FlagList2)
#View(SF7)
#SF7$Datetime <- as.character(SF7$Datetime)
# Trims Flag to something comparable to SF7's Datetime column


FlagCol <- SF7$Datetime %in% FlagList3
SF7$FlagCol <- FlagCol
# Only produces "FALSE", probably a date problem

SF7[, "FlagCola"] <- NA
FlagDateFun <- ifelse(FlagList3 %in% SF7$Datetime,"TRUE","FALSE")
SF8 <- as.data.frame(SF7 %>% mutate(FlagCola = FlagDateFun))
# Everything false again . . . 

View(SF8)

SF9 <- as_tibble(SF8)
LagFun <- lag()
SF9 %>% mutate(c("Unc_OutZ", "Unc_InZ", 
                 "Cor_OutZ", "Cor_InZ", "Sap_Flow_OutZ", "Sap_Flow_InZ"), 
               list(FlagCol, LagFun))
# Doesn't work yet