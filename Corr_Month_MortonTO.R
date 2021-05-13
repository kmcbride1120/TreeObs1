### Tree Observatory Project Correlations Code ###
## American sycamore (Platanus occidentalis) Analysis at The Morton Arboretum ##


DT5May <- DT5[DT5$Date>"2019-04-30" & DT5$Date<"2019-06-01",]
DT5June <- DT5[DT5$Date>"2019-05-31" & DT5$Date<"2019-07-01",]
DT5July <- DT5[DT5$Date>"2019-06-30" & DT5$Date<"2019-08-01",]
DT5Aug <- DT5[DT5$Date>"2019-07-31" & DT5$Date<"2019-09-01",]
DT5Sep <- DT5[DT5$Date>"2019-08-31" & DT5$Date<"2019-10-01",]
DT5Oct <- DT5[DT5$Date>"2019-09-30" & DT5$Date<"2019-11-01",]
# Subsets data by month using forestry plot data through 2019

SR1 <- DT5$SR
VPD1 <- DT5$VPD
T1 <- DT5$Temp
R1 <- DT5$Rain
SFO1 <- DT5$Corr_Out_Ave
SFI1 <- DT5$Corr_In_Ave
# Subsets variables

#X <- c(SR1, VPD1, T1, R1, SFO1, SFI1)
#Y <- gsub("DT5", "May", X)
# I want to make a function that pastes "May" (or "June" or whatever)
  # in the correct location for me to subset data properly and ggplot2 it
  # but I think I'll just make a big list of variables instead 
  # because it doesn't matter

ggplot(DT5May, aes(x = SR1, y = SFO1))+ 
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + I(x), size = 1) +
  ylab("Average Sap Velocity Out (cm/hr)") +
  xlab("[Variable]") +
  theme(
    axis.text=element_text(size=10),
    axis.title=element_text(size=13,face="bold"))  
# Graphs correlation

SF <- lm(y~x)
summary(SF)
# See p-value and R^2

#xa <- x^2
#SFa <- lm(y~x + xa)
#summary(SFa)