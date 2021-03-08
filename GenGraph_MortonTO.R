### Tree Observatory Project General Graphing Script ###
## American sycamore (Platanus occidentalis) Analysis at The Morton Arboretum ##
# This script is mainly a starting place template for an analysis of tree
  # observatory and forestry plot data. Use it for data exploration.
  # Inputs: Data (from Readin, then Sync).
  # Outputs: One very bulky graph.

# Graphs all of the tree observatory and forestry plot data.
print(ggplot(Data, aes(x=Datetime))+ 
  geom_line(aes(y=Corrected_Out, colour="Corr_Out_TO"))+
  geom_line(aes(y=Corrected_In, colour="Corr_In_TO"))+
  geom_line(aes(y=Uncorrected_Out, colour="Un_Out_TO"))+
  geom_line(aes(y=Uncorrected_In, colour="Un_In_TO"))+
  geom_line(aes(y=Sap_Flow_Out, colour="SF_Out_TO"))+
  geom_line(aes(y=Sap_Flow_In, colour="SF_In_TO"))+
  geom_line(aes(y=Corrected_Out_T1, colour="Corr_Out_T1"))+
  geom_line(aes(y=Corrected_In_T1, colour="Corr_In_T1"))+
  geom_line(aes(y=Uncorrected_Out_T1, colour="Un_Out_T1"))+
  geom_line(aes(y=Uncorrected_In_T1, colour="Un_In_T1"))+
  geom_line(aes(y=Corrected_Out_T2, colour="Corr_Out_T2"))+
  geom_line(aes(y=Corrected_In_T2, colour="Corr_In_T2"))+
  geom_line(aes(y=Uncorrected_Out_T2, colour="Un_Out_T2"))+
  geom_line(aes(y=Uncorrected_In_T2, colour="Un_In_T2"))+
  geom_line(aes(y=Corrected_Out_T3, colour="Corr_Out_T3"))+
  geom_line(aes(y=Corrected_In_T3, colour="Corr_In_T3"))+
  geom_line(aes(y=Uncorrected_Out_T3, colour="Un_Out_T3"))+
  geom_line(aes(y=Uncorrected_In_T3, colour="Un_In_T3"))+
  geom_line(aes(y=Corrected_Out_T4, colour="Corr_Out_T4"))+
  geom_line(aes(y=Corrected_In_T4, colour="Corr_In_T4"))+
  geom_line(aes(y=Uncorrected_Out_T4, colour="Un_Out_T4"))+
  geom_line(aes(y=Uncorrected_In_T4, colour="Un_In_T4"))+
  geom_line(aes(y=Corrected_Out_T5, colour="Corr_Out_T5"))+
  geom_line(aes(y=Corrected_In_T5, colour="Corr_In_T5"))+
  geom_line(aes(y=Uncorrected_Out_T5, colour="Un_Out_T5"))+
  geom_line(aes(y=Uncorrected_In_T5, colour="Un_In_T5"))+
  geom_point(aes(y = Temp, colour = "Temp"))+
  geom_col(aes(y = Rain, colour = "Rain"))+
  geom_point(aes(y = VPD, colour = "VPD"))+
  geom_point(aes(y = SR, colour = "SR"))+
  scale_x_datetime(#limits = as_datetime(c('2019-05-01 00:00:00', 
                                          #'2019-11-31 23:59:59')), 
                   xlab("Date"), 
                   date_breaks = "1 month",
                   date_minor_breaks = "1 week")+
  scale_y_continuous(limits = c(-1, 100), 
                     breaks = seq(-1, 100, by = 5), 
                     ylab("Sap Flow Out (cm/hr)")))