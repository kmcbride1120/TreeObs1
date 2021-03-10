### Tree Observatory Project Synchronizing Script ###
## American sycamore (Platanus occidentalis) Analysis at The Morton Arboretum ##
# This combines the Tree Observatory and forestry plot data into one dataframe. 
  # Inputs: SFTO and DT5 (see Readin for FP and Cleaning and Drift_Sync for TO.)
  # Outputs: Data, a dataframe that combines TO and FP data for analysis.

# Merges Tree Observatory and Forestry Plot data into one dataframe.
Data <- merge.data.frame(SFTO, DT5, 
                         key = "Datetime", all.x = T, all.y = T)

