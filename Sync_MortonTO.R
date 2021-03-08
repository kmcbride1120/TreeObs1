### Tree Observatory Project Synchronizing Script ###
## American sycamore (Platanus occidentalis) Analysis at The Morton Arboretum ##
# This combines the Tree Observatory and forestry plot data into one dataframe. 
  # Inputs: Nov2020 and DT5 (see Readin_MortonTO for more information).
    # Inputs will be modified after the cleaning file works properly.
  # Outputs: Data, a dataframe that combines TO and FP data for analysis.

# Merges Tree Observatory and Forestry Plot data into one dataframe.
Data <- merge.data.frame(Nov2020, DT5, 
                         key = "Datetime", all.x = T, all.y = T)

