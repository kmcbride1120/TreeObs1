### Tree Observatory Project Synchronizing Script ###
## American sycamore (Platanus occidentalis) Analysis at The Morton Arboretum ##

Data <- merge.data.frame(Nov2020, DT5, 
                         key = "Datetime", all.x = T, all.y = T)

# Can include code for 58/58/59 day offset after cleaning script is developed
