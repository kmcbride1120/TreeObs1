### Tree Observatory Project Source Code/Central Hub ###
## American sycamore (Platanus occidentalis) Analysis at The Morton Arboretum ##
# This code is meant to act as a central hub for other scripts associated with
  # the project. See individual scripts for a list of in/outputs.

# Clear workspace
rm(list = ls())

# Loads packages
library(tidyverse)
library(lubridate)

# Reads in Tree Observatory, forestry plot, and environmental data
source('D:/Zelda/College/Research/Sap Flow/GitHub/Readin_MortonTO.R')

# Cleans and zeroes Tree Observatory data
#<In progress (readin currently imports manually cleaned TO data)>

# Synchronizes forestry plot, Tree Observatory, and environmental data into Data
source('D:/Zelda/College/Research/Sap Flow/GitHub/Sync_MortonTO.R')

# Graphs ALL data (takes a bit of time and isn't terribly useful)
source('D:/Zelda/College/Research/Sap Flow/GitHub/GenGraph_MortonTO.R')

