### Tree Observatory Project Source Code/Central Hub ###
## American sycamore (Platanus occidentalis) Analysis at The Morton Arboretum ##

library(tidyverse)
library(lubridate)
library(suncalc)
library(MASS)
library(reshape2)
library(modeest)
library("fourierin")
library(scales)
library(ggfortify)
library(patchwork)
library(zoo)
# Loads packages

source('D:/Zelda/College/Research/Sap Flow/GitHub/Readin_MortonTO.R')
# Reads in Tree Observatory, forestry plot, and environmental data

#<In progress (readin currently imports manually cleaned TO data)>
# Cleans and zeroes Tree Observatory data

source('D:/Zelda/College/Research/Sap Flow/GitHub/Sync_MortonTO.R')
# Synchronizes forestry plot, Tree Observatory, and environmental data into Data

source('D:/Zelda/College/Research/Sap Flow/GitHub/GenGraph_MortonTO.R')
# Graphs ALL data (takes a bit of time)

# Tree comparisons

# Week/month level comparisons

# Hour/day level comparisons
