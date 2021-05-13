# TreeObs1
Repository for work connected to the Tree Observatory Program and tweet automation at The Morton Arboretum

To compare recent data to GAM-generated predictions:
1) Download most recent weather and sap flow data
2) Run sap flow data through Cleaning -> Drift Sync -> Cleaning Harmonic
3) Run GAM Comp
4) Look at the dataframe output for the dates of interest - should be "Within predictions." or "Potentially unusual day."
