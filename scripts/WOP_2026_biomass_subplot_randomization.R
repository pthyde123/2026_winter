
#### NOT FINISHED ####





#this script assign subplot biomass sampling time points
#input -- the full B4I meta data
#output -- PM3D subplots and biomass sampling designation
#details --  each plot should be sampled once in every time point but the individual subplot should be randomly selected. 


library(tidyverse)
library(readr)

B4I_2025_T3_meta <- read_csv("data/B4I_2025_T3_meta.csv")  # this is the meta data for all locations, trials, plots and subplots for 2025

B4I_2025_PM3D_subplot_meta <- B4I_2025_T3_meta %>% 
  filter(studyName %in% c("B4I_2025_PM3D_IL", "B4I_2025_PM3D_NY", "B4I_2025_PM3D_ND",
                          "B4I_2025_PM3D_AL", "B4I_2025_PM3D_IA")) %>% # filter to only PM3D trials
  filter(observationLevel == "subplot") # filter to only subplot


runif(1)*10000
set.seed(9831) # set seed once


B4I_2025_PM3D_biomass_timepoints <- B4I_2025_PM3D_subplot_meta %>% # start with PM3D trials, only subplots
  
  mutate(subplotNumber = str_sub(observationUnitName,-1)) %>% # pull out subplot number (1,2,3) from observationUnitName
  
  mutate(rand = runif(300)) %>% # add a random number to each subplot
  
  arrange(locationName,plotNumber,rand) %>%   # arrange by random within location and plot
  
  mutate( biomass_timepoint = rep(c( 1,2,3), times = (100))) %>%  # assign sample biomass sample 1, 2, or 3
  
  arrange(locationName,plotNumber,subplotNumber) %>% # put them back in location, plot, subplot order
  
  mutate(plot.subplot = str_c(plotNumber,".",subplotNumber)) %>% 
  
  select(observationUnitName,observationUnitDbId,plot.subplot,biomass_timepoint) %>% # pull out the unique id's and time point information 
  
  mutate(biomass_T1 = if_else(biomass_timepoint == 1,1,NA)) %>% # set up management factor format
  mutate(biomass_T2 = if_else(biomass_timepoint == 2,1,NA)) %>%
  mutate(biomass_T3 = if_else(biomass_timepoint == 3,1,NA)) 


B4I_2025_PM3D_biomass_timepoints


#write.csv(B4I_2025_PM3D_biomass_timepoints, "data/2025_PM3D_biomass_timepoints.csv",row.names = F)


