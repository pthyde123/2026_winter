
#this script assign subplot biomass sampling time points
#input -- randomization( to get plot categories not in T3), T3 subplot layout 
#output -- PM3D subplots and biomass sampling designation
#details --  each plot should be sampled once in every time point but the individual subplot should be randomly selected. 


library(tidyverse)
library(readr)

runif(1)*10000
set.seed(6392) # set seed once 6392

## Pull the intercrop meta data from the randomization (intercrop type, catagory etc.)
WOP_2026_meta <- read_csv("data/WOP_2026_randomization.csv")%>% 
  mutate(plot_number = str_pad(as.character(plotNo), width = 3, side = "left", pad = "0")) %>% 
  select(plot_number,intercrop,category,crop,crop_2)

## Join the meta with the T3 subplot layout
CU_ARS_2026_WOP_subplot_layout <- read_csv("data/CU_ARS_2026_WOP_subplot_layout.csv") %>% 
  left_join(WOP_2026_meta)

## Filter to only biomass subplots
WOP_2026_biomass_subplots <- CU_ARS_2026_WOP_subplot_layout %>% 
  filter(category == "Biomass")  
 


## Subplot, biomass randomization
WOP_2026_PM3D_biomass_timepoints <- WOP_2026_biomass_subplots %>% # start with PM3D biomass subplot
  
  mutate(rand = runif(72)) %>% # add a random number to each subplot
  
  arrange(plot_number,rand) %>%   # arrange by random within  plot
  
  mutate( biomass_timepoint = rep(c( 1,2,3), times = (24))) %>%  # assign sample biomass sample 1, 2, or 3, 24 total plot in this trial
  
  arrange(plot_number, subplot_number) %>% # put them back in plot, subplot order
  
  mutate(plot.subplot = str_c(plot_number,".",subplot_number)) %>% # plot.subplot format is useful for manual entry into PM3D
  
  select(plot_name,subplot_name,subplot_number, row_number, col_number, plot.subplot,crop_2,biomass_timepoint) %>% # pull out the unique id's and time point information 
  
  mutate(biomass_T1 = if_else(biomass_timepoint == 1,1,NA)) %>% # set up management factor format
  mutate(biomass_T2 = if_else(biomass_timepoint == 2,1,NA)) %>%
  mutate(biomass_T3 = if_else(biomass_timepoint == 3,1,NA)) 

## Take a look
WOP_2026_PM3D_biomass_timepoints

## Save as .csv
#write.csv(WOP_2026_PM3D_biomass_timepoints, "data/WOP_2026_PM3D_biomass_timepoints.csv",row.names = F)


