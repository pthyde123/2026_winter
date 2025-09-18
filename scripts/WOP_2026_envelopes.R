library(tidyverse)
library(readr)
library(here)

WOP_2026_randomization <- read_csv(here::here("data","WOP_2026_randomization.csv"))

### making csv for use on Sorrels envelop printer
### file must must have these headers in this order 
###     source,plotNo,oatName,peaName
### arrange by -oatname so all plots for a given accession are together for seed separating from bulk bag
### arrange by reverse entry to when printed they end up sequental


WOP_2026_randomization %>% 
  mutate(peaName = intercrop_accession_name) %>% 
  mutate(oatName = accession_name) %>% 
  mutate(PlotNo = plotNo) %>% 
  mutate(source = str_c("entry ",entry)) %>% 
  arrange(-entry, -plotNo) %>% # reverse entry order for printer
  select(source,plotNo,oatName,peaName) 
  #write.csv( "data/WOP_2026_envelopes.csv", row.names = F)

