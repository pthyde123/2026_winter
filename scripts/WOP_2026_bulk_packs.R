
library(readr)
library(tidyverse)
library(readxl)
library(here)


randomization <- WOP_2026_randomization <- read_csv("data/WOP_2026_randomization.csv")

bulk_oat_packs <- 
randomization %>% 
  select(accession_name, seedlot_name, nPlots) %>% 
  mutate(g_oat_per_plot = 40.5) %>% 
  group_by(seedlot_name) %>% 
  summarize(g_oat_per_accession = round(sum(g_oat_per_plot),1),
            nPlots = n()) %>% # the grams of seed for each accession needed at each location. 
  ungroup() %>% 
  filter(seedlot_name != "BLAZE-Cornell_WinterOatPeaIntercrop_2024_Ithaca-bulk") %>% 
  mutate(entry = seq(1:93)) %>% 
  print(n=93) 
  

#write.csv(bulk_oat_packs, here::here("data","WOP_2026_bulk_oat_packs_demo.csv")) 

