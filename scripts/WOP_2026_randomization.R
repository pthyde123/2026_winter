library(AlgDesign)
library(tidyverse)
library(readxl)
library(here)

ss <- floor(runif(1, 10000, 1000000))
set.seed(591742) 

# Import selected genotypes
accession_plan <- read_excel(here::here("data","WOP_2026_planning.xlsx"),  
                       sheet = "accession_plan") 

# Format dataframe 

planning <- accession_plan %>% 
  select(accession_name,intercrop_accession_name,nReps,nPlots,category,paired,seedlot_name) 


print(planning,n=nrow(planning))


# 8 Blocks
# Each block will have 50 plots

nBlks <- 8   #8 blocks
nPlotsPerBlk <- 27   #27 accessions, pairing comes in later 


plots <- planning %>% 
  slice(rep(1:n(), times = planning$nReps)) 
 

blkOP <- AlgDesign::optBlock(frml=as.formula(~accession_name),
                             withinData=plots,
                             blocksizes=rep(nPlotsPerBlk, nBlks))


# Assemble the design
winterOatDesign<-tibble(accession_name=character())  # create empty dataframe  

for (blockNum in 1:nBlks){
  thisBlock <- as_tibble(blkOP$Blocks[[blockNum]]) %>%
    dplyr::arrange(sample(nrow(blkOP$Blocks[[blockNum]]))) %>%
    dplyr::mutate(block=paste0("Block", blockNum)) %>%
    dplyr::relocate(block, .before=accession_name)
  winterOatDesign <- dplyr::bind_rows(winterOatDesign, thisBlock)
}


winterOatDesign


# Some quick checks
monoPea <- dplyr::filter(winterOatDesign, accession_name=="NO_OAT_PLANTED")
table(monoPea$block)

biomass <- dplyr::filter(winterOatDesign, category=="Monocrop_pea")

table(biomass$block)

hullless <- dplyr::filter(winterOatDesign, category=="Hull-less")

table(hullless$block)


# Make it a planting plan  # this is getting realllyyy convoluted over the years try and clean it up before next use. 
#Look at what the output is and figure out a better way to get here.  

winterOatPea_2026_2 <- winterOatDesign %>% 
  mutate("paired_2" = if_else(paired==0,"single","pair")) %>% #identify paired plots or unpaired plots based on assignment in plan (0=unpaired, 1= paired)
  mutate(pairNo = seq(1:nrow(winterOatDesign))) %>% # create a sequence of pair designations
  mutate(pairs = if_else(paired==0,1,2)) %>% # identify how many plots are in each pair, either 1 or 2
  group_by(pairNo)

df <- winterOatPea_2026_2 %>% 
  expand(count = seq(1:pairs)) ## create a dataframe doubling the pairs = 2  

winterOatPea_2026 <- winterOatPea_2026_2 %>% 
  left_join(df) %>% 
  ungroup() ## join the df with the original 

# Randomly assign if the first or second plot in the pair is the monocrop or intercrop

winterOatPea_2026_3 <-  winterOatPea_2026 %>% 
  mutate("SN" = seq(1:nrow(winterOatPea_2026))) %>% 
  group_by(SN) %>% 
  mutate("random" = runif(1)) %>% # randomly assign a number for each plot
  ungroup() %>% 
  
  mutate("intercrop" = if_else(count==1,"intercrop","monocrop")) %>% 
  mutate(intercrop = if_else(intercrop_accession_name=="NO_PEA_PLANTED","monocrop",intercrop)) %>% #sorting out biomass experiment
  mutate(intercrop = if_else(accession_name=="BLAZE_biomass","monocrop",intercrop)) %>%   #sorting out biomass experiment
  
  arrange(pairNo,random) %>%  # arrange within a pair if the intercrop plot is first or second using the random number.
  mutate("plotNo" = seq(1:nrow(winterOatPea_2026))) %>%  ## add the plot numbers, sequentially, the mono-inter was randomize in the previous line 
  select(plotNo,pairNo,nPlots,block,accession_name,intercrop,category,seedlot_name) %>% 
  
  # the rest is just getting meta data designation and formatting it better for t3 trial upload 
  mutate(accession_name=dplyr::if_else(accession_name == "BLAZE_mono", "NO_OAT_PLANTED", accession_name)) %>% 
  mutate(accession_name=dplyr::if_else(accession_name == "BLAZE_biomass", "NO_OAT_PLANTED", accession_name)) %>% 
  
  mutate(crop=dplyr::if_else(grepl("NO_OAT_PLANTED", accession_name), "Pea", "Oat")) %>% 

  mutate(intercrop = if_else(crop=="Pea","monocrop",intercrop)) %>%
  mutate("crop_2" = if_else(crop=="Oat" & intercrop == "monocrop","oat",
                            if_else(crop =="Oat" & intercrop == "monocrop", "oat",
                                    if_else(crop == "Pea", "pea", 
                                            if_else(crop=="Oat" & intercrop == "intercrop","oat-pea","na"))))) %>% ### add oat, pea, oat-pea designation 
  
  mutate(intercrop_accession_name = if_else(crop_2 == "oat-pea", "BLAZE", NA)) %>% 
  mutate(intercrop_accession_name = if_else(accession_name == "NO_OAT_PLANTED", "BLAZE", intercrop_accession_name)) %>% 
  mutate(intercrop_accession_name = if_else(intercrop_accession_name == "BLAZE" & category == "Keystone_Pea_Test", "KEYSTONE", intercrop_accession_name)) %>% 
  
  mutate(intercrop_accession_name = if_else(is.na(intercrop_accession_name), "NO_PEA_PLANTED",intercrop_accession_name))


#### create entry number, should have done this first, very useful for seed packing to have an easy reference not the accession name
#### got ahead of myself and created this ENRTYno. for the bulk packs that is why I used the seedlot name to sort.  In the future use accession_name.


winterOatPea_2026_3 %>% 
  select(accession_name,seedlot_name) %>% 
  arrange(seedlot_name) %>% 
  distinct() %>% 
  filter(accession_name != "NO_OAT_PLANTED") %>% 
  mutate(entry = seq(1:93)) %>% 
  select(accession_name,entry) %>% 
  write.csv(here::here("data","WOP_2026_entry_demo.csv"))


winterOatPea_2026_4 <- winterOatPea_2026_3 %>% 
  left_join(read.csv(here::here("data","WOP_2026_entry_demo.csv")), join_by(accession_name))


  
# Take a look
print(winterOatPea_2026_4, n= nrow(winterOatPea_2026_4)) 


#  Save it
write.csv(winterOatPea_2026_4, here::here("data","WOP_2026_randomization_demo.csv"))


