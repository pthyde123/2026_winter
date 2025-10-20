

### Code for taking randomization to T3/oat and field plan


library(tidyverse)
library(readxl)
library(readr)


## the randomization  # replace here

randomization <- WOP_2026_randomization_old <- read_csv("data/WOP_2026_randomization_old.csv")


##  the map # replace here 
map <- CU_2025_Ithaca_WOP_PLOT_layout <- read_excel("data/WOP_2026_planning.xlsx", 
                                                    sheet = "plot_layout")


#### ENTER FIELD META DATA HERE ####

# trail name convention mother-ship_year_location(short)_trial abbreviation_type(PLOT or HR) 

trial_name = "CU_ARS_2026_WOP"
breeding_program = "Intercropping Cooperative"
location = "Ithaca, NY - Helfer"
year = "2026"
design_type = "p-rep"
description =  "2026 winter oat pea intercrop trial located in Ithaca"
trial_type = "phenotyping_trial"
plot_width = "1.3"
plot_length = ""
field_size = ""     #this will be calculated and added in
planting_date =  "2025-09-30"   ## set as text in the future so format stays in .csv for upload
harvest_date  = ""




###T3 trial headers 


t3_trial_upload_headers <- c("trial_name",	"breeding_program",	"location",	"year",	"design_type",	"description",	"trial_type",	
                             "plot_width", "plot_length",	"field_size",	"planting_date",	"harvest_date",	"plot_name",	"accession_name",
                             "plot_number",
                             "block_number",	"is_a_control",	"rep_number",	"range_number",	"row_number",	"col_number",
                             "seedlot_name",	"num_seed_per_plot",	"weight_gram_seed_per_plot",	"entry_number",	"is_private")



### Field Meta Calculations  ###


#Two methods for calculating total number of plots make sure they match!

length(randomization$plotNo) # how many plots are there?
max(randomization$plotNo) # how many plots are there?


if_else(length(randomization$plotNo) == max(randomization$plotNo), "ALL GOOD" , "DANGER")


total_plots  <- length(randomization$plotNo)

total_plots




# row_number, col_number and field size in hectares

randomization_row_col  <-  map %>% 
  pivot_longer(!row, names_to = "column", values_to = "plot") %>% 
  mutate(row_number = row) %>% 
  mutate(col_number = as.numeric(column)) %>% 
  mutate(plot_number = plot) %>% 
  select(plot_number,	row_number,col_number) %>% 
  filter(!is.na(plot_number)) %>% 
  arrange(plot_number)

row_number <- randomization_row_col$row_number
col_number <- randomization_row_col$col_number


field_size = ((max(randomization_row_col$row_number)) * 5.5) * 
  ((max(randomization_row_col$col_number)) * 1.4 ) / 10000





### Create data frame of trial level meta


trial_level_headers <- c("trial_name",	"breeding_program",	"location",	"year",	"design_type",	"description",	"trial_type",	
                         "plot_width", "plot_length",	"field_size",	"planting_date",	"harvest_date")




trial_meta <- bind_cols(trial_name,  #keep in this order, to match t3 template
                        breeding_program,
                        location,
                        year,
                        design_type,
                        description,
                        trial_type,
                        plot_width,
                        plot_length,
                        field_size,
                        planting_date,
                        harvest_date)

colnames(trial_meta) <- trial_level_headers

trial_meta <- trial_meta  %>%  slice(rep(1:n(), each = total_plots)) # duplicate the meta to match the number of plots

trial_meta



### plot level data

plot_level_headers <- c("plot_name",	"accession_name", "plot_number",
                        "block_number",	"is_a_control",	"rep_number",	"range_number",	"row_number",
                        "col_number", "seedlot_name",	"num_seed_per_plot",
                        "weight_gram_seed_per_plot",	"entry_number",	"is_private")

randomization


number_of_blocks <- length(unique(randomization$block))


# check if the number of plots in each block are equal
table(randomization$block)

##plots_per_block <- unique(table(randomization$block)) # if the number of plots are not equal in all blocks this will mess things up


##stopifnot(length(plots_per_block)==1)  # if the number of plots per block are not equal "length(plots_per_block)" will be greater than 1

total_plots
number_of_blocks
##plots_per_block


# double check that the plot and block numbers match up
#if_else(total_plots == number_of_blocks*plots_per_block, "ALL GOOD" , "DANGER")

#stopifnot(total_plots == number_of_blocks*plots_per_block)




### What are the pea accessions?
#pea_accessions <- randomization %>% 
  #filter(crop == "Pea") %>%
 # select(accession) %>% 
  #unique()

pea_accessions <- randomization$intercrop_accession_name %>% 
  unique()


### Trial set up  ###


# preliminary calculation

trial <- randomization %>% 
  mutate(plot_number = str_pad(as.character(plotNo), width = 3, side = "left", pad = "0"))  # add padding to plot numbers, I like this so that all plot names are the same number of characters




# renaming for T3 upload
trial <- trial %>% 
  
  mutate(plot_name = str_c(trial_name,"_",plot_number)) %>% 
  
  mutate(block_number  = as.numeric(str_remove(randomization$block, "Block" ))) %>% 
  
  mutate(is_a_control = "") %>% 
  
  mutate(rep_number = "") %>% 
  
  mutate(range_number ="") %>% 
  
  mutate(row_number = row_number) %>%
  
  mutate(col_number = col_number) %>% 
  mutate(seedlot_name = seedlot_name) %>% 
  mutate(num_seed_per_plot = "") %>% 
  mutate(weight_gram_seed_per_plot = "41") %>% 	
  mutate(entry_number = "") %>% 
  mutate(is_private = "")



trial <- trial %>% 
  select(plot_name,
         accession_name,
         plot_number,
         block_number,
         is_a_control, 
         rep_number,               
         range_number,
         row_number,     
         col_number,             
         seedlot_name,         
         num_seed_per_plot,      
         weight_gram_seed_per_plot,
         entry_number,
         is_private,
         intercrop_accession_name)  


full_trial <- bind_cols( trial_meta ,trial) 


##write.csv(full_trial,str_c("data/",trial_name,"_t3_upload.csv"), row.names = FALSE)

#changes made in excel while uploading
# NO_PEA/ NO_OAT to plural NO_PEAS/ NO_OATS
# Date format
# NO_OAT_Planted seedlot cant be blaze, seedlots match accession







