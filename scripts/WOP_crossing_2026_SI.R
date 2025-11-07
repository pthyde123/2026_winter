library(tidyverse)
library(readr)
library(lme4)
library(BrAPI)

options(scipen = 999)

### Input files  phenotypes and genotypes 
#T3 search wizard/trials/Cornell_WinterOatPeaIntercrop_2024_Ithaca,CU_2025_Ithaca_WOP_PLOT

WOP_2024_2025_t3_phenotypes <- read_csv("data/WOP_2024_2025_t3_phenotypes.csv")
GRM_long_format <- read_csv("data/GRM_long_format.csv")


phenotypes <- WOP_2024_2025_t3_phenotypes %>% 
  select(studyYear,studyName,germplasmName,observationLevel,intercropGermplasmName,
         blockNumber,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`,
         `Grain yield - g/m2|CO_350:0000260`,
         `Pea Grain Yield - g/m2|CO_xxx:0003008`) %>% 
  filter(observationLevel == "plot") %>% 
  filter(germplasmName != "NO_OATS_PLANTED" ) %>% 
  mutate(year_block = str_c(studyYear,"_",blockNumber)) %>% 
  
  mutate(`Freeze damage severity - 0-9 Rating|CO_350:0005001` = if_else(germplasmName == "NF13-4126-4_3" & studyName == "CU_2025_Ithaca_WOP_PLOT",
                                     NA,`Freeze damage severity - 0-9 Rating|CO_350:0005001` )) %>% 
  rename(damage = `Freeze damage severity - 0-9 Rating|CO_350:0005001`) %>%
  rename(grain_yield = `Grain yield - g/m2|CO_350:0000260`)  %>% 
  rename(pea_yield = `Pea Grain Yield - g/m2|CO_xxx:0003008`)


phenotypes

count(phenotypes,germplasmName) %>% 
  print(n=110)

### Damage BLUPS ###
#I only used Damage and not winter survival because there was historic T3 data on Damage vs Yield
lm_damage <- lme4::lmer(damage ~ (1|germplasmName) + (1|year_block), data = phenotypes)

df_damage <- as.data.frame(VarCorr(lm_damage))
vg_damage <- df_damage[df_damage$grp == "germplasmName","vcov"]
ve_damage <- df_damage[df_damage$grp == "Residual", "vcov"]
h2_damage <- vg_damage/(vg_damage + ve_damage/(2))  # 2 is the minimum number of reps

allBLUPs <- ranef(lm_damage)
germplasmNameBLUPs <- allBLUPs$germplasmName
colnames(germplasmNameBLUPs)[1] <- "damage_BLUPs"

germplasmNameBLUPs$Genotype <- rownames(germplasmNameBLUPs)
germplasmNameBLUPs <- germplasmNameBLUPs[,c(2,1)]
rownames(germplasmNameBLUPs) <- NULL

damage_BLUPs <- germplasmNameBLUPs

damage_BLUPs



### Grain Yield BLUPS
#PrEf
lm_grain_y <- lme4::lmer(grain_yield ~ (1|germplasmName) + (1|year_block), data = phenotypes)

df_grain_y <- as.data.frame(VarCorr(lm_grain_y))
vg_grain_y <- df_grain_y[df_grain_y$grp == "germplasmName","vcov"]
ve_grain_y <- df_grain_y[df_grain_y$grp == "Residual", "vcov"]
h2_grain_y <- vg_grain_y/(vg_grain_y + ve_grain_y/(2))  # 2 is the minimum number of reps

allBLUPs <- ranef(lm_grain_y)
germplasmNameBLUPs <- allBLUPs$germplasmName
colnames(germplasmNameBLUPs)[1] <- "grain_y_BLUPs"

germplasmNameBLUPs$Genotype <- rownames(germplasmNameBLUPs)
germplasmNameBLUPs <- germplasmNameBLUPs[,c(2,1)]
rownames(germplasmNameBLUPs) <- NULL

grain_y_BLUPs <- germplasmNameBLUPs
grain_y_BLUPs


#### Pea Yield  
#AsEf
#removed mono oat plots 

lm_pea_y <- lme4::lmer(pea_yield ~ (1|germplasmName) + (1|year_block), data = phenotypes%>% 
                         filter(intercropGermplasmName == "BLAZE")) ### remove mono oat plots


df_pea_y <- as.data.frame(VarCorr(lm_grain_y))
vg_pea_y <- df_pea_y[df_pea_y$grp == "germplasmName","vcov"]
ve_pea_y <- df_pea_y[df_pea_y$grp == "Residual", "vcov"]
h2_pea_y <- vg_pea_y/(vg_pea_y + ve_pea_y/(2))  # 2 is the minimum number of reps

allBLUPs <- ranef(lm_pea_y)
germplasmNameBLUPs <- allBLUPs$germplasmName
colnames(germplasmNameBLUPs)[1] <- "pea_y_BLUPs"

germplasmNameBLUPs$Genotype <- rownames(germplasmNameBLUPs)
germplasmNameBLUPs <- germplasmNameBLUPs[,c(2,1)]
rownames(germplasmNameBLUPs) <- NULL

pea_y_BLUPs <- germplasmNameBLUPs
pea_y_BLUPs


### Estimate EV of freeze damage using T3 data available with freeze damage and yield   

UWOYT_damage_yield <- read_csv("data/UWOYT_damage_yield.csv") %>% 
  select(germplasmName,studyName,`Freeze damage severity - 0-9 Rating|CO_350:0005001`,`Grain yield - g/m2|CO_350:0000260`) %>% 
  filter(!is.na(`Grain yield - g/m2|CO_350:0000260`)) %>% 
  filter(!is.na(`Freeze damage severity - 0-9 Rating|CO_350:0005001`)) %>% 
  rename(damage = `Freeze damage severity - 0-9 Rating|CO_350:0005001`) %>%
  rename(grain_yield = `Grain yield - g/m2|CO_350:0000260`)

lme4_1 <- lme4::lmer(grain_yield ~ damage+ (1|germplasmName) + (studyName) , data =  UWOYT_damage_yield)

lme4_1

EV_damage <- -11.95


### Combine the BLUPS, add in EV and calculate some SI

full_SI <- damage_BLUPs %>% 
  left_join(grain_y_BLUPs) %>% 
  left_join(pea_y_BLUPs) %>% 
  filter(!is.na(grain_y_BLUPs)) %>% 
  
  mutate(EV_damage = -11.95) %>% 
  mutate(EV_grain_y = 1) %>% 
  mutate(EV_pea_y = .5) %>% 
  
  mutate(SI= (damage_BLUPs*EV_damage)+
           (grain_y_BLUPs*EV_grain_y)+
           (pea_y_BLUPs*EV_pea_y)) %>% 
  
  mutate(SI_damage = damage_BLUPs*EV_damage) %>% 
  
  mutate(SI_grain_y = grain_y_BLUPs*EV_grain_y) %>% 
  
  mutate(SI_pea_y = pea_y_BLUPs*EV_pea_y) %>% 
  
  mutate(PrEfAsEf =  grain_y_BLUPs +  pea_y_BLUPs) %>% 
  
  arrange(-SI)


  
full_SI %>% arrange(-PrEfAsEf)

full_SI %>% arrange(-SI_damage)

full_SI %>% arrange(-SI)

plot(full_SI$grain_y_BLUPs,full_SI$pea_y_BLUPs)

write.csv(full_SI,str_c("data/","WOP_2026_SI.csv"), row.names = FALSE)



### Calculate Cross SI by combining parent SI and GRM value  (P1 and P2 SI from full_SI)

df2 <- full_SI %>% 
  mutate(p1=Genotype) %>% 
  mutate(p2=Genotype)

df2

p1 <- df2 %>% 
  select(p1,SI) 

p2 <- df2 %>% 
  select(p2,SI)

cross <- expand.grid(p1 = df2$p1, p2 = df2$p2)

cross_SI <- cross %>% 
  left_join(p1, by = join_by(p1 == p1)) %>% # left join filters down to only crosses we have
  left_join(p2, by = join_by(p2 == p2)) %>% 
  mutate(cross_SI = SI.x + SI.y) %>% 
  arrange(-cross_SI) %>% 
  mutate(cross = str_c(p1," X ", p2)) %>%
  left_join(GRM_long_format) %>% 
  distinct(cross, .keep_all = TRUE) %>% 
  filter(p1 != p2) %>% 
  
  mutate(SI_GRM = cross_SI + (GRM_Value * -10)) %>% #GRM times -10 give a very light bump for divergent genotypes approx 5%.  
  arrange(-SI_GRM) %>% 
  
  distinct(SI_GRM,.keep_all = TRUE)

cross_SI

### write.csv(cross_SI,str_c("data/","WOP_2026_cross_SI.csv"), row.names = FALSE)


