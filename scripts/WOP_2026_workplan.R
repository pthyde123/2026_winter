
library(readxl)

library(tidyverse)
library(readxl)
library(kableExtra)
library(RColorBrewer)



WOP_2026_work_plan <- read_excel("data/WOP_2026_work_plan.xlsx")


plot<-WOP_2026_work_plan %>% 
  mutate(time_point = as.factor(as.character(time_point))) %>% 
  mutate(name = fct_reorder(name, -order)) %>%
  ggplot() +
  geom_segment( aes(x=name, xend=name, y=start, yend=end,color=time_point),size=3) +
  coord_flip()+
  theme_classic()+
  theme(
    legend.position = "none",) +
  xlab("") +
  ylab("")+
  theme(axis.text.x=element_text(angle = 0, hjust = .5))+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))+
  theme(axis.text.x = element_text( color = "black", size = 16))+
  theme(axis.text.y = element_text( color = "black", size = 12))+
  scale_color_brewer(palette = "Set3") 
  
ggsave("output/WOP_2026_work_plan.png", plot = plot)

