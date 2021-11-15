# Multifunctionality of dike grasslands 
# Prepare data of river Danube datatset
# Markus Bauer
# 2021-11-12
# Citation: 
## Teixeira LH, Bauer M, Moosner M, Kollmann J (submitted) 
## Multifunctionality of dike grasslands: Trade-offs between flood protection, biodiversity, recreation and management. 
## unpublished data.



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Load data ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)

### Start ###
#installr::updateR(browse_news = F, install_R = T, copy_packages = T, copy_Rprofile.site = T, keep_old_packages = T, update_packages = T, start_new_R = F, quit_R = T, print_R_versions = T, GUI = F)
#sessionInfo()
rm(list = ls())
setwd(here("data/raw"))

### Load data ###
sites <- read_csv("data_raw_danube.csv", col_names = T, na = c("na", "NA"), col_types = 
                    cols(
                      .default = "?"
                    ))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


sites <- sites %>%
  mutate(surveyYearF = factor(surveyYear),
         botanist = str_replace_all(botanist, " ", "_"),
         botanistYear = str_c(botanist, surveyYear))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


write_csv(sites, here("data/processed/data_processed_danube.csv"))


