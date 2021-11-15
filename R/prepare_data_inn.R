# Multifunctionality of dike grasslands 
# Prepare data of river Inn datatset
# Markus Bauer
# 2021-11-12
# Citation: 
## Bauer M, Teixeira LH, Moosner M, Kollmann J (2021) 
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
flowers <- read_csv2("data_raw_inn.csv", col_names = T, na = c("na", "NA", ""), col_types = 
                    cols(
                      .default = "?"
                    ))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


flowers <- flowers %>%
  mutate(prop.flow.spec = numb.flow.spec / numb.spec * 100,
         prop.flow.spec = round(prop.flow.spec, digits = 4))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


write_csv(flowers, here("data/processed/data_processed_inn.csv"))


