# Multifunctionality of dike grasslands 
# Calculate model Flowering species proportion ~ Treatment * Month
# Michaela Moosner
# 2021-11-12
# Citation: 
## Bauer M, Teixeira LH, Moosner M, Kollmann J (2021) 
## Multifunctionality of dike grasslands: Trade-offs between flood protection, biodiversity, recreation and management. 
## unpublished data.



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(nlme)

### Start ###
rm(list = ls())
setwd(here("data/processed"))

### Load data ###
flowers <- read.table("data_processed_inn.csv", header = TRUE, sep = ",", dec = ".") %>%
  mutate(month = factor(month, levels = c("May", "Jun", "Jul", "Aug", "Sep")))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### a model ----------------------------------------------------------------------------------------
m <- lme(prop.flow.spec ~ treatment * month, data = flowers, random = ~1|area/subplot)

### b model check ----------------------------------------------------------------------------------------
plot(m)
qqnorm(m)

### c model output ----------------------------------------------------------------------------------------
MuMIn::r.squaredGLMM(m) #R2m = 0.481, R2c = 0.567
summary(m)
car::Anova(m, type = 3)
summary(multcomp::glht(m, emmeans::lsm(pairwise ~ treatment | month), by = NULL))

### Save ###
table <- broom::tidy(car::Anova(m, type = 3))
write.csv(table, here("outputs/statistics/table_anova_flowerSpecies_treatment_month.csv"))

