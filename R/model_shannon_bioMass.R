# Multifunctionality of dike grasslands 
# Calculate model Shannon ~ Biomass
# Markus Bauer
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
library(lme4)
library(DHARMa)

### Start ###
rm(list = ls())
setwd(here("data/processed"))

### Load data ###
sites <- read_csv("data_processed_danube.csv", col_names = T, na = c("na", "NA"), col_types = 
                    cols(
                      .default = "?",
                      id = "c",
                      plot = "f",
                      block = "f",
                      substrateType = "f",
                      seedmixType = "f"
                    )) %>%
  filter(bioMass > 0) %>%
  mutate(n = shannon,
         block = factor(block),
         substrateType = factor(substrateType),
         seedmixType = factor(seedmixType))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

### a Graphs ---------------------------------------------------------------------------------------------
#simple effects
#1way
ggplot(sites, aes(x = log(bioMass), y = n)) + 
  geom_point() + 
  geom_smooth(method = "lm")

#### b Outliers, zero-inflation, transformations? -----------------------------------------------------
boxplot(sites$n);#identify(rep(1, length(edata$rgr13)), edata$rgr13, labels = c(edata$n))
plot(table((sites$n)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(n)) + geom_density()
ggplot(sites, aes((n^2))) + geom_density()


## 2 Model building ################################################################################

### a models ----------------------------------------------------------------------------------------
#random structure
m1a <- lmer(n ~ 1 + (1|block), sites, REML = F)
VarCorr(m1a)
#fixed effects
m2 <- lmer(n ~ log(bioMass) + substrateType + seedmixType + 
             (1|block), 
           data = sites, 
           REML = F);
simulateResiduals(m2, plot = T);
isSingular(m2)
m3 <- lmer(n ~ log(bioMass) + substrateType + seedmixType + graminoidCov + 
             (1|block), 
           data = sites, 
           REML = F);
simulateResiduals(m3, plot = T);
isSingular(m3)
m4 <- lmer(n ~ log(bioMass) + substrateType * seedmixType + graminoidCov + 
             (1|block), 
           data = sites, 
           REML = F);
simulateResiduals(m4, plot = T);
isSingular(m4)

### b comparison -----------------------------------------------------------------------------------------
anova(m2, m3, m4) # m3 slightly lower AIC but worse model critic --> m2 chosen
rm(list = setdiff(ls(), c("sites", "m2")))

### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m2, plot = T)
plotResiduals(simulationOutput$scaledResiduals, sites$block)
plotResiduals(simulationOutput$scaledResiduals, sites$substrateType)
plotResiduals(simulationOutput$scaledResiduals, sites$seedmixType)
plotResiduals(simulationOutput$scaledResiduals, sites$vegetationCov)
plotResiduals(simulationOutput$scaledResiduals, sites$graminoidCov)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
MuMIn::r.squaredGLMM(m2)
VarCorr(m2)
sjPlot::plot_model(m2, type = "re", show.values = T)
car::Anova(m2, type = 2)
sjPlot::plot_model(m2, type = "emm", terms = c("bioMass[all]"))

### Save ###
table <- broom::tidy(car::Anova(m2, type = 2))
write.csv(table, here("outputs/statistics/table_anova_shannon_bioMass.csv"))
