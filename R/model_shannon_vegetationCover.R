# Multifunctionality of dike grasslands 
# Calculate model Shannon ~ Vegetation cover
# Markus Bauer
# 2021-11-12
# Citation: 
## Teixeira LH, Bauer M, Moosner M, Kollmann J (submitted) 
## Multifunctionality of dike grasslands: Trade-offs between flood protection, biodiversity, recreation and management. 
## unpublished data.



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(here)
library(tidyverse)
library(lmerTest)
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
                      surveyYear = "d",
                      surveyYearF = "f",
                      botanistYear = "f",
                      exposition = "f",
                      substrateType = "f",
                      seedmixType = "f"
                    )) %>%
  mutate(vegetationCov_scaled = scales::rescale(vegetationCov),
         n = shannon) 



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#2way
ggplot(sites, aes(x = vegetationCov, y = n)) + 
  geom_point(aes(color = surveyYearF)) + 
  geom_smooth(aes(color = surveyYearF), method = "loess", se = T) +
  geom_smooth(method = "loess", se = T, color = "black")
#3way
ggplot(sites, aes(x = vegetationCov_scaled, y = n)) + 
  geom_point(aes(color = surveyYearF),) + 
  geom_smooth(aes(color = surveyYearF), 
              method = "lm", se = F) +
  geom_smooth(method = "loess", se = T,
              color = "black") +
  facet_wrap(~exposition)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
dotchart((sites$n), groups = factor(sites$exposition), main = "Cleveland dotplot")
dotchart((sites$n), groups = factor(sites$surveyYearF), main = "Cleveland dotplot")
boxplot(sites$n);#identify(rep(1, length(edata$rgr13)), edata$rgr13, labels = c(edata$n))
plot(table((sites$n)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(sites, aes(n)) + geom_density()
ggplot(sites, aes(vegetationCov_scaled)) + geom_density()
ggplot(sites, aes(exp(vegetationCov_scaled))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1a <- lmer(n ~ 1 + (1|botanistYear) + (surveyYear|block/plot), sites, REML = T)
m1b <- lmer(n ~ 1 + (1|botanistYear) + (1|block/plot), sites, REML = T)
VarCorr(m1a)
VarCorr(m1b)
#fixed effects
m2 <- lmer(n ~ (vegetationCov_scaled + surveyYearF + exposition)^2 + 
             seedmixType + substrateType +
             (1|botanistYear) + (1|block/plot),
           data = sites, 
           REML = F)
simulateResiduals(m2, plot = T)
m3 <- lmer(n ~ vegetationCov_scaled * surveyYearF * exposition + 
             seedmixType + substrateType +
             (1|botanistYear) + (1|block/plot),
           data = sites, 
           REML = F)
simulateResiduals(m3, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
anova(m2, m3) #similar but m3 better model critic
rm(list = setdiff(ls(), c("sites", "m3")))

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m3, plot = T)
testOutliers(simulationOutput)
plotResiduals(simulationOutput$scaledResiduals, sites$surveyYearF)
plotResiduals(simulationOutput$scaledResiduals, sites$botanistYear)
plotResiduals(simulationOutput$scaledResiduals, sites$block)
plotResiduals(simulationOutput$scaledResiduals, sites$plot)
plotResiduals(simulationOutput$scaledResiduals, sites$exposition)
plotResiduals(simulationOutput$scaledResiduals, sites$substrateType)
plotResiduals(simulationOutput$scaledResiduals, sites$seedmixType)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
MuMIn::r.squaredGLMM(m3)
VarCorr(m3)
sjPlot::plot_model(m3, type = "re", show.values = T)
car::Anova(m3, type = 3)
sjPlot::plot_model(m2, type = "emm", terms = c("vegetationCov_scaled"))

### Effect sizes -----------------------------------------------------------------------------------------
emmeans::emtrends(m3, pairwise ~ surveyYearF, var = "vegetationCov_scaled", mult.name = "surveyYearF")

### Save ###
table <- broom::tidy(car::Anova(m3, type = 3))
write.csv(table, here("outputs/statistics/table_anova_shannon_vegetationCov.csv"))
