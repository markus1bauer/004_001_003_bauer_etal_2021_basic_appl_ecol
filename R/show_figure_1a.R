# Multifunctionality of dike grasslands 
# Plot figure 1A
# Markus Bauer
# 2021-11-12
# Citation: 
## Teixeira LH, Bauer M, Moosner M, Kollmann J (submitted) 
## Multifunctionality of dike grasslands: Trade-offs between flood protection, biodiversity, recreation and management. 
## unpublished data.



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(lme4)
library(ggeffects)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b")))
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
  select(id, plot, block, surveyYearF, botanistYear, exposition, substrateType, seedmixType, shannon, vegetationCov) %>%
  mutate(vegetationCov_scaled = scales::rescale(vegetationCov),
         n = shannon)

### * Choosen model ####
m3 <- lmer(n ~ vegetationCov * surveyYearF * exposition + 
             seedmixType + substrateType +
             (1|botanistYear) + (1|block/plot),
           data = sites, 
           REML = F)

### * Functions ####
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 9, color = "black"),
    strip.text = element_text(size = 10),
    axis.text.y = element_text(angle = 0, hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.line = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_model <- ggeffect(m3, c("vegetationCov[all]", "surveyYearF"))
data <- sites %>%
  rename(predicted = shannon, x = vegetationCov, group = surveyYearF)
(graph_a <- ggplot() +
    #geom_point(data = data, aes(x = x, y = predicted, color = group)) +
    geom_ribbon(data = data_model, aes(x = x, y = predicted, linetype = group, ymin = conf.low, ymax = conf.high), alpha = .1) +
    geom_line(data = data_model, aes(x = x, y = predicted, linetype = group)) +
    scale_y_continuous(limits = c(0, 3.6), breaks = seq(-100, 400, .5)) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(-100, 500, 20)) +
    scale_linetype_manual(values = c("dotted", "dashed", "dotdash", "solid")) +
    labs(x = "Vegetation cover [%]", y = "Shannon index", linetype = "Year") +
    themeMB()
)

### Save ###
ggsave(here("outputs/figures/figure_1a_(800dpi_8x8cm).tiff"),
       dpi = 800, width = 8, height = 8, units = "cm")
