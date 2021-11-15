# Multifunctionality of dike grasslands 
# Plot figure 1B
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
library(ggeffects)

### Start ###
rm(list = setdiff(ls(), c("graph_a")))
setwd(here("data/processed"))

### Load data ###
sites <- read_csv("data_processed_danube.csv", col_names = T, na = c("na", "NA"), col_types = 
                    cols(
                      .default = "?",
                      block = "f",
                      substrateType = "f"
                    )) %>%
  filter(bioMass > 0) %>%
  select(block, plot, bioMass, shannon, substrateType, seedmixType, conf.low, conf.high)

### Choosen model ###
m2 <- lmer(shannon ~ log(bioMass) + substrateType + seedmixType + 
             (1|block), 
           data = sites, 
           REML = F)

### Functions ###
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



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data <- ggeffect(m2, "bioMass[all]") 
sites <- rename(sites, predicted = shannon, x = bioMass)
(graph_b <- ggplot(data, aes(x, predicted, ymin = conf.low, ymax = conf.high)) +
    geom_point(data = sites, aes(x = x, y = predicted)) +
    geom_line(linetype = "dashed") +
    geom_ribbon(aes(ymin =conf.low, ymax = conf.high), alpha = .1) +
    #annotate("text", label = expression(italic(p)==1.5%*%10^-2), x = 1100, y = 0.7, size = 2) +
    scale_y_continuous(limits = c(0.9, 3), breaks = seq(0, 3, 0.5)) +
    scale_x_continuous(limits = c(0, 1450), breaks = seq(0, 2000, 200)) +
    labs(x = "Biomass [g]", y = "Shannon index") +
    themeMB()
)

### Save ###
ggsave(here("outputs/figures/figure_1b_(800dpi_8x8cm).tiff"),
       dpi = 800, width = 8, height = 8, units = "cm")
