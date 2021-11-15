# Multifunctionality of dike grasslands 
# Figure 2A and 2B to figure 2
# Michaela Moosner
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
library(nlme)
library(ggeffects)
library(ggbeeswarm)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b")))
setwd(here("data/processed"))

### Load data ###
flowers <- read.table("data_processed_inn.csv", header = T, sep = ",", dec = ".") %>%
  mutate(month = factor(month, levels = c("May", "Jun", "Jul", "Aug", "Sep")))

### Choosen model ###
m <- lme(log1p(flow.cov) ~ treatment * month, 
         data = flowers, 
         random = ~1|area/subplot)

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

data_model <- ggeffect(m, c("month", "treatment"), type = "fe", back.transform = T) 

flowers <- flowers %>%
  rename("predicted" = "flow.cov", "group" = "treatment", "x" = "month")
pd <- position_dodge(.6)

(graph_b <- ggplot() +
    geom_quasirandom(data = flowers, 
                aes(x, log1p(predicted), shape = group),
                dodge.width = .6, size = 1, alpha = .2) + 
    geom_errorbar(data = data_model, 
                  aes(x, predicted, group = group, ymin = conf.low, ymax = conf.high),
                  position = pd, width = 0.0, size = 0.4) +
    geom_point(data = data_model, 
               aes(x, predicted, shape = group),
               position = pd, size = 2)+
    annotate("text", 
             label =c("n.s.", "n.s.", "***", "***", "n.s."), 
             x = c(1, 2, 3, 4, 5), 
             y = c(7, 7, 7, 7, 7)) +
    scale_y_continuous(limits = c(-.05, 7), breaks = seq(-100, 400, 1)) +
    labs(x = "Month", y = "Flower cover [cm²] log-transformed", shape = "Mowing date") +
    guides(shape = "none") +
    themeMB())

    
### Save ###
ggsave(here("outputs/figures/figure_2b_(800dpi_8x8cm).tiff"),
       dpi = 800, width = 8, height = 8, units = "cm")

