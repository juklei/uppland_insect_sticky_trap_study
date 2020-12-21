## Make figures for the BACI study for the diptera results:
##
## First edit: 20200302
## Last edit:  20201221
##
## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

require("ggplot2")

## 2. Load and transform data --------------------------------------------------

cover <- read.csv("clean/exp_intercepts_cover.csv")
pm <- read.csv("clean/exp_intercepts_pm.csv")

## Combine all data:
cover$response <- "Mean increment per day"
pm$response <- "Day with highest increment"
comb <- rbind(cover, pm)

## Change level names and order:
levels(comb$treatment) <-  c("Complete retention", 
                             "Conventional", 
                             "No forestry",
                             "Understory retention")
comb$experiment <- factor(comb$experiment, levels = c("before", "after"))
levels(comb$experiment) <- c("Before", "After")

## 3. Make combined figure with median and 95%CIs ------------------------------

## Graph for probs:

p1 <- ggplot(data = comb, aes(x = experiment, y = X50., colour = treatment))
p2 <- geom_point(size = 5, position = position_dodge(0.6))
p3 <- geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), width = 0.3, size = 1.5,
                    position = position_dodge(0.6))
p4 <- facet_grid(response ~ ., scales = "free_y")

P <- p1 + p2 + p3 + p4 +
  xlab("") + ylab("% Trap cover               Days post march") +
  scale_colour_manual(values = c("#00AFBB", "#FC4E07", "black", "#E7B800")) +
  theme_light(35) +
  theme(legend.position = c(0.34, 0.41), 
        legend.title = element_blank(),
        legend.text = element_text(size = 26),
        legend.key = element_blank(),
        legend.key.size = unit(2, 'lines'),
        legend.background =  element_blank(),
        legend.box = "vertical")

png("figures/exp_intersects.png", 5250/8, 7000/8, "px", res = 600/8)
P
dev.off()

## -------------------------------END-------------------------------------------

