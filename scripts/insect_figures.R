## Make figures for the BACI study for the diptera results:
##
## First edit: 20200302
## Last edit:  20200508
##
## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

require("ggplot2")

## 2. Load and transform data --------------------------------------------------

cover_NF <- read.csv("clean/BACI_cover_NF.csv")
pm_NF <- read.csv("clean/BACI_pm_NF.csv")
cover_CR <- read.csv("clean/BACI_cover_CR.csv")
pm_CR <- read.csv("clean/BACI_pm_CR.csv")

## Exclude URT from NF controls:
cover_NF <- droplevels(cover_NF[cover_NF$treatment != "URT", ])
pm_NF <- droplevels(pm_NF[pm_NF$treatment != "URT", ])

## Combine all data:
cover <- rbind(cover_NF, setNames(cover_CR, names(cover_NF)))
cover$response <- "Mean increment (% trap cover) per day"
pm <- rbind(pm_NF, setNames(pm_CR, names(pm_NF)))
pm$response <- "Day after march with highest increment"
BACI_i <- rbind(cover, pm)

## Change level names:
levels(BACI_i$treatment) <-  c("Complete retention", 
                              "Conventional thinning", 
                              "Understory retention thinning")
levels(BACI_i$indicator) <- c("BACI-contrast", 
                              "CI-contribution", 
                              "CI-divergence")
levels(BACI_i$ref) <- c("Control = NF", "Control = CR")

## 3. Make combined figure with probabilities ----------------------------------

## Graph for probs:

p1 <- ggplot(data = BACI_i, aes(x = response, y = 0, colour = treatment))
p2a <- geom_errorbar(aes(ymin = 0, ymax = BACI_i$ecdf), 
                     position = position_dodge(0.6),
                     size = 6, 
                     width = 0)
p2b <- geom_errorbar(aes(ymin = BACI_i$ecdf - 1, ymax = 0), 
                     position = position_dodge(0.6),
                     size = 6,                  
                     width = 0)
p3 <- facet_grid(ref ~ indicator)

P <- p1 + geom_hline(yintercept = 0, size = 1, color = "darkgrey") +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1),
                     labels = c("1", ".5", "0", "", ""),
                     sec.axis = dup_axis(
                       name = "Probability that the indicator is positive",
                       labels = c("", "", "0", ".5", "1"))) +
  p2a + p2b + p3 +
  xlab("") + ylab("Probability that the indicator is negative") + 
  coord_flip() +
  scale_colour_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  theme_light(35) +
  theme(legend.position = c(-0.36, 1.2), 
        legend.title = element_blank(),
        legend.key.size = unit(3, 'lines'),
        legend.box = "vertical")

png("figures/BACI_i_probs.png", 12000/8, 5500/8, "px", res = 600/8)
P
dev.off()

## -------------------------------END-------------------------------------------

