## This script analyses if trap hapiness is occurring in the case of sticky
## traps.
##
## First edit: 20200626
## Last edit: 20200626
##
## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

require(data.table)
require(lme4)
require(lmerTest)
require(ggplot2)
require(MuMIn)
require(dplyr)

## 2. Load and explore data ----------------------------------------------------

dir("data")
i_2017 <- read.csv("data/insects_tl_2017.csv")
i_2018 <- read.csv("data/insects_tl_2018.csv")
i_2019 <- read.csv("data/insects_tl_2019.csv")
forest <- read.csv("data/forest_data_uppland_plot.csv")
lidar <- read.csv("data/lidar_data_sticky_trap.csv")

## 3. Select the traps which fulfill the requirements for testing trap ---------
##    happines, combine insect data, drop al post treatment data, keep only the 
##    period for which data is available for all traps, and add lidar data. 

## Combine all inect data:
i_comb <- rbind(i_2017, i_2018, i_2019)

## Remove post treatment data, except for No forestry (TC) plots:
i_comb <- merge(i_comb, 
                unique(forest[, c("plot", "effect_year", "treatment")]), 
                by = "plot")
i_comb <- i_comb[(i_comb$obs_year < i_comb$effect_year) | 
                   i_comb$treatment == "TC", ]

## Include only days which are covered across all plots (exclude 120):
i_comb <- as.data.table(i_comb)
temp <- i_comb[i_comb$plot != "plot_120",
               list("min" = min(post_march), "max" = max(post_march)), 
               by = c("plot", "obs_year")]
pm_limits <- c(max(temp$min), min(temp$max))
rm(temp)
i_comb <- i_comb[i_comb$post_march >= pm_limits[1] & 
                   i_comb$post_march <= pm_limits[2], ]

## Calculte mean increment per plot trap and year (-9999 is NA):
i_comb$mean_incr[i_comb$mean_incr == -9999] <- NA
i_comb_out <- i_comb[, list("acc_mi" = mean(mean_incr, na.rm = TRUE)*100),
                     by = c("plot", "trap", "obs_year")]

## Add lidar data:
icl <- merge(i_comb_out, lidar, by = c("plot", "trap"))

## Add unique trap_id for random effect:
icl$trap <- paste0(icl$plot, "_", icl$trap)

## 4. Anayse if the forest density 50 m around the trap affects the insect cover 
##    on the trap forests:

plot(icl$ud5_50m_buffer, icl$acc_mi)

model50 <- lmer(log(acc_mi) ~ scale(ud5_50m_buffer) + (1|obs_year) + (1|plot),
                data = icl)

par(mfrow = c(1,2))
qqnorm(resid(model50)); qqline(resid(model50)) 
hist(resid(model50))

dir.create("results")
capture.output(summary(model50), print("R2:"), r.squaredLR(model50)) %>% 
  write(., "results/th_50m_buffer.txt")

## 5. Anayse if the forest density around the trap affeects the insect cover in 
##    open forests:

## Select those traps which are open (< 10? find threshold above) at the 50m buffer:
icl5 <- icl[icl$ud5_50m_buffer < mean(lidar$ud5_50m_buffer), ]

plot(icl5$ud5_5m_buffer, icl5$acc_mi)

model5 <- lmer(log(acc_mi) ~ scale(ud5_5m_buffer) + (1|obs_year) + (1|plot),
               data = icl5)

par(mfrow = c(1,2))
qqnorm(resid(model5)); qqline(resid(model5)) 
hist(resid(model5))

dir.create("results")
capture.output(summary(model5), print("R2:"), r.squaredLR(model5)) %>% 
  write(., "results/th_5m_buffer.txt")

## 6. Make a figure illustrating the results -----------------------------------

g1 <- ggplot(icl, aes(ud5_5m_buffer, acc_mi*))

## -------------------------------END-------------------------------------------