## This script analyses if trap hapiness is occurring in the case of sticky
## traps. We tested the year as random and fixed effect term and the rsults were 
## the same. We then used random effects because predicting is easier.
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

## Make two log-normal mixed-models and compare AIC:
model50.1 <- lmer(log(acc_mi) ~ scale(ud5_50m_buffer) + (1|obs_year) + (1|plot),
                  data = icl)
model50.2 <- lmer(log(acc_mi) ~ poly(scale(ud5_50m_buffer), 2) + 
                                (1|obs_year) + (1|plot),
                  data = icl)

par(mfrow = c(1,2))
qqnorm(resid(model50.2)); qqline(resid(model50.2)) 
hist(resid(model50.2))

dir.create("results")
capture.output(model.sel(model50.1, model50.2), 
               summary(model50.2), 
               print("R2:"), 
               r.squaredGLMM(model50.2)) %>% write(., "results/th50m_buffer.txt")

## 5. Anayse if the forest density around the trap affeects the insect cover in 
##    open forests:

## Select those traps which are open (< 10? find threshold above) at the 50m buffer:
icl5 <- icl[icl$ud5_50m_buffer < mean(lidar$ud5_50m_buffer), ]

plot(icl5$ud5_5m_buffer, icl5$acc_mi)

## Make two log-normal mixed-models and compare AIC:
model5.1 <- lmer(log(acc_mi) ~ scale(ud5_5m_buffer) + (1|obs_year) + (1|plot),
                 data = icl5)
model5.2 <- lmer(log(acc_mi) ~ poly(scale(ud5_5m_buffer), 2) + 
                               (1|obs_year) + (1|plot),
                 data = icl5)

par(mfrow = c(1,2))
qqnorm(resid(model5.2)); qqline(resid(model5.2)) 
hist(resid(model5.2))

dir.create("results")
capture.output(model.sel(model5.1,  model5.2),
               summary(model5.2), 
               print("R2:"), 
               r.squaredGLMM(model5.2)) %>% write(., "results/th5m_buffer.txt")

## 6. Make a figure illustrating the results -----------------------------------

## Combine 50m and 5m buffer data set:
D50 <- cbind(icl[, -5], 
             exp(predict(model50.2, re.form = NA)), 
             "buffer" = "50m around trap")
colnames(D50)[5] <- "ud5"
D5 <- cbind(icl5[, -6], 
            exp(predict(model5.2, re.form = NA)), 
            "buffer" = "5m around trap")
colnames(D5)[5] <- "ud5"
D <- rbind(D50, D5)
D$open <- ifelse(D$trap %in% D$trap[D$buffer == "5m around trap"], 
                 "Open, 50m around trap",
                 "Dense, 50m around trap")

## Annotation data:
ann_text <- data.frame(ud5 = c(21.5, 16.5), 
                       acc_mi = c(0.088, 0.12),
                       open = NA,
                       buffer = factor(unique(D$buffer)))

## Make figure:
G <- ggplot(D, aes(x = ud5, y = acc_mi, colour = open)) +
     geom_point(size = 10, alpha = 0.6) +
     geom_line(aes(y = V2), size = 6, color = "black") +
     facet_grid(. ~ buffer, scales = "free_x") +
     xlab("Understory density (% laser returns 0.5-5m above ground)") +
     ylab("Mean increment (%trap cover) per day") +
     coord_trans(y = "log") +
     scale_color_manual(breaks = c("Open, 50m around trap",
                                   "Dense, 50m around trap"),
                        values = c("orange", "darkgreen")) +
     geom_text(data = ann_text, 
               label = c("P(x) < .001,  P(x2) = .76", 
                         "P(x) = .74, P(x2) = .23"), 
               show.legend = FALSE,
               size = 11,
               colour = "black") +
     theme_light(40) +
     theme(legend.position = "top",#c(0.36, 0.95),
           legend.title = element_blank(),
           legend.direction = "horizontal")
  
png("figures/trap_hapiness.png", 15000/4, 10000/4, "px", res = 600/4)
G
dev.off()

## -------------------------------END-------------------------------------------