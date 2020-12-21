## This script prepares the data from the sticky traps and runs the analysis
## in the BACI experiment
##
## First edit: 20200218
## Last edit: 20201221
##
## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

require(data.table)
require(rjags)
require(runjags)
require(coda)
require(dplyr)

## 2. Load and explore data ----------------------------------------------------

dir("data")
i_2017 <- read.csv("data/insects_pl_2017.csv")
i_2018 <- read.csv("data/insects_pl_2018.csv")
i_2019 <- read.csv("data/insects_pl_2019.csv")
forest <- read.csv("data/forest_data_uppland_plot.csv")

## 3. Prepare insect data and extract acc. cover and date of highest -----------
##    increase and merge with forest data increase

i_comb <- as.data.table(rbind(i_2017, i_2018, i_2019))

## Include only days which are covered across all plots (exclude 120):
temp <- i_comb[i_comb$plot != "plot_120",
               list("min" = min(post_march), "max" = max(post_march)), 
               by = c("plot", "obs_year")]
pm_limits <- c(max(temp$min), min(temp$max))
rm(temp)
i_comb <- i_comb[i_comb$post_march >= pm_limits[1] & 
                 i_comb$post_march <= pm_limits[2], ]

## Calculte mean increment per plot and year:
i_out <- i_comb[, list("acc_mi" = mean(mean_incr),
                       "pm_max" = mean(post_march[which(mean_incr == 
                                                        max(mean_incr))])),
                by = c("plot", "obs_year")]

## Merge with forest data required for the BACI analysis:

if_comb <- merge(i_out, 
                 unique(forest[, c("plot", "block", "effect_year", "treatment")]), 
                 by = "plot")

## 4. Prepare the model data, the inits and load the model ---------------------

## Adjust response here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
response <- "cover"
# response <- "pm"
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## Create model data set:
resp <- if(response == "cover") if_comb$acc_mi*100 else if_comb$pm_max
data <- list(nobs = nrow(if_comb),
             block = as.numeric(if_comb$block),
             resp = resp,
             exp = ifelse(if_comb$obs_year < if_comb$effect_year, 1, 2),
             treat = as.numeric(if_comb$treatment),
             year_2018 = ifelse(if_comb$obs_year == 2018, 1, 0),
             year_2019 = ifelse(if_comb$obs_year == 2019, 1, 0),
             eval = c(2, 4),
             ref = 1)

## Create inits:
inits <- list(list(a = matrix(0, max(data$treat), max(data$exp)),
                   sigma = 0.001,
                   b_2018 = 0,
                   b_2019 = 0,
                   sigma_block = 0.001),
              list(a = matrix(5, max(data$treat), max(data$exp)),
                   sigma = 5,
                   b_2018 = 5,
                   b_2019 = 5,
                   sigma_block = 5),
              list(a = matrix(1, max(data$treat), max(data$exp)),
                   sigma = 1,
                   b_2018 = -5,
                   b_2019 = -5,
                   sigma_block = 1)
              )

start <- Sys.time()

model <- "scripts/JAGS/insect_JAGS_cover&pm.R"

jm <- jags.model(model,
                 data = data,
                 n.adapt = 5000, 
                 inits = inits, 
                 n.chains = 3) 

burn.in <-  50000
update(jm, n.iter = burn.in) 

samples <- 20000
n.thin <- 16

zc <- coda.samples(jm,
                   # variable.names = "sigma_block",
                   variable.names = c("a", "sigma", "sigma_block",
                                      "b_2018", "b_2019"),
                   n.iter = samples, 
                   thin = n.thin)

end <- Sys.time()
end-start

## Export parameter estimates:
capture.output(summary(zc), HPDinterval(zc, prob = 0.95)) %>% 
  write(., paste0("results/parameters_insect_", response, ".txt"))

## 5. Validate the model and export validation data and figures ----------------

pdf(paste0("figures/plot_insect_", response, ".pdf"))
plot(zc); gelman.plot(zc) 
dev.off()

capture.output(raftery.diag(zc), 
               heidel.diag(zc), 
               gelman.diag(zc),
               cor(data.frame(combine.mcmc(zc)))) %>% 
  write(., paste0("results/diagnostics_insect_", response, ".txt"))

## Produce validation metrics:
zj_val <- jags.samples(jm,
                       variable.names = c("mean_obs", "mean_sim","p_mean",
                                          "cv_obs", "cv_sim", "p_cv",
                                          "fit", "fit_sim", "p_fit"),
                       n.iter = samples,
                       thin = n.thin)

## Fit of mean:
plot(zj_val$mean_obs,
     zj_val$mean_sim,
     xlab = "mean real",
     ylab = "mean simulated",
     cex = .05)
abline(0, 1)
p <- summary(zj_val$p_mean, mean)
text(x = 0.6, y = 0.7, paste0("P=", round(as.numeric(p[1]), 4)), cex = 1.5)

## Fit of variance:
plot(zj_val$cv_obs,
     zj_val$cv_sim,
     xlab = "cv real",
     ylab = "cv simulated",
     cex = .05)
abline(0,1)
p <- summary(zj_val$p_cv, mean)
text(x = 1, y = .8, paste0("P=", round(as.numeric(p[1]), 4)), cex = 1.5)

## Overall fit:
plot(zj_val$fit,
     zj_val$fit_sim,
     xlab = "ssq real",
     ylab = "ssq simulated",
     cex = .05)
abline(0,1)
p <- summary(zj_val$p_fit, mean)
text(x = 0.2, y = 0.4, paste0("P=", round(as.numeric(p[1]), 4)), cex = 1.5)

## 6. Export all 8 intercepts forgraphing --------------------------------------

zj_out <- coda.samples(jm, 
                       variable.names = "a_bt",
                       n.iter = samples, 
                       thin = n.thin)

## Export for graphing:

zj_out_exp <- as.data.frame(summary(zj_out)$quantiles)
zj_out_exp <- cbind(zj_out_exp, 
                    "treatment" = levels(if_comb$treatment),
                    "experiment" = c(rep("before", 4), rep("after", 4)))

## Export:
write.csv(zj_out_exp, paste0("clean/exp_intercepts_", response, ".csv"))

## 7. Export BACI indicators ---------------------------------------------------

zj_out2 <- coda.samples(jm, 
                        variable.names = c("BACI", "CI_div", "CI_ctr"),
                        n.iter = samples, 
                        thin = n.thin)
zj_out2 <- combine.mcmc(zj_out2)
dimnames(zj_out2)[[2]] <- sort(paste0(rep(c("BACI_", "CI_ctr_", "CI_div_"), 2), 
                                      c("Conventional", "Understory_retention")))

BACI <- as.data.frame(summary(zj_out2)$quantiles)
BACI$ecdf <- apply(zj_out2, 2, function(x) 1-ecdf(x)(0))
write.csv(BACI, paste0("results/BACI_insect_", response, ".csv"))

## -------------------------------END-------------------------------------------
