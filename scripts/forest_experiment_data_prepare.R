## Take the forest data and create all sorts of explamnatory variables related 
## to the thinning experiment

## First edit: 20191023
## Last edit:  20200227

## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

library(data.table)
library(reshape)

## 2. Load and explore data ----------------------------------------------------

dir("data")
forest <- read.csv("data/forest_data_uppland_plot.csv")
head(forest)

## Combines Sönkes blocks to one:
levels(forest$block)[c(2,3,7,10)] <- "sigtuna"

## 3. Reduce data set to needed variables, claculate percentages, --------------
##    and calculate differences due to treatments.

## Exclude unneeded varables:
f_red <- as.data.table(forest[,c(1:3,19:length(forest))])

## Calculate percentages:
f_red$perc_gran <- f_red$nr_gran/f_red$nr_all_alive
f_red$perc_lov <- f_red$nr_lov/f_red$nr_all_alive
f_red$perc_tall <- f_red$nr_tall/f_red$nr_all_alive
f_red$perc_skarm <- f_red$nr_skarm/f_red$nr_all_alive
f_red$perc_dv <- f_red$nr_staende_dodved/(f_red$nr_all_alive + 
                                            f_red$nr_staende_dodved)

## Calculate BA:
f_red$BA_gran <- (f_red$average_dbh_gran/200)^2*pi*f_red$nr_gran
f_red$BA_gran <- f_red$BA_gran/(10^2*pi*3/10000)
f_red$BA_lov <- (f_red$average_dbh_lov/200)^2*pi*f_red$nr_lov
f_red$BA_lov <- f_red$BA_lov/(10^2*pi*3/10000)
f_red$BA_tall <- (f_red$average_dbh_tall/200)^2*pi*f_red$nr_tall
f_red$BA_tall <- f_red$BA_tall/(10^2*pi*3/10000)
f_red$BA_dv <- (f_red$average_dbh_staende_dodved/200)^2*pi*f_red$nr_staende_dodved
f_red$BA_dv <- f_red$BA_dv/(10^2*pi*3/10000)
f_red$BA <- (f_red$average_dbh_all_alive/200)^2*pi*f_red$nr_all_alive
f_red$BA <- f_red$BA/(10^2*pi*3/10000)

## Calculate differences:

## Which plots have after data?
B1 <- f_red$plot[f_red$experiment == "after"]
f_diff <- f_red[f_red$plot %in% B1,]

## Calculate the difference:
diff <- f_diff[f_diff$experiment == "after",] - 
        f_diff[f_diff$experiment == "before",]

## Replace name variables in the new file with the unique values from above:
diff[,c(1:2,16)] <- unique(f_diff[,c(1:2,16)])
diff$experiment <- as.character(diff$experiment)
diff$experiment <- "difference"

## Join before-after with differences:
f_all <- rbind(f_red,diff)

## 4. Adjust the raw experiment data to data by year for the JAGS model --------

## Create a "after" data set for all true controls, except for those that 
## already have some "after" measurments, and add to f_raw:
T1 <- f_red$plot[f_red$experiment == "after" & f_red$treatment == "TC"]
T1 <- f_red[f_red$treatment == "TC" & !f_red$plot %in% T1, ]
T1$experiment <- "after"
f_red <- rbind(f_red, T1)

## Replace NAs in the combination after*control|tc with before*control|tc
F1 <- function(x){
  x[x$treatment %in% c("C", "TC") & x$experiment == "after", c(3:13, 17:26)] <- 
    x[x$treatment %in% c("C", "TC") & x$experiment == "before", c(3:13, 17:26)]
  return(x)
}
f_raw <- f_red[, F1(.SD), by = "plot"]

## Fill for all years:
T2 <- expand.grid.df(unique(f_raw[, c("block", "plot", "effect_year")]),
                     data.frame("year" = c(2017, 2018, 2019)))
T3 <- merge(T2[T2$effect_year > T2$year,], f_raw[f_raw$experiment == "before"],
            by = c("block", "plot", "effect_year"))
T4 <- merge(T2[T2$effect_year <= T2$year,], f_raw[f_raw$experiment == "after"],
            by = c("block", "plot", "effect_year"))

f_raw <- rbind(T3,T4)

## 5. Export:

dir.create("clean")
write.csv(f_all, "clean/forest_experiment_data.csv", row.names = F)
write.csv(f_raw, "clean/forest_experiment_data_JAGS.csv", row.names = F)

## -----------------------------------END---------------------------------------