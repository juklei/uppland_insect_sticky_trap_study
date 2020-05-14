## Compare forest strucures among treatments and T and URT after the
## experiment
##
## First edit: 20200226
## Last edit: 20200227
##
## Author: Julian Klein

## 1. Clear environment and load libraries -------------------------------------

rm(list = ls())

require(ggplot2)
require(data.table)
require(dplyr)
require(nlme)
require(reshape2)

## 2. Load and explore data ----------------------------------------------------

forest <- as.data.table(read.csv("clean/forest_experiment_data.csv"))
head(forest)

## 3. Analyse similarity of treatments before ----------------------------------

## We want to compare the treatments only from before:
f_before <- forest[forest$experiment == "before", c(1, 14:16, 23:27)]

## Now analyse for all the forest variables the differences between the 
## treatments:
before_stat <- vector("list", length = length(f_before)-2)
names(before_stat) <- colnames(f_before)[c(-1, -4)]
for(i in names(before_stat)){
  temp <- na.omit(f_before[, c(i, "treatment", "block"), with = FALSE])
  f <- as.formula(paste(i, "treatment", sep = " ~ "))
  before_stat[[i]] <- summary(lm(f, temp))
}

capture.output(before_stat) %>% write("results/forest_treatment_compare.txt") 

## Make figure with estimates, std. errors and signifcances:

## Extract coefficients and create anova table:
bs_coeff <- lapply(before_stat, "[[", "coefficients")
anova.create.1 <- function(x){
  x[2:4, 1] <- x[2:4, 1] + x[1, 1]
  return(x)
}
bs_coeff <- lapply(bs_coeff, anova.create.1)

## Make graphing data frame:
gg_data_1 <- melt.list(bs_coeff)
gg_data_1 <- dcast(gg_data_1, X1 + L1 ~ X2, value.var = "value")
levels(gg_data_1$X1) <- c("Complete retention", 
                          "Conventional thinning", 
                          "Control", 
                          "Understory retention thinning")
gg_data_1$exp <- "Before"

## 4. Analyse similarity of treatments after -----------------------------------

## We want to compare the treatments only after the treatment:
f_after <- forest[forest$experiment == "after" & 
                    forest$treatment %in% c("T", "URT"), 
                  c(1, 14:16, 23:27)]

## Now analyse for all the forest variables the differences between the 
## treatments:
after_stat <- vector("list", length = length(f_after)-2)
names(after_stat) <- colnames(f_after)[c(-1, -4)]
for(i in names(after_stat)){
  temp <- na.omit(f_after[, c(i, "treatment", "block"), with = FALSE])
  f <- as.formula(paste(i, "treatment", sep = " ~ "))
  after_stat[[i]] <- summary(lm(f, temp))
}

capture.output(after_stat) %>% write("results/forest_after_compare.txt") 

## Make figure with estimates, std. errors and signifcances:

## Extract coefficients and create anova table:
ba_coeff <- lapply(after_stat, "[[", "coefficients")
anova.create.2 <- function(x){
  x[2, 1] <- x[2, 1] + x[1, 1]
  return(x)
}
ba_coeff <- lapply(ba_coeff, anova.create.2)

## Make graphing data frame and rename levels:
gg_data_2 <- melt.list(ba_coeff)
gg_data_2 <- dcast(gg_data_2, X1 + L1 ~ X2, value.var = "value")
levels(gg_data_2$X1) <- c("Conventional thinning", 
                          "Understory retention thinning")
gg_data_2$exp <- "After"

## 5. Make a table and figure --------------------------------------------------

gg_data <- rbind(gg_data_1, gg_data_2)

## Make table:

table_data <- gg_data
table_data$est_CI <- paste0(round(table_data$Estimate, 2), " (",
                            round(table_data$`Std. Error`*1.96, 2), ")")

dcast(table_data, exp + X1 ~ L1, value.var = "est_CI") %>%
write.csv(., "results/forest_var_table.csv", row.names = FALSE)

## Make graph:

gg_data$L1 <- as.factor(gg_data$L1)
levels(gg_data$L1) <- c("Basal area (BA)", 
                        "BA dead wood", 
                        "BA spruce", 
                        "BA deciduous", 
                        "BA pine", 
                        "Visibility (m)", 
                        "Nr. umbr. spruce")
colnames(gg_data)[5] <- "Std.Error"

## Adjust level order:
gg_data$X1 <- factor(gg_data$X1, 
                       levels = c("Control",
                                  "Complete retention", 
                                  "Understory retention thinning",
                                  "Conventional thinning"))
gg_data$exp <- factor(gg_data$exp, levels = c("Before", "After"))

## Make graph:
G1 <- ggplot(gg_data, aes(x = X1, y = Estimate, color = X1)) +
  geom_errorbar(aes(ymin = Estimate - 1.96*Std.Error, 
                    ymax = Estimate + 1.96*Std.Error),
                size = 5, 
                width = 0) +
  geom_point(size = 7, color = "black") +
  facet_grid(L1 ~ exp, scales = "free", space = "free_x") +
  xlab("") + ylab("") +
  scale_colour_manual(values = c("grey", "#00AFBB", "#E7B800", "#FC4E07")) +
  theme_light(30) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

png("figures/forest_var.png", 4000/8, 15000/8, "px", res = 600/8)
G1
dev.off()

## -------------------------------END-------------------------------------------
