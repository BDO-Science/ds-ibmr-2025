# plot_abundance_lambda.R
# Plot lambda and abundance based off summarized file created in read_minimum_abundance_outputs.R
# Created by Catarina Pien on 3/13/2025

# Packages
library(stringr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(here)
library(map)

# Filepaths
output_path <- here::here("output/model_outputs/")
plot_path <- here::here("output/figures/")
output1 <- read_csv(here(output_path, "summarized_output/abundance_meanmin_all_alts_AdjHist.csv"))
output2 <- read_csv(here(output_path, "summarized_output/abundance_meanmin_all_alts_2022MED.csv"))

# Plot

## abundance----------------------

### AdjHist ---------------------------
(plot_abund1 <- ggplot(output1) + 
    geom_point(aes(x =Alternatives, y = MeanMinAbundance), size = 3, shape = 17, color = "navy")+
    geom_hline(yintercept = 500000, linetype = "dashed") + 
    geom_label(aes(x = Alternatives, y = MeanMinAbundance + 200000, label = round(MeanMinAbundance)), size = 3.5) + 
    labs(y = "Mean Minimum Abundance")+ 
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank()))

png(filename = file.path(plot_path, "abundance_AdjHist.png"), width = 7.5, height = 6, units = "in", res = 300)
plot_abund1
dev.off()  

### 2022MED -----------------------------------------
(plot_abund2 <- ggplot(output2) + 
    geom_point(aes(x =Alternatives, y = MeanMinAbundance), size = 4, shape = 18, color = "steelblue4")+
    geom_hline(yintercept = 500000, linetype = "dashed") + 
    geom_label(aes(x = Alternatives, y = MeanMinAbundance + 200000, label = round(MeanMinAbundance)), size = 3.5) + 
    labs(y = "Mean Minimum Abundance")+ 
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank()))

png(filename = file.path(plot_path, "abundance_2022MED.png"), width = 7.5, height = 6, units = "in", res = 300)
plot_abund2
dev.off()  

## lambda ------------------------------

### AdjHist----------------------
(plot_lambda1 <- ggplot(output1) + 
    geom_point(aes(x =Alternatives, y = lambda), size = 3, shape = 17, color = "navy")+
    geom_hline(yintercept = 1, linetype = "dashed") + 
    geom_label(aes(x = Alternatives, y = lambda+ 0.01, label = round(lambda,3)), size = 3.5) + 
    labs(y = "Lambda")+ 
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank()))

png(filename = file.path(plot_path, "lambda_AdjHist.png"), width = 7, height = 6, units = "in", res = 300)
plot_lambda1
dev.off()  

###2022MED------------------------
(plot_lambda2 <- ggplot(output2) + 
    geom_point(aes(x =Alternatives, y = lambda), size = 4, shape = 18, color = "steelblue4")+
    geom_hline(yintercept = 1, linetype = "dashed") + 
   geom_label(aes(x = Alternatives, y = lambda+ 0.02, label = round(lambda,3)), size = 3.5) + 
    labs(y = "Lambda")+ 
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank()))

png(filename = file.path(plot_path, "lambda_2022MED.png"), width = 7, height = 6, units = "in", res = 300)
plot_lambda2
dev.off()
