##########
####read_lambda_outputs.R####
# Read in files of relevance to gather all lambda values in a data frame.
# Created by Catarina Pien 

library(stringr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(here)
library(purrr)

# output_path <- here::here("output/model_outputs/outputs_AdjHist")
output_path <- here::here("output/model_outputs/outputs_2022MED/")
plot_path <- here::here("output/figures/")

# Read all files and filter to lambda mean files
fp_lam <- dir(here(output_path), full.names = TRUE)
fp_lam2 <- data.frame(fp_lam) %>%
  filter(grepl("lamABmn", fp_lam))

fp_lam_char <- as.character(fp_lam2$fp_lam)
ls_lam <- map(fp_lam_char, read.table)

# Tidy up the data into a data frame
df_lam <- list_cbind(ls_lam)
# altnums <- c("alt1", "alt10", "alt11", "alt12", "alt2", "alt4", "alt5", "alt6", "alt7", "alt8", "alt9")
cols <- c("StatusQuo", "June", "MaxWater", "MaxWater_noSMSCG", "MaxDS_Even",
          "MaxDS_Hist", "SummerFall_Even", "Summer_Even", "Summer_Even_AltSMSCG",
          "SummerFall_Hist", "Summer_Hist")
colnames(df_lam) <- cols
names <- data.frame(description = c("mean_all_years", "mean_2007_2014", "mean_2005_2014", "mean_1995_2006", "mean_AN_W", "mean_D_C", "mean_1997_2014", "L95CL", "U95CL")) 
lammn_df <- cbind(names, df_lam)

# Write file
# write_csv(lammn_df, here(output_path, "../summarized_output/mean_lambda_all_alts_2022MED.csv"))

# Make a long version of the data for plotting
long <- pivot_longer(lammn_df, cols = StatusQuo:Summer_Hist, names_to = "alt", values_to = "lambdaval") %>%
  mutate(alt = as.factor(alt),
           # alt = forcats::fct_relevel(alt,  c("alt1", "alt2", "alt4", "alt5", "alt6", "alt7", "alt8", "alt9", "alt10", "alt11", "alt12"))) %>%
         alt = forcats::fct_relevel(alt,  c("StatusQuo", "MaxDS_Even","MaxDS_Hist", "SummerFall_Even", "Summer_Even", "Summer_Even_AltSMSCG",
                                            "SummerFall_Hist", "Summer_Hist", "June", "MaxWater", "MaxWater_noSMSCG"))) 

# write_csv(long, here(output_path, "../summarized_output/mean_lambda_all_alts_long_2022MED.csv"))


# Plot lambda
(lambda_plot <- ggplot() + 
    geom_point(data = long %>%
                 filter(description =="mean_all_years"), aes(x = alt, y = lambdaval), size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    labs(y = "Mean Lambda")+ 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank()))

png(filename = file.path(plot_path, "lambda_2022MED.png"), width = 6, height = 6, units = "in", res = 300)
lambda_plot
dev.off()

# Plot lambda two ways
(lambda_facet_plot <- ggplot() + 
    geom_point(data = long %>%
                 filter(description %in% c("mean_all_years", "mean_AN_W")), aes(x = alt, y = lambdaval), size = 2) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    facet_wrap(~description, nrow = 2, scales = "free_y") + 
    labs(y = "Mean Lambda")+ 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          axis.title.x = element_blank()))

png(filename = file.path(plot_path, "lambda_all_anwet.png"), width = 6, height = 8, units = "in", res = 300)
lambda_facet_plot
dev.off()

# Plot with confidence limits
cls <- long %>% filter(description %in% c("mean_all_years", "L95CL", "U95CL")) %>%
  pivot_wider(names_from = "description", values_from = "lambdaval")

ggplot() +
  geom_point(data = cls, aes(x = alt, y = mean_all_years), size = 3) +
  geom_errorbar(data = cls, aes(x = alt, ymin = L95CL, ymax = U95CL ))+
  labs(y = "Mean Lambda")+ 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
        axis.title.x = element_blank())
