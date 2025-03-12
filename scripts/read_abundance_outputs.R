library(stringr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

output_path <- here::here("output/model_outputs/")
plot_path <- here::here("output/figures/")

# Read all files and filter to lambda mean files
fp_abund <- dir(here(output_path), full.names = TRUE)
fp_abund2 <- data.frame(fp_abund) %>%
  filter(grepl("abundance", fp_abund))

fp_abund_char <- as.character(fp_abund2$fp_abund)
ls_abund <- map(fp_abund_char, read_csv)
  

# Tidy up the data into a data frame
df_abund0 <- list_rbind(ls_abund)
alts <- c(rep("StatusQuo",19), rep("June",19), rep("MaxWater",19), rep("MaxWater_noSMSCG",19), rep("MaxDS_Even",19),
          rep("MaxDS_Hist",19), rep("SummerFall_Even",19), rep("Summer_Even",19), rep("Summer_Even_AltSMSCG",19),
          rep("SummerFall_Hist",19), rep("Summer_Hist",19))
df_abund <- cbind(df_abund0, alts) %>%
  select(-1)
cols <- c("mean", "min", "max", "q0.025","q0.05", "q0.25","q0.5", "q0.75", "q0.9", "q0.975", "year", "alt")
colnames(df_abund) <- cols

write_csv(df_abund, file.path(output_path, "../summarized_output/abundance_all_alts.csv"))

# Plot
abundance1 <- read_csv(here(output_path, "../summarized_output/abundance_meanmin_all_alts.csv"))

(plot_abund1 <- ggplot(abundance1) + 
  geom_point(aes(x =Alternatives, y = MeanMinAbundance), size = 3, shape = 17, color = "navy")+
  labs(y = "Mean Minimum Abundance")+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank()))

png(filename = file.path(plot_path, "abundance_AdjHist.png"), width = 6, height = 6, units = "in", res = 300)
plot_abund1
dev.off()  




abundance2 <- read_csv(here(output_path, "../summarized_output/abundance_meanmin_all_alts_2022MED.csv"))

(plot_abund2 <- ggplot(abundance2) + 
    geom_point(aes(x =Alternatives, y = MeanMinAbundance), size = 4, shape = 18, color = "steelblue4")+
    labs(y = "Mean Minimum Abundance")+ 
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank()))

png(filename = file.path(plot_path, "abundance_2022MED.png"), width = 6, height = 6, units = "in", res = 300)
plot_abund2
dev.off()  
