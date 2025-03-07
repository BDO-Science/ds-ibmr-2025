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

write_csv(df_abund, file.path(output_path, "summarized_output/abundance_all_alts.csv"))

# Plot

abundance_sum <- df_abund %>%
  group_by(alt) %>%
  summarize(meanmin = mean(q0.05)) %>%
  mutate(alt = forcats::fct_relevel(alt,  c("StatusQuo", "MaxDS_Even","MaxDS_Hist", "SummerFall_Even", "Summer_Even", "Summer_Even_AltSMSCG",
                                     "SummerFall_Hist", "Summer_Hist", "June", "MaxWater", "MaxWater_noSMSCG"))) 

(plot_0.05abund <- ggplot(abundance_sum) + 
  geom_point(aes(x =alt, y = meanmin), size = 3, shape = 2)+
  labs(y = "Mean q0.05 Abundance")+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank()))

png(filename = file.path(plot_path, "abundance.png"), width = 6, height = 6, units = "in", res = 300)
plot_0.05abund
dev.off()  
