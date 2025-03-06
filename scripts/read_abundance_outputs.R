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
df_abund <- list_cbind(ls_abund)
# altnums <- c("alt1", "alt10", "alt11", "alt12", "alt2", "alt4", "alt5", "alt6", "alt7", "alt8", "alt9")
cols <- c("StatusQuo", "June", "MaxWater", "MaxWater_noSMSCG", "MaxDS_Even",
          "MaxDS_Hist", "SummerFall_Even", "Summer_Even", "Summer_Even_AltSMSCG",
          "SummerFall_Hist", "Summer_Hist")
colnames(df_lam) <- cols
names <- data.frame(description = c("mean_all_years", "mean_2007_2014", "mean_2005_2014", "mean_1995_2006", "mean_AN_W", "mean_D_C", "mean_1997_2014", "L95CL", "U95CL")) 
lammn_df <- cbind(names, df_lam)