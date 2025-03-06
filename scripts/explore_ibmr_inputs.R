#####
# This script explores IBMR inputs to help explain results
# Created by Catarina Pien

library(tidyverse)

setwd(here::here())
input_data_path <- here::here("data/data_processed")
output_path <- here::here("output/model_outputs/")
plot_path <- here::here("output/figures/")

## X2 ------------------------------------------------
x2 <- read_csv(here(input_data_path, "IBMR_X2_SF2025_input.csv"))
zoop <- read_csv(here(input_data_path, "zoop_scalar_output_SF2025_2025-03-04.csv"))
lambda <- read_csv(here(output_path, "mean_lambda_all_alts_long.csv"))

x2_long <- x2 %>%
  pivot_longer(cols = 3:14, values_to = "x2", names_to = "month") %>%
  mutate(month = as.numeric(month)) %>%
  mutate(scenario = forcats::fct_relevel(scenario,  c("StatusQuo", "MaxDS_Even","MaxDS_Hist", "SummerFall_Even", "Summer_Even", "Summer_Even_AltSMSCG",
                                                       "SummerFall_Hist", "Summer_Hist", "June", "MaxWater", "MaxWater_noSMSCG"))) 
x2_summerfall <- x2_long %>% filter(month > 5 & month<11)


# Compare summer X2s
(summer_x2 <- ggplot() + 
  geom_boxplot(data= x2_summerfall %>% filter(month<9), aes(x = scenario, y = x2))+ 
  facet_wrap(~month, nrow = 3) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
        axis.title.x = element_blank()))

# Compare summer/Fall X2
(summer_fall_x2 <- ggplot() + 
  geom_boxplot(data= x2_summerfall, aes(x = scenario, y = x2))+ 
  facet_wrap(~month, nrow = 3) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
        axis.title.x = element_blank()))

# Line plot 

(summer_fall_x2_line_plot <- ggplot() +
    geom_point(data = x2_summerfall, aes(x = year, y = x2, color = scenario))+
    geom_line(data = x2_summerfall, aes(x = year, y = x2, color = scenario))+
    facet_wrap(~month, nrow = 5) + 
    theme_bw())

# Interactive plot
plotly::ggplotly(summer_fall_x2_line_plot)

# Tile plot
(summer_fall_x2_tile <- ggplot() +
    geom_tile(data = x2_summerfall, aes(x = year, y = scenario, fill = x2), color = "black")+
    facet_wrap(~month)+
    viridis::scale_fill_viridis() + 
    theme_bw())

# w_an <- c()

### Write plots ------------------------------------
png(filename = file.path(plot_path, "X2_summer.png"), width = 5, height = 8, units = "in", res = 300)
summer_x2
dev.off()

png(filename = file.path(plot_path, "X2_summerfall.png"), width = 6, height = 8, units = "in", res = 300)
summer_fall_x2
dev.off()
