#####
# This script explores IBMR inputs to help explain results
# Created by Catarina Pien

library(tidyverse)

setwd(here::here())
input_data_path <- here::here("data/data_processed")
output_path <- here::here("output/model_outputs/")
plot_path <- here::here("output/figures/")



## Combine files -----------------------

x2a <- read_csv(here(input_data_path, "IBMR_X2_SF2025_input.csv")) %>%
  mutate(hydro = "AdjHist")
zoopa <- read_csv(here(input_data_path, "zoop_scalar_output_SF2025_2025-03-13.csv"))%>%
  mutate(hydro = "AdjHist")
x2b <- read_csv(here(input_data_path, "IBMR_X2_SF2022MED_input.csv"))%>%
  mutate(hydro = "2022MED")
zoopb <- read_csv(here(input_data_path, "zoop_scalar_output_SF2022MED_2025-03-13.csv"))%>%
  mutate(hydro = "2022MED")
wy <- read_csv(here(input_data_path, "wytype_may.csv")) %>%
  mutate(year = WY) %>% select(-WY)
outputa <- read_csv(here(output_path, "summarized_output/abundance_meanmin_all_alts_AdjHist.csv"))%>%
  mutate(hydro = "AdjHist")
outputb <- read_csv(here(output_path, "summarized_output/abundance_meanmin_all_alts_2022MED.csv"))%>%
  mutate(hydro = "2022MED")

#X2
x2 <- bind_rows(x2a, x2b)

x2_long <- x2 %>%
  pivot_longer(cols = 3:14, values_to = "x2", names_to = "month") %>%
  mutate(month = as.numeric(month)) %>%
  left_join(wy) %>%
  mutate(actionyr = if_else(wytype %in% c("AN", "W"), "Y", "N")) %>%
  mutate(scenario = forcats::fct_relevel(scenario,  c("StatusQuo", "MaxDS_Even","MaxDS_Hist", "SummerFall_Even", "Summer_Even", "Summer_Even_AltSMSCG",
                                                      "SummerFall_Hist", "Summer_Hist", "June", "MaxWater", "MaxWater_noSMSCG"))) 
x2_summerfall <- x2_long %>% filter(month > 5 & month<11)

# zoop
zoop <- bind_rows(zoopa, zoopb)
zoop_long <- zoop %>%
  pivot_longer(cols = starts_with("sal"),
               names_to = "alt",
               values_to = "zoop") %>%
  mutate(alt = str_remove(alt, "_median"),
         alt = str_remove(alt, "sal_")) %>%
  left_join(wy)

# output (lambda and abundance)
output <- bind_rows(outputa, outputb)


## DWR Hydro -------------------------------
# Compare summer X2s
(summer_x2 <- ggplot() + 
  geom_boxplot(data= x2_summerfall %>% filter(month<9, hydro == "AdjHist"), aes(x = scenario, y = x2))+ 
  facet_wrap(~month, nrow = 5) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
        axis.title.x = element_blank()))

# Compare summer/Fall X2
(summer_fall_x2 <- ggplot() + 
  geom_boxplot(data= x2_summerfall %>%filter(hydro=="AdjHist"), aes(x = scenario, y = x2, fill = actionyr))+ 
  facet_wrap(~month, nrow = 5) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
        axis.title.x = element_blank()))

png(filename = file.path(plot_path, "x2_boxplot_AdjHist.png"), width = 8, height = 8, units = "in", res = 300)
summer_fall_x2
dev.off() 

(summer_fall_x2 <- ggplot() + 
    geom_violin(data= x2_summerfall, aes(x = scenario, y = x2, fill = actionyr))+ 
    facet_wrap(~month, nrow = 5) + 
    theme_bw()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
          axis.title.x = element_blank()))



# Line plot 
x2_summerfall_a <- x2_summerfall %>% filter(hydro == "AdjHist")

(summer_fall_x2_line_plot <- ggplot() +
    geom_point(data = x2_summerfall_a, aes(x = year, y = x2, color = scenario, shape = scenario, size = actionyr))+
    geom_line(data = x2_summerfall_a, aes(x = year, y = x2, color = scenario))+
    geom_text(data = x2_summerfall_a, aes(x = year, y = 95, label = wytype))+
    scale_shape_manual(values = c(20, 0, 18, 6, 8, 9, 10, 23, 1, 13, 14))+
    viridis::scale_color_viridis(discrete = TRUE, option = "turbo") + 
    scale_x_continuous(breaks = seq(1995, 2014, 1))+ 
    scale_y_continuous(breaks = seq(55, 95, 10)) + 
    facet_wrap(~month, nrow = 5) + 
    labs(y = "X2 (km)")+
    theme_bw() +
    theme(axis.text  = element_text(size = 11),
          axis.text.x = element_text(angle = 90))) 

# Interactive plot
plotly::ggplotly(summer_fall_x2_line_plot)

# Tile plot
(summer_fall_x2_tile <- ggplot() +
    geom_tile(data = x2_summerfall_a, aes(x = year, y = scenario, fill = x2), color = "black")+
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

png(filename = file.path(plot_path, "x2_violplot_AdjHist.png"), width = 8, height = 8, units = "in", res = 300)
summer_fall_x2
dev.off() 

png(filename = file.path(plot_path, "x2_trends_AdjHist.png"), width = 8, height = 8, units = "in", res = 300)
summer_fall_x2_line_plot
dev.off() 

## 2022MED hydro --------------------------
### X2 ------------------------------------------------
x2_summerfall_b <- x2_summerfall %>% filter(hydro == "2022MED")

# Compare summer/Fall X2
(summer_fall_x2_2022MED <- ggplot() + 
    geom_boxplot(data= x2_summerfall_b, aes(x = scenario, y = x2, fill = actionyr))+ 
    facet_wrap(~month, nrow = 5) + 
    theme_bw()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
          axis.title.x = element_blank()))

png(filename = file.path(plot_path, "x2_boxplot_2022MED.png"), width = 8, height = 8, units = "in", res = 300)
summer_fall_x2_2022MED
dev.off() 

(summer_fall_x2_2022MED <- ggplot() + 
    geom_violin(data= x2_summerfall_b, aes(x = scenario, y = x2, fill = actionyr))+ 
    facet_wrap(~month, nrow = 5) + 
    theme_bw()+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 0.99),
          axis.title.x = element_blank()))

png(filename = file.path(plot_path, "x2_violplot_2022MED.png"), width = 8, height = 8, units = "in", res = 300)
summer_fall_x2_2022MED
dev.off() 
# Line plot 

(summer_fall_x2_line_plot_2022MED <- ggplot() +
    geom_point(data = x2_summerfall_b, aes(x = year, y = x2, color = scenario, shape = scenario, size = actionyr))+
    geom_line(data = x2_summerfall_b, aes(x = year, y = x2, color = scenario))+
    geom_text(data = x2_summerfall_b, aes(x = year, y = 95, label = wytype))+
    scale_shape_manual(values = c(20, 0, 18, 6, 8, 9, 10, 23, 1, 13, 14))+
    viridis::scale_color_viridis(discrete = TRUE, option = "turbo") + 
    scale_x_continuous(breaks = seq(1995, 2014, 1))+ 
    scale_y_continuous(breaks = seq(55, 95, 10)) + 
    facet_wrap(~month, nrow = 5) + 
    labs(y = "X2 (km)")+
    theme_bw() +
    theme(axis.text  = element_text(size = 11),
      axis.text.x = element_text(angle = 90))) 

png(filename = file.path(plot_path, "x2_trends_2022MED.png"), width = 8, height = 8, units = "in", res = 300)
summer_fall_x2_line_plot_2022MED
dev.off() 

### zooplankton -------------
zoop_summ <- zoop_long %>%
  group_by(year, IBMR, hydro, alt, wytype) %>% 
  summarize(mnzoop = mean(zoop, na.rm = TRUE),
            sumzoop = sum(zoop, na.rm = TRUE)) %>%
  mutate(actionyr = if_else(wytype %in% c("AN", "W"), "*", "")) %>%
  filter(alt !="base")

#### with 1997 ------------------------
(barplot_zoop_adjhist <- ggplot(zoop_summ %>%filter(hydro == "AdjHist")) +
  geom_col(aes(x = factor(year), y = sumzoop, fill = IBMR)) +
  geom_text(aes(x = factor(year), y = sumzoop + 5, label = actionyr))+
  labs(title = "AdjHist")+
  viridis::scale_fill_viridis(option = "turbo", discrete = TRUE)+
  facet_wrap(~alt)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)))

(barplot_zoop_2022MED <- ggplot(zoop_summ %>%filter(hydro == "2022MED")) +
  geom_col(aes(x = factor(year), y = sumzoop, fill = IBMR)) +
  geom_text(aes(x = factor(year), y = sumzoop + 5, label = actionyr))+
  facet_wrap(~alt)+
  labs(title = "2022MED")+
  viridis::scale_fill_viridis(option = "turbo", discrete = TRUE)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)))

png(filename = file.path(plot_path, "barplot_sumzoop_AdjHist.png"), width = 12, height = 8, units = "in", res = 300)
barplot_zoop_adjhist
dev.off() 

png(filename = file.path(plot_path, "barplot_sumzoop_2022MED.png"), width = 12, height = 8, units = "in", res = 300)
barplot_zoop_2022MED
dev.off() 


#### without 1997 ------------------------

(barplot_zoop_adjhist2 <- ggplot(zoop_summ %>%filter(hydro == "AdjHist", year != 1997)) +
   geom_col(aes(x = factor(year), y = sumzoop, fill = IBMR)) +
   geom_text(aes(x = factor(year), y = sumzoop + 5, label = actionyr))+
   labs(title = "AdjHist no 1997")+
   viridis::scale_fill_viridis(option = "turbo", discrete = TRUE)+
   facet_wrap(~alt)+
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90)))

(barplot_zoop_2022MED2 <- ggplot(zoop_summ %>%filter(hydro == "2022MED", year !=1997)) +
    geom_col(aes(x = factor(year), y = sumzoop, fill = IBMR)) +
    geom_text(aes(x = factor(year), y = sumzoop + 5, label = actionyr))+
    facet_wrap(~alt)+
    labs(title = "2022MED no 1997")+
    viridis::scale_fill_viridis(option = "turbo", discrete = TRUE)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)))

png(filename = file.path(plot_path, "barplot_sumzoop_AdjHist_no1997.png"), width = 12, height = 8, units = "in", res = 300)
barplot_zoop_adjhist2
dev.off() 

png(filename = file.path(plot_path, "barplot_sumzoop_2022MED_no1997.png"), width = 12, height = 8, units = "in", res = 300)
barplot_zoop_2022MED2
dev.off() 







zoop_x2 <- left_join(zoop_long %>%
                       select(scenario = alt, year, month, IBMR, hydro, zoop), x2_long)

ggplot(zoop_x2 %>%
         filter(actionyr=="Y")) + 
  geom_point(aes(x2, zoop, color = IBMR)) 
