library(dplyr)
library(ggplot2)
library(here)

alt <-  "alt1_StatusQuo"
alt <-  "alt2_MaxDS_Even"
alt <- "alt4_MaxDS_Hist"
alt <- "alt5_SummerFall_Even_Hist"
alt <- "alt6_Summer_Even"
alt <- "alt7_Summer_Even_AltSMSCG"
alt <- "alt8_SummerFall_Hist"
alt <- "alt9_Summer_Hist"
alt <- "alt10_June"
alt <- "alt11_MaxWater"
alt <- "alt12_MaxWater_noSMSCG"

output <- readRDS(here(paste0("output/model_outputs/", alt, "_output.rds")))
lambda<-read.table(here(paste0("output/model_outputs/", alt, "_lamAB.txt")),header=T)
lambdamn2<-read.table(here(paste0("output/model_outputs/", alt, "_lamABmn.txt")),header=T)
abundance <- read.csv(here(paste0("output/model_outputs/", alt, "_abundance.csv")))
# defined in number 1 of get summaries

# lambda file
lambda_df <- as.data.frame(lambda) %>%
  cbind(data.frame(year = 1996:2014))

# lambda mean file
names <- data.frame(description = c("mean_all_years", "mean_2007_2014", "mean_2005_2014", "mean_1995_2006", "mean_AN_W", "mean_D_C", "mean_1997_2014", "L95CL", "U95CL")) 
lam.mn.df <- data.frame(lambdamn)
lammn_df <- cbind(names, lam.mn.df)

ggplot(lammn_df) + geom_col(aes(x = description, y = lam.mn)) +
  theme(axis.text.x = element_text(angle  = 90))

# plot lambda
ggplot(lambda_df) +
  geom_line(aes(year, mean), color = "magenta4") +
  geom_line(aes(year, min), color = "magenta3", linetype = "dotted") +
  geom_line(aes(year, max), color = "magenta3", linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(y = "Mean lambda")+
  theme_bw()
