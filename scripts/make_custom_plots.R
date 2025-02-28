library(dplyr)
library(ggplot2)
library(here)

output <- readRDS(here("output/model_outputs/alt1_status-quo_output.rds"))
lambda<-read.table(here("output/model_outputs/alt1_status-quo_lamAB.txt"),header=T)
lambdamn<-read.table(here("output/model_outputs/alt1_status-quo_lamABmn.txt"),header=T)
# defined in number 1 of get summaries

lambda_df <- as.data.frame(lambda) %>%
  cbind(data.frame(year = 1996:2014))

ggplot(lambda_df) +
  geom_line(aes(year, mean), color = "magenta4") +
  geom_line(aes(year, min), color = "magenta3", linetype = "dotted") +
  geom_line(aes(year, max), color = "magenta3", linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(y = "Mean lambda")+
  theme_bw()
