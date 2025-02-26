library(dplyr)
library(ggplot2)
library(here)

output <- readRDS(here("IBMR/Output/outz_FirstFlush.rds"))
lambda<-read.table(here("IBMR/Output/lamAB_FirstFlush.txt"),header=T)
lambdamn<-read.table(here('IBMR/Output/lamABmn_FirstFlush.txt'),header=T)
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

# Questions

# Walk through output rds parameters
# What is lambdamn (what are the 9 rows?)
# Are 1996-2014 the right years for lambda?
# What are we getting from CalSim?
# Adjust parameters in Action effects
# Parallel processing might speed up the modeling
