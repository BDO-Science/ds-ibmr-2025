library(stringr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(here)
library(purrr)

rm(list=ls(all=TRUE))

# change this depending on which output used
output_path <- here::here("output/model_outputs/outputs_nonflow_actions")

# Read all files and filter to rds files
fp_abund <- dir(here(output_path), full.names = TRUE)
fp_abund2 <- data.frame(fp_abund) %>%
  filter(grepl("alt_", fp_abund))

fp_abund_char <- as.character(fp_abund2$fp_abund)
# List of rds output
ls_abund <- map(fp_abund_char, readRDS)

# Sort by alternatives
fp_abund_char
names(ls_abund) <- c("MaxDS_Even_NF_AquaticWeed_2022MED", "MaxDS_Even_NF_AquaticWeed_adjHist", "MaxWater_NF_AquaticWeed_2022MED","MaxWater_NF_AquaticWeed_adjHist")

# Calculate super - based off original ibmr input and should be standard across alts.
outz <- readRDS(file.path(output_path, "../original_ibmr_output.rds"))
input_path <- here::here("data/data_raw/demo_inputs")
FWS.abundance<-read.table(file.path(input_path,'FWS.abundance_LCME.txt'),header=F)
FWS.abundance<-cbind(FWS.abundance[,2],FWS.abundance[,3],FWS.abundance[,4],FWS.abundance[,5])
super<-median(c(apply(outz[1:20,1,],1,median,na.rm=T)/FWS.abundance[1:20,1], # get ratio of simulated abundance to LCME-estimated abundance
                apply(outz[1:20,2,],1,median,na.rm=T)/FWS.abundance[1:20,2],
                apply(outz[1:20,3,],1,median,na.rm=T)/FWS.abundance[1:20,3],
                apply(outz[1:20,4,],1,median,na.rm=T)/FWS.abundance[1:20,4]))

# Define a function to get the minimum from a specific column
get_column_minimum <- function(matrix_data) {
  # Initialize a vector to store minimum values
  min_values <- numeric(nrow(matrix_data))
  
  # Loop through each row of the matrix
  for (i in 1:dim(matrix_data)[3]) {
    # Get the specific vector (column)
    vector <- matrix_data[(1:nrow(matrix_data)),4,i]
    
    # Store the minimum value in the min_values vector
    min_values[i] <- min(vector)/super[1]
  }
  
  return(min_values)
}

# Grab minimum across simulations
min_abundance_table <- data.frame(Alternatives=
                                    c("MaxDS_Even_NF_AquaticWeed_2022MED", "MaxDS_Even_NF_AquaticWeed_adjHist", "MaxWater_NF_AquaticWeed_2022MED","MaxWater_NF_AquaticWeed_adjHist")
                                  ,MeanMinAbundance= c(
                                    mean(get_column_minimum(ls_abund[["MaxDS_Even_NF_AquaticWeed_2022MED"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["MaxDS_Even_NF_AquaticWeed_adjHist"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["MaxWater_NF_AquaticWeed_2022MED"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["MaxWater_NF_AquaticWeed_adjHist"]]),na.rm=T)),
                                  MinAbundance_05= c(quantile(get_column_minimum(ls_abund[["MaxDS_Even_NF_AquaticWeed_2022MED"]]), 0.05, na.rm = TRUE),
                                                     quantile(get_column_minimum(ls_abund[["MaxDS_Even_NF_AquaticWeed_adjHist"]]), 0.05, na.rm = TRUE),
                                                     quantile(get_column_minimum(ls_abund[["MaxWater_NF_AquaticWeed_2022MED"]]), 0.05, na.rm = TRUE),
                                                     quantile(get_column_minimum(ls_abund[["MaxWater_NF_AquaticWeed_adjHist"]]), 0.05, na.rm = TRUE)),
                                  MaxAbundance_95= c(quantile(get_column_minimum(ls_abund[["MaxDS_Even_NF_AquaticWeed_2022MED"]]), 0.95, na.rm = TRUE),
                                                     quantile(get_column_minimum(ls_abund[["MaxDS_Even_NF_AquaticWeed_adjHist"]]), 0.95, na.rm = TRUE),
                                                     quantile(get_column_minimum(ls_abund[["MaxWater_NF_AquaticWeed_2022MED"]]), 0.95, na.rm = TRUE),
                                                     quantile(get_column_minimum(ls_abund[["MaxWater_NF_AquaticWeed_adjHist"]]), 0.95, na.rm = TRUE)))



####
write_csv(min_abundance_table, file.path(output_path, "../summarized_output/abundance_meanmin_all_nonflowalts.csv"))





