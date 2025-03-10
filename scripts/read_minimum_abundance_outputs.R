library(stringr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)


output_path <- here::here("output/model_outputs/")

# Read all files and filter to rds files
fp_abund <- dir(here(output_path), full.names = TRUE)
fp_abund2 <- data.frame(fp_abund) %>%
  filter(grepl("SMSCGfixed.rds", fp_abund))

fp_abund_char <- as.character(fp_abund2$fp_abund)
# List of rds output
ls_abund <- map(fp_abund_char, readRDS)

# Sort by alternatives
fp_abund_char
names(ls_abund) <- c("StatusQuo", "June", "MaxWater","MaxWater_noSMSCG","MaxDS_Even","MaxDS_Hist",
                     "SummerFall_Even","Summer_Even","Summer_Even_AltSMSCG","SummerFall_Hist","Summer_Hist")

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
outz_StatusQuo<- get_column_minimum(ls_abund[["StatusQuo"]])
min_abundance_table <- data.frame(Alternatives=
                                    c("StatusQuo", "June", "MaxWater","MaxWater_noSMSCG","MaxDS_Even","MaxDS_Hist",
                                      "SummerFall_Even","Summer_Even","Summer_Even_AltSMSCG",
                                      "SummerFall_Hist","Summer_Hist")
                                  ,MeanMinAbundance= c(
                                    mean(get_column_minimum(ls_abund[["StatusQuo"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["June"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["MaxWater"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["MaxWater_noSMSCG"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["MaxDS_Even"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["MaxDS_Hist"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["SummerFall_Even"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["Summer_Even"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["Summer_Even_AltSMSCG"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["SummerFall_Hist"]]),na.rm=T),
                                    mean(get_column_minimum(ls_abund[["Summer_Hist"]]),na.rm=T)
                                    ))


# Export out results as csv
write_csv(min_abundance_table, file.path(output_path, "summarized_output/abundance_meanmin_all_alts.csv"))


####
# Check correlation with lambda
min_abundance_table$lambda <- c(0.985157396 ,0.946904929,0.960571257,0.958925921791839,
                                1.04458429926919, 1.034668792,0.953879698,0.966400795,0.967343145,
                                0.945440517, 0.958756902)

summary(lm(min_abundance_table$lambda~min_abundance_table$MeanMinAbundance))





##############
# Excess abundance calcs
adult_median<- apply(outz[(1:nrow(outz)),4,],1,median,na.rm=T)/super[1]
adult_minimum <- apply(outz[(1:nrow(outz)),4,],1,min,na.rm=T)/super[1]
adult_minimum_list<- outz[(1:nrow(outz)),4,1]

outz[(1:nrow(outz)),4,1]/super[1]
outz[(1:nrow(outz)),4,330]/super[1]


outz[(1:nrow(outz)),4,330]

dim(outz)[3]


