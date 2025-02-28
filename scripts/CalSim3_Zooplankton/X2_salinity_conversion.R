#Script to convert X2 to salinity data for IBMR regions
root <- "~/GitHub/ds-ibmr-2025"
setwd(root)

path_hydro <- file.path(root,"scripts","CalSim3_Zooplankton")

library(reshape)
library(tidyverse)
library(stringr)
library(lubridate)
library(conflicted)
library(wql)
conflict_prefer("rename", "dplyr")

# Load final salinity-X2 model
salX2mod <- readRDS(file.path(path_hydro,"model_sal_X2.Rdata")) 

# Load X2 data from CalSim3
AltStatusQuo_data <- read.csv(file.path(path_hydro,"CalSim3_data_StatusQuo.csv")) 
AltJune_data <- read.csv(file.path(path_hydro,"CalSim3_data_June.csv")) 
AltMaxDS_Even_data <- read.csv(file.path(path_hydro,"CalSim3_data_MaxDS_Even.csv")) 
AltMaxDS_Hist_data <- read.csv(file.path(path_hydro,"CalSim3_data_MaxDS_Hist.csv")) 


# Combine X2 data
x2_data <- AltStatusQuo_data %>% select(Date,X2_current) %>% mutate(Scenario="StatusQuo") %>%
  bind_rows((AltJune_data %>% select(Date,X2_current) %>% mutate(Scenario="June"))) %>%
  bind_rows((AltMaxDS_Even_data %>% select(Date,X2_current) %>% mutate(Scenario="MaxDS_Even"))) %>%
  bind_rows((AltMaxDS_Hist_data %>% select(Date,X2_current) %>% mutate(Scenario="MaxDS_Hist")))

#####
# Load Montezuma slough salinity data from CalSim
MTZ_data <- AltStatusQuo_data %>% select(Date,MTZ_EC_current) %>% mutate(Scenario="StatusQuo") %>%
  bind_rows((AltJune_data %>% select(Date,MTZ_EC_current) %>% mutate(Scenario="June"))) %>%
  bind_rows((AltMaxDS_Even_data %>% select(Date,MTZ_EC_current) %>% mutate(Scenario="MaxDS_Even"))) %>%
  bind_rows((AltMaxDS_Hist_data %>% select(Date,MTZ_EC_current) %>% mutate(Scenario="MaxDS_Hist"))) %>%
  # Convert data to salinity units per discretewq package (https://github.com/InteragencyEcologicalProgram/discretewq)
  mutate(MTZ_salinity=wql::ec2pss(.data$MTZ_EC_current / 1000, t = 25))

##
x2_data <- na.omit(x2_data) %>% rename(X2 = X2_current) %>% mutate(Month=month(Date))

# Create X2 data frame for all relevant regions
x2_data_expanded <- crossing(x2_data, Region=c("NW Suisun","SW Suisun","NE Suisun","SE Suisun","Confluence", "Suisun Marsh"))

# Add the proper predictors (change month and region to factors)
x2_data_expanded <- x2_data_expanded %>% mutate(month_f = as.factor(Month),region_f= as.factor(Region))

# Use the CSAMP X2-Salinity model to convert CalSim3 X2 values to salinity
x2_data_expanded$salinity<-predict(salX2mod,x2_data_expanded,type="response")

# Ensure that there will be no negative salinity values and use the minimum value in Sam's conversion table
summary(salX2mod)
x2_data_expanded$salinity <- ifelse(x2_data_expanded$salinity<0.1,0.1,x2_data_expanded$salinity)

# Finalize data format
x2_data_reformat <- x2_data_expanded %>%
  mutate(year=year(Date)) %>% rename(region=Region, month=Month) %>% select(-Date,-month_f,-region_f,-X2) %>%
  spread(Scenario,salinity) 

# Create data for Suisun Marsh
SM_data <- MTZ_data %>% mutate(year=year(Date),month=month(Date),region="Suisun Marsh") %>% rename(scenario=Scenario) %>%
  select(month,region,year,scenario,MTZ_salinity) %>% spread(scenario,MTZ_salinity)
#####

# Remove original Suisun Marsh salinity from X2-salinity model
x2_data_reformat_SM_edit <- x2_data_reformat %>% dplyr::filter(region!="Suisun Marsh") %>% bind_rows(SM_data) %>%
  dplyr::filter(!is.na(StatusQuo))

# Rename column names to sal_
colnames(x2_data_reformat_SM_edit)[4:ncol(x2_data_reformat_SM_edit)] <- paste("sal", colnames(x2_data_reformat_SM_edit)[4:ncol(x2_data_reformat_SM_edit)] , sep = "_")


#Export output file for  model input
write.csv(x2_data_reformat_SM_edit,file.path(path_hydro,"converted_salinity_data_SF2025.csv"),row.names=F)
