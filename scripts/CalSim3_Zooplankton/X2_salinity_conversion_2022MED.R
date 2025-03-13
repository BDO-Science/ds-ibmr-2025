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
AltStatusQuo_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_StatusQuo.csv")) 
AltJune_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_June.csv")) 
AltMaxDS_Even_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_MaxDS_Even.csv")) 
AltMaxDS_Hist_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_MaxDS_Hist.csv")) 
AltMaxWater_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_MaxWater.csv")) 
AltSummer_Even_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_Summer_Even.csv")) 
AltSummer_Even_AltSMSCG_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_Summer_Even_AltSMSCG.csv")) 
AltSummer_Hist_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_Summer_Hist.csv")) 
AltSummerFall_Even_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_SummerFall_Even.csv")) 
AltSummerFall_Hist_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_SummerFall_Hist.csv")) 
AltMaxWater_noSMSCG_data <- read.csv(file.path(path_hydro,"CalSim3_data_SF2022MED_MaxWater_noSMSCG.csv")) 

# Combine X2 data
x2_data <- AltStatusQuo_data %>% select(Date,X2_current) %>% mutate(Scenario="StatusQuo") %>%
  bind_rows((AltJune_data %>% select(Date,X2_current) %>% mutate(Scenario="June"))) %>%
  bind_rows((AltMaxDS_Even_data %>% select(Date,X2_current) %>% mutate(Scenario="MaxDS_Even"))) %>%
  bind_rows((AltMaxDS_Hist_data %>% select(Date,X2_current) %>% mutate(Scenario="MaxDS_Hist"))) %>%
  bind_rows((AltMaxWater_data %>% select(Date,X2_current) %>% mutate(Scenario="MaxWater"))) %>%
  bind_rows((AltSummer_Even_data %>% select(Date,X2_current) %>% mutate(Scenario="Summer_Even"))) %>%
  bind_rows((AltSummer_Even_AltSMSCG_data %>% select(Date,X2_current) %>% mutate(Scenario="Summer_Even_AltSMSCG"))) %>%
  bind_rows((AltSummer_Hist_data %>% select(Date,X2_current) %>% mutate(Scenario="Summer_Hist"))) %>%
  bind_rows((AltSummerFall_Even_data %>% select(Date,X2_current) %>% mutate(Scenario="SummerFall_Even"))) %>%
  bind_rows((AltSummerFall_Hist_data %>% select(Date,X2_current) %>% mutate(Scenario="SummerFall_Hist"))) %>%
  bind_rows((AltMaxWater_noSMSCG_data %>% select(Date,X2_current) %>% mutate(Scenario="MaxWater_noSMSCG")))
  
#####
# Load Belden Landing salinity data from CalSim
BD_data <- AltStatusQuo_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="StatusQuo") %>%
  bind_rows((AltJune_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="June"))) %>%
  bind_rows((AltMaxDS_Even_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="MaxDS_Even"))) %>%
  bind_rows((AltMaxDS_Hist_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="MaxDS_Hist"))) %>%
  bind_rows((AltMaxWater_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="MaxWater"))) %>%
  bind_rows((AltSummer_Even_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="Summer_Even"))) %>%
  bind_rows((AltSummer_Even_AltSMSCG_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="Summer_Even_AltSMSCG"))) %>%
  bind_rows((AltSummer_Hist_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="Summer_Hist"))) %>%
  bind_rows((AltSummerFall_Even_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="SummerFall_Even"))) %>%
  bind_rows((AltSummerFall_Hist_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="SummerFall_Hist"))) %>%
  bind_rows((AltMaxWater_noSMSCG_data %>% select(Date,BD_EC_current) %>% mutate(Scenario="MaxWater_noSMSCG"))) %>%
  mutate(Month=month(Date)) %>%
  #Based on looking at the data, will use temperature at 25 for now and assume this is specific conductance
  mutate(BD_salinity = wql::ec2pss(.data$BD_EC_current / 1000, t = 25))
  # Convert data to salinity units per discretewq package (https://github.com/InteragencyEcologicalProgram/discretewq)
  # Refer to Suisun Marsh Survey data at MZ6 to get avg temperature for salinity conversion
  #mutate(BD_salinity= case_when(
  #  Month==1 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 9.51),
  #  Month==2 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 11.5),
  #  Month==3 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 13.6),
  #  Month==4 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 17.2),
  #  Month==5 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 18.7),
  #  Month==6 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 21.7),
   # Month==7 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 22.4),
   # Month==8 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 22.9),
   # Month==9 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 21.1),
   # Month==10 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 18.6),
    #Month==11 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 15.4),
    #Month==12 ~ wql::ec2pss(.data$BD_EC_current / 1000, t = 11.2)
   # ))

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
SM_data <- BD_data %>% mutate(year=year(Date),month=month(Date),region="Suisun Marsh") %>% rename(scenario=Scenario) %>%
  select(month,region,year,scenario,BD_salinity) %>% spread(scenario,BD_salinity)
#####

# Remove original Suisun Marsh salinity from X2-salinity model
x2_data_reformat_SM_edit <- x2_data_reformat %>% dplyr::filter(region!="Suisun Marsh") %>% bind_rows(SM_data) %>%
  dplyr::filter(!is.na(StatusQuo))

# Rename column names to sal_
colnames(x2_data_reformat_SM_edit)[4:ncol(x2_data_reformat_SM_edit)] <- paste("sal", colnames(x2_data_reformat_SM_edit)[4:ncol(x2_data_reformat_SM_edit)] , sep = "_")


#Export output file for  model input
write.csv(x2_data_reformat_SM_edit,file.path(path_hydro,"converted_salinity_data_SF2022MED.csv"),row.names=F)
