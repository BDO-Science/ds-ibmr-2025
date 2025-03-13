# Script to convert necessary CalSim3 dss information into csv
# Identify your working directory for saving outputs of interest
root <- "~/GitHub/ds-ibmr-2025"
setwd(root)

path_here <- file.path(root,"scripts","Redd_dewatering")
path_output <- file.path(root,"data","data_processed")

# The following libraries need to be installed and loaded
# NOTE: You also need to have HEC-DSSVue installed on your computer
# See: https://www.hec.usace.army.mil/software/hec-dssvue/downloads.aspx

library(tidyverse)
library(stringr)
library(lubridate)
library(rJava)


#############
#Read DSS file

# The following function for is used for turning CalSim time stamps into R dates. 

from_time_stamp <- function(x) {
  day_ref <- as.Date("1899-12-30")
  return(day_ref+x/1440)
}



# Run this workaround if your R session crashes when running .jinit() - below
# This issue occurs in base R versions 4.2 and later
# In lieu of this workaround, you can also install a patched R version 
# E.g., https://cran.r-project.org/bin/windows/base/rpatched.html

replacement <- function(category = "LC_ALL") {
  
  if (identical(category, "LC_MESSAGES"))
    return("")
  
  category <- match(category, .LC.categories)
  if (is.na(category)) 
    stop("invalid 'category' argument")
  .Internal(Sys.getlocale(category))
  
}
base <- asNamespace("base")
environment(replacement) <- base
unlockBinding("Sys.getlocale", base)
assign("Sys.getlocale", replacement, envir = base)
lockBinding("Sys.getlocale", base)


# This code establishes your java connection with HEC-DSSVue

# Specify your own location for 'HEC-DSSVue'
dss_location <- "C:\\Program Files\\HEC\\HEC-DSSVue\\" 

# Specify your own location for the 'jar' sub-folder
# This identifies all possible java executables that meet be needed
jars <- c(list.files("C:\\Program Files\\HEC\\HEC-DSSVue\\jar")) 

jars <- paste0(dss_location, "jar/", jars)

# Specify your own location for the 'lib' sub-folder
libs <- "-Djava.library.path=C:\\Program Files\\HEC\\HEC-DSSVue\\lib\\"

.jinit(classpath = jars, parameters = libs)


##########
# Function to assemble the dataset

# Identify the DSS file you want to access with dss_input

dss_data_pull<-function(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output"){
  # Open the DSS file through rJava
  dssFile <- .jcall("hec/heclib/dss/HecDss", "Lhec/heclib/dss/HecDss;",   method="open", dss_input)
  #storage volumes
  java.SAC_KES <- dssFile$get("/CALSIM/C_SAC299/CHANNEL//1MON/L2020A/") 

  SAC_KES=data.frame(Date=java.SAC_KES$times %>% from_time_stamp,SAC_Keswick_CFS=java.SAC_KES$values)
  
  #Water year type
  java.WYSAC <- dssFile$get("/CALSIM/WYT_SAC_/WATERYEARTYPE//1MON/L2020A/") 
  WYSAC=data.frame(Date=java.WYSAC$times %>% from_time_stamp,WY_type_SAC=java.WYSAC$values)
  
  final_data_frame= SAC_KES %>% left_join(WYSAC)
  
  
  return(final_data_frame)
}


#Use the function to create data frame
AltStatusQuo_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_StatusQuo_dv")
AltJune_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_June_dv")
AltMaxDS_Even_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_MaxDS_Even_dv")
AltMaxDS_Hist_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_MaxDS_Hist_dv")
AltMaxWater_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_MaxWater_dv")
AltSummer_Even_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_Summer_Even_dv")
AltSummer_Even_AltSMSCG_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_Summer_Even_AltSMSCG_dv")
AltSummer_Hist_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_Summer_Hist_dv")
AltSummerFall_Even_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_SummerFall_Even_dv")
AltSummerFall_Hist_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_SummerFall_Hist_dv")
AltMaxWater_noSMSCG_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-03-07 - CalSim3 Output BOR hydrology\\SF2022MED_MaxWater_noSMSCG_dv")

##### add alternative names

AltStatusQuo_data <- AltStatusQuo_data %>% mutate(scenario="StatusQuo")
AltJune_data <- AltJune_data %>% mutate(scenario="June")
AltMaxDS_Even_data <- AltMaxDS_Even_data %>% mutate(scenario="MaxDS_Even")
AltMaxDS_Hist_data <- AltMaxDS_Hist_data %>% mutate(scenario="MaxDS_Hist")
AltMaxWater_data <- AltMaxWater_data %>% mutate(scenario="MaxWater")
AltSummer_Even_data <- AltSummer_Even_data %>% mutate(scenario="Summer_Even")
AltSummer_Even_AltSMSCG_data <- AltSummer_Even_data %>% mutate(scenario="Summer_Even_AltSMSCG")
AltSummer_Hist_data <- AltSummer_Hist_data %>% mutate(scenario="Summer_Hist")
AltSummerFall_Even_data <- AltSummerFall_Even_data %>% mutate(scenario="SummerFall_Even")
AltSummerFall_Hist_data <- AltSummerFall_Hist_data %>% mutate(scenario="SummerFall_Hist")
AltMaxWater_noSMSCG_data <- AltMaxWater_noSMSCG_data %>% mutate(scenario="MaxWater_noSMSCG")


combined_data <- bind_rows(AltStatusQuo_data,AltJune_data,AltMaxDS_Even_data,AltMaxDS_Hist_data,AltMaxWater_data,AltSummer_Even_data,AltSummer_Even_AltSMSCG_data,
                           AltSummer_Hist_data,AltSummerFall_Even_data,AltSummerFall_Hist_data,AltMaxWater_noSMSCG_data) %>% 
  mutate(year=year(Date), month=month(Date))


combined_data <- combined_data %>%
  arrange(scenario, Date)


# Specify the month of interest (May)
month_of_interest <- 5  # May

# Extract the value for May of each year
# Add water year type for sorting

data_may <- combined_data %>%
  #group_by(scenario) %>%
  mutate(May_Value = ifelse(month == month_of_interest, WY_type_SAC, NA)) %>%
  fill(May_Value, .direction = "down") %>%
  mutate(May_Value = ifelse(month %in% c(12,1,2,3,4), NA, May_Value)) %>%
  fill(May_Value, .direction = "up") %>%
  mutate(WY_type = case_when(
    May_Value==1 ~ "Wet",
    May_Value==2 ~ "Above Normal",
    May_Value==3 ~ "Below Normal",
    May_Value==4 ~ "Dry",
    May_Value==5 ~ "Critically Dry"
  ))

# Filter just Above Normal
data_AN <- data_may %>% filter(WY_type == "Above Normal") %>%
  select(-May_Value,-WY_type_SAC) %>%
  filter(month %in% (4:11)) %>% filter(year > 1921)

unique(data_AN$year)

#Export full data
write.csv(data_AN,file.path(path_here,"SacFlowData_ReddDewatering_AdjustedHistHydro_2022MED.csv"),row.names=F)

