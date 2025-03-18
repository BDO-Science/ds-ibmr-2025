# Script to convert necessary CalSim3 dss information into csv
# Identify your working directory for saving outputs of interest
root <- "~/GitHub/ds-ibmr-2025"
setwd(root)

path_hydro <- file.path(root,"scripts","Water_data_CalSim3")
path_output <- file.path(root,"data","data_processed")

# The following libraries need to be installed and loaded
# NOTE: You also need to have HEC-DSSVue installed on your computer
# See: https://www.hec.usace.army.mil/software/hec-dssvue/downloads.aspx

library(tidyverse)
library(stringr)
library(lubridate)
library(rJava)
library(readxl)

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
  java.SHSTA <- dssFile$get("/CALSIM/S_SHSTA/STORAGE//1MON/L2020A/") 
  java.OROVL <- dssFile$get("/CALSIM/S_OROVL/STORAGE//1MON/L2020A/") 
  java.FOLSM <- dssFile$get("/CALSIM/S_FOLSM/STORAGE//1MON/L2020A/") 
  
  SHASTA=data.frame(Date=java.SHSTA$times %>% from_time_stamp,Shasta_storage=java.SHSTA$values)
  OROVILLE=data.frame(Date=java.OROVL$times %>% from_time_stamp,Oroville_storage=java.OROVL$values)
  FOLSOM=data.frame(Date=java.FOLSM$times %>% from_time_stamp,Folsom_storage=java.FOLSM$values)
  
  #Water year type
  java.WYSAC <- dssFile$get("/CALSIM/WYT_SAC_/WATERYEARTYPE//1MON/L2020A/") 
  WYSAC=data.frame(Date=java.WYSAC$times %>% from_time_stamp,WY_type_SAC=java.WYSAC$values)
  #Rock Slough
  java.RS_CL <- dssFile$get("/CALSIM/RS_CL_MONTH/SALINITY//1MON/L2020A/") 
  RS_CL_Month =data.frame(Date=java.RS_CL$times %>% from_time_stamp,RockSlough_chloride=java.RS_CL$values)
  #Jersey Point
  java.JP_EC <- dssFile$get("/CALSIM/JP_EC_MONTH/SALINITY//1MON/L2020A/") 
  JP_EC_Month =data.frame(Date=java.JP_EC$times %>% from_time_stamp,JerseyPoint_EC=java.JP_EC$values)
  #Water export
  #SWP water pumping at Banks
  java.SWP_EXP <- dssFile$get("/CALSIM/C_CAA003_SWP/FLOW-DELIVERY//1MON/L2020A/") 
  SWP_EXP = data.frame(Date=java.SWP_EXP$times %>% from_time_stamp,SWP_DeltaExport=java.SWP_EXP$values) #in CFS
  #CVP water pumping C_DMC003+C_CAA003_CVP (pumping at Jones and CVP pumping at Banks)
  java.CVP_jones <- dssFile$get("/CALSIM/C_CAA003_CVP/FLOW-DELIVERY//1MON/L2020A/") 
  java.CVP_banks <- dssFile$get("/CALSIM/C_DMC003/CHANNEL//1MON/L2020A/") 
  #both are in CFS
  CVP_EXP=data.frame(Date=java.CVP_jones$times %>% from_time_stamp,CVP_DeltaExport=java.CVP_jones$values+java.CVP_banks$values)
  
  #Water Cost calculation for SWP
  #java.SWP_summerEXP <- dssFile$get("/CALSIM/SWP_EXPCUT_FOR_SUMMERHABITATDV/SUMMERHABITAT//1MON/L2020A/")
  #java.SWP_fallEXP <- dssFile$get("/CALSIM/SWP_EXPCUT_FOR_FX2DV/FALLX2//1MON/L2020A/")
  #java.SWP_summerDS <- dssFile$get("/CALSIM/SWPDSFORSUMMERHABITATDV/SUMMERHABITAT//1MON/L2020A/")
  #java.SWP_fallDS <- dssFile$get("/CALSIM/SWPDSFORFX2DV/FALLX2//1MON/L2020A/")
  
  #Water Cost calculation for CVP
  #java.CVP_summerEXP <- dssFile$get("/CALSIM/CVP_EXPCUT_FOR_SUMMERHABITATDV/SUMMERHABITAT//1MON/L2020A/")
  #java.CVP_summerDS <- dssFile$get("/CALSIM/CVPSW_FOR_SUMMERHABITATDV/SUMMERHABITAT//1MON/L2020A/")
  #java.CVP_fallEXP <- dssFile$get("/CALSIM/CVPFALLX2_EXPRED2DV/OUTPUT-CFS//1MON/L2020A/")
  #java.CVP_fallDS <- dssFile$get("/CALSIM/CVPFALLX2_SWDV/OUTPUT-TAF//1MON/L2020A/")
  #java.CVP_UnusedFS_summer <- dssFile$get("/CALSIM/CVPSUMMERHABITAT_UNUSEDFSDV//1MON/L2020A/")
  #java.CVP_UnusedFS_fall <- dssFile$get("/CALSIM/CVPFALLX2_UNUSEDFSDV/OUTPUT-CFS//1MON/L2020A/")
  
  
  final_data_frame= SHASTA %>% left_join(OROVILLE) %>% left_join(FOLSOM) %>% left_join(WYSAC) %>% 
    left_join(RS_CL_Month) %>% left_join(JP_EC_Month) %>% left_join(SWP_EXP) %>% left_join(CVP_EXP)
  
  
  return(final_data_frame)
}


#Use the function to create data frame
AltStatusQuo_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_StatusQuo_dv")
AltJune_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_June_wShastaPA_dv")
AltMaxDS_Even_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_MaxDS_Even_ShastaPA_dv")
AltMaxDS_Hist_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_MaxDS_Hist_ShastaPA_dv")
AltMaxWater_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_MaxWater_ShastaPA_dv")
AltSummer_Even_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_Summer_Even_ShastaPA_dv")
AltSummer_Even_AltSMSCG_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_Summer_Even_AltSMSCG_ShastaPA_dv")
AltSummer_Hist_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_Summer_Hist_dv")
AltSummerFall_Even_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_SummerFall_Even_dv")
AltSummerFall_Hist_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_SummerFall_Hist_dv")
AltMaxWater_noSMSCG_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_MaxWater_noSMSCG_dv")

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

#Export full data
write.csv(combined_data,file.path(path_hydro,"CalSim3Data_PerfMetrics.csv"),row.names=F)

### Extra processing 
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
  mutate(May_Value = ifelse(month %in% c(10,11,12,1,2,3,4), NA, May_Value)) %>%
  fill(May_Value, .direction = "up") %>%
  mutate(WY_type = case_when(
    May_Value==1 ~ "Wet",
    May_Value==2 ~ "Above Normal",
    May_Value==3 ~ "Below Normal",
    May_Value==4 ~ "Dry",
    May_Value==5 ~ "Critically Dry"
  )) %>% mutate(WY = ifelse(month >=10, year(Date)+1, year(Date))) %>%
  filter(year>1921)

# Shasta end of September calculation
data_shasta <- data_may %>% filter(month==9) %>% filter(WY_type %in% c("Wet","Above Normal")) %>%
  group_by(scenario) %>%
  summarise(Shasta_end_of_Sept = mean(Shasta_storage))

length(unique(data_shasta$WY))

# Above 2.4 mil acre feet in Sept
data_shasta_sept <- data_may %>% filter(month==9) %>% filter(WY_type %in% c("Wet","Above Normal")) %>%
  mutate(AboveShasta = ifelse(Shasta_storage>=2400,1,0)) %>%
  group_by(scenario) %>%
  summarise(Shasta_end_of_Sept_exceedance = mean(AboveShasta))

# Oroville calculation
data_oroville <- data_may %>% filter(month==9) %>% filter(WY_type %in% c("Wet","Above Normal")) %>%
  mutate(AboveOroville_185 = ifelse(Oroville_storage>=1850,1,0)) %>%
  group_by(scenario) %>%
  summarise(Oroville_end_of_Sept_exceedance = mean(AboveOroville_185))

# Folsom calculation

data_end_December_folsom <- data_may %>% filter(month==12) %>% arrange(scenario, Date) %>%
  mutate(prev_WY_type=lag(WY_type,n=1,default=NA)) %>%
  filter(prev_WY_type %in% c("Wet","Above Normal")) %>%
  group_by(scenario) %>%
  summarise(Folsom_end_of_December = mean(Folsom_storage))

data_end_dec_prob_folsom <- data_may %>% filter(month==12) %>% arrange(scenario, Date) %>%
  mutate(prev_WY_type=lag(WY_type,n=1,default=NA)) %>%
  filter(prev_WY_type %in% c("Wet","Above Normal")) %>%
  mutate(Above300TAF_Dec = ifelse(Folsom_storage>=300,1,0)) %>%
  group_by(scenario) %>%
  summarise(Folsom_end_of_Dec_exceedance_300TAF = mean(Above300TAF_Dec))

data_end_apr_prob_folsom <- data_may %>% filter(month==4) %>% arrange(scenario, Date) %>%
  mutate(prev_WY_type=lag(WY_type,n=1,default=NA)) %>%
  filter(prev_WY_type %in% c("Wet","Above Normal")) %>%
  mutate(Above300TAF_Apr = ifelse(Folsom_storage>=300,1,0)) %>%
  group_by(scenario) %>%
  summarise(Folsom_end_of_Apr_exceedance_300TAF = mean(Above300TAF_Apr))

# Shasta end of April calculation

data_end_april <- data_may %>% filter(month==4) %>% arrange(scenario, Date) %>%
  mutate(prev_WY_type=lag(WY_type,n=1,default=NA)) %>%
  filter(prev_WY_type %in% c("Wet","Above Normal")) %>%
  group_by(scenario) %>%
  summarise(Shasta_end_of_April = mean(Shasta_storage))

data_end_april_prob <- data_may %>% filter(month==4) %>% arrange(scenario, Date) %>%
  mutate(prev_WY_type=lag(WY_type,n=1,default=NA)) %>%
  filter(prev_WY_type %in% c("Wet","Above Normal")) %>%
  mutate(AboveShasta_37 = ifelse(Shasta_storage>=3700,1,0)) %>%
  group_by(scenario) %>%
  summarise(Shasta_end_of_April_exceedance = mean(AboveShasta_37))

# Water cost calculation
cost_data <- read_xlsx(file.path(path_hydro,"SummerFall_WaterCost_AdjHist_CFS.xlsx")) %>%
  left_join(data_may %>% select(WY, WY_type) %>% distinct) %>%
  arrange(Scenario,Date) %>%
  mutate(WY_adjusted = lag(WY,n=1,default=NA), WY_type_adjusted=lag(WY_type,n=1,default=NA))

test_data <- cost_data %>% filter(Scenario=="MaxWater")

cost_data_sum <- cost_data %>% filter(WY_type_adjusted %in% c("Wet","Above Normal")) %>% 
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(Export_SWP=SWP_EXPCUT_FOR_FX2DV+SWP_EXPCUT_FOR_SUMMERHABITATDV,
         Export_CVP=CVPFALLX2_EXPRED2DV+CVP_EXPRED_SUMMERHABITATDV,
         Withdraw_SWP=SWPDSFORFX2DV+SWPDSFORSUMMERHABITATDV,
         Withdraw_CVP=CVPFALLX2_SWDV+CVPSW_FOR_SUMMERHABITATDV) %>%
  #Convert to TAF
  mutate(Export_SWP=0.0595*Export_SWP,
         Export_CVP=0.0595*Export_CVP,
         Withdraw_SWP=0.0595*Withdraw_SWP,
         Withdraw_CVP=0.0595*Withdraw_CVP) %>%
  group_by(WY_adjusted,Scenario) %>%
  summarise(Export_SWP=sum(Export_SWP),Export_CVP=sum(Export_CVP),Withdraw_SWP=sum(Withdraw_SWP),
            Withdraw_CVP=sum(Withdraw_CVP))

cost_data_mean <- cost_data_sum %>% group_by(Scenario) %>%
  summarise(Export_SWP=mean(Export_SWP),Export_CVP=mean(Export_CVP),Withdraw_SWP=mean(Withdraw_SWP),
            Withdraw_CVP=mean(Withdraw_CVP))

write.csv(cost_data_mean,file.path(path_hydro,"WaterCostCalculation_DWRAdjustedHydro.csv"))

##########################
# Export calculation
data_CVP_currentcontractyr <- combined_data %>%
  group_by(scenario) %>%
  arrange(scenario, Date) %>%
  mutate(May_Value = ifelse(month == month_of_interest, WY_type_SAC, NA)) %>%
  fill(May_Value, .direction = "down") %>%
  mutate(May_Value = ifelse(month %in% c(10,11,12,1,2,3,4), NA, May_Value)) %>%
  fill(May_Value, .direction = "up") %>%
  mutate(WY_type = case_when(
    May_Value==1 ~ "Wet",
    May_Value==2 ~ "Above Normal",
    May_Value==3 ~ "Below Normal",
    May_Value==4 ~ "Dry",
    May_Value==5 ~ "Critically Dry"
  )) %>% mutate(WY = ifelse(month >=10, year(Date)+1, year(Date))) %>% 
  mutate(contract_CVP_WY_type=lag(WY_type,n=5,default=NA)) %>% 
  #Fill NA and move up 
  fill(contract_CVP_WY_type, .direction = "up") %>%
  # Remove the earlier months without a full contract year and those with NA values
  filter(Date > as.Date("1922-02-28")) %>% 
  mutate(prev_contract_CVP_WY_type=lag(contract_CVP_WY_type,n=12,default=NA)) %>%
  mutate(actionyr_or_after=ifelse(contract_CVP_WY_type %in% c("Wet","Above Normal")|prev_contract_CVP_WY_type %in% c("Wet","Above Normal"),"yes","no")) %>%
  # Convert to TAF
  mutate(CVP_DeltaExport_TAF = 0.0595*CVP_DeltaExport) %>%
  # Remove non-action year or those not +1
  filter(actionyr_or_after=="yes") %>% 
  # add contract yr
  mutate(contractyr_name=lag(WY,n=5,default=NA)) %>% 
  fill(contractyr_name, .direction = "up") %>%
  # check that there's no NA
  filter(!is.na(contractyr_name)) %>%
  group_by(scenario, contractyr_name) %>%
  summarise(CVP_DeltaExport_TAF=sum(CVP_DeltaExport_TAF)) %>%
  # average across scenarios
  group_by(scenario) %>% summarise(CVP_DeltaExport_TAF=mean(CVP_DeltaExport_TAF))

data_SWP_currentcontractyr <- combined_data %>%
  group_by(scenario) %>%
  arrange(scenario, Date) %>%
  mutate(May_Value = ifelse(month == month_of_interest, WY_type_SAC, NA)) %>%
  fill(May_Value, .direction = "down") %>%
  mutate(May_Value = ifelse(month %in% c(10,11,12,1,2,3,4), NA, May_Value)) %>%
  fill(May_Value, .direction = "up") %>%
  mutate(WY_type = case_when(
    May_Value==1 ~ "Wet",
    May_Value==2 ~ "Above Normal",
    May_Value==3 ~ "Below Normal",
    May_Value==4 ~ "Dry",
    May_Value==5 ~ "Critically Dry"
  )) %>% mutate(WY = ifelse(month >=10, year(Date)+1, year(Date))) %>% 
  mutate(contract_SWP_WY_type = ifelse(month == month_of_interest, WY_type, NA)) %>%
  fill(contract_SWP_WY_type, .direction = "down") %>%
  mutate(contract_SWP_WY_type = ifelse(month %in% c(1,2,3,4), NA, contract_SWP_WY_type)) %>%
  fill(contract_SWP_WY_type, .direction = "up") %>%
  # Remove the earlier months without a full contract year and those with NA values
  filter(Date >= as.Date("1922-01-31")) %>% 
  mutate(prev_contract_SWP_WY_type=lag(contract_SWP_WY_type,n=12,default=NA)) %>%
  mutate(actionyr_or_after=ifelse(contract_SWP_WY_type %in% c("Wet","Above Normal")|prev_contract_SWP_WY_type %in% c("Wet","Above Normal"),"yes","no")) %>%
  # Convert to TAF
  mutate(SWP_DeltaExport_TAF = 0.0595*SWP_DeltaExport) %>%
  # Remove non-action year or those not +1
  filter(actionyr_or_after=="yes") %>% 
  # add contract yr
  mutate(contractyr_name=year) %>% 
  # check that there's no NA
  filter(!is.na(contractyr_name)) %>%
  group_by(scenario, contractyr_name) %>%
  summarise(SWP_DeltaExport_TAF=sum(SWP_DeltaExport_TAF)) %>%
  # average across scenarios
  group_by(scenario) %>% summarise(SWP_DeltaExport_TAF=mean(SWP_DeltaExport_TAF))


data_contract_yr_combined <- data_CVP_currentcontractyr %>% left_join(data_SWP_currentcontractyr)

write.csv(data_contract_yr_combined,file.path(path_hydro,"ContractYearExportCalculation_DWRAdjustedHydro.csv"))


# Code to check that the trends report and this calc is the same if I consider 100yr total
#test<- data_may %>% filter(Date >= as.Date("1922-01-31")) %>% group_by(year,scenario) %>% # Convert to TAF
#  mutate(SWP_DeltaExport_TAF = 0.0595*SWP_DeltaExport) %>%
#  summarise(SWP_DeltaExport_TAF=sum(SWP_DeltaExport_TAF)) %>%
#  # average across scenarios
#  group_by(scenario) %>% summarise(SWP_DeltaExport_TAF=mean(SWP_DeltaExport_TAF))