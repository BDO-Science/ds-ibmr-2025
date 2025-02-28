# Script to convert necessary CalSim3 dss information into csv
# Identify your working directory for saving outputs of interest
root <- "~/GitHub/ds-ibmr-2025"
setwd(root)

path_hydro <- file.path(root,"scripts","CalSim3_Zooplankton")
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
  #Delta Outflow (OUT in Dayflow)
  java.NDOI_MIN <- dssFile$get("/CALSIM/NDOI_MIN/FLOW//1MON/L2020A/") 
  java.NDOI_ADD <- dssFile$get("/CALSIM/NDOI_ADD/FLOW//1MON/L2020A/") 
  OUTFLOW=data.frame(Date=java.NDOI_MIN$times %>% from_time_stamp,OUTFLOW=java.NDOI_MIN$values+java.NDOI_ADD$values)
  #Old and Middle River flow (OMR)
  java.OMR <- dssFile$get("/CALSIM/C_OMR014/CHANNEL//1MON/L2020A/") 
  OMR=data.frame(Date=java.OMR$times %>% from_time_stamp,OMR=java.OMR$values)
  #Montezuma salinity (previous month)
  java.MTZ <- dssFile$get("/CALSIM/MTZ_EC_7DAY/SALINITY//1MON/L2020A/") 
  MTZ = data.frame(Date=java.MTZ$times %>% from_time_stamp, MTZ_EC_prev=java.MTZ$values)
  #X2 (previous month)
  java.X2 <- dssFile$get("/CALSIM/X2_PRV/X2-POSITION-PREV//1MON/L2020A/") 
  X2=data.frame(Date=java.X2$times %>% from_time_stamp,X2_prev=java.X2$values)
  
  final_data_frame= OUTFLOW %>% left_join(OMR) %>% left_join(MTZ) %>% left_join(X2) %>%
    mutate(X2_current=lead(X2_prev,n=1),MTZ_EC_current=lead(MTZ_EC_prev,n=1))
  return(final_data_frame)
}


#Use the function to create data frame
AltStatusQuo_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_StatusQuo_dv")
AltJune_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_June_wShastaPA_dv")
AltMaxDS_Even_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_MaxDS_Even_ShastaPA_dv")
AltMaxDS_Hist_data <- dss_data_pull(dss_input="D:\\Summer Flow Action SDM\\2025-02-27 - CalSim3 Output\\SF2025_MaxDS_Hist_ShastaPA_dv")
#AltS74_data <- dss_data_pull(dss_input="D:\\2024-07-01 - Delta Smelt SummerFallX2 VOI CalSim Runs\\CS3_DS_VOI_Alt_S74\\DSS\\output\\CS3_DS_VOI_S74_DV_dp")
#AltS74F80_data <- dss_data_pull(dss_input="D:\\2024-07-01 - Delta Smelt SummerFallX2 VOI CalSim Runs\\CS3_DS_VOI_Alt_S74F80\\DSS\\output\\CS3_DS_VOI_S74F80_DV_dp")
#AltNoX2_data <- dss_data_pull(dss_input="D:\\2024-07-01 - Delta Smelt SummerFallX2 VOI CalSim Runs\\CS3_DS_VOI_Alt_NoX2\\DSS\\output\\CS3_DS_VOI_NoX2_DV_dp")


#Export DSS output files for Delta Smelt LCM model input
write.csv(AltStatusQuo_data,file.path(path_hydro,"CalSim3_data_StatusQuo.csv"),row.names=F)
write.csv(AltJune_data,file.path(path_hydro,"CalSim3_data_June.csv"),row.names=F)
write.csv(AltMaxDS_Even_data,file.path(path_hydro,"CalSim3_data_MaxDS_Even.csv"),row.names=F)
write.csv(AltMaxDS_Hist_data,file.path(path_hydro,"CalSim3_data_MaxDS_Hist.csv"),row.names=F)
#write.csv(AltS74_data,file.path(path_hydro,"AltS74_data_CalSim3_data.csv"),row.names=F)
#write.csv(AltS74F80_data,file.path(path_hydro,"AltS74F80_data_CalSim3_data.csv"),row.names=F)
#write.csv(AltNoX2_data,file.path(path_hydro,"AltNoX2_CalSim3_data.csv"),row.names=F)

##### Calculate flow input for the Delta Smelt IBMR

AltStatusQuo_data <- AltStatusQuo_data %>% mutate(scenario="StatusQuo")
AltJune_data <- AltJune_data %>% mutate(scenario="June")
AltMaxDS_Even_data <- AltMaxDS_Even_data %>% mutate(scenario="MaxDS_Even")
AltMaxDS_Hist_data <- AltMaxDS_Hist_data %>% mutate(scenario="MaxDS_Hist")


combined_data <- bind_rows(AltStatusQuo_data,AltJune_data,AltMaxDS_Even_data,AltMaxDS_Hist_data)
data_flow_IBMR <- combined_data %>% rename(X2=X2_current) %>% select(-X2_prev) %>% mutate(year=year(Date), month=month(Date))

#Export full data
write.csv(data_flow_IBMR,file.path(path_hydro,"IBMR_FlowData_DeltaSmelt_SF2025_alternatives.csv"),row.names=F)
#write.csv(data_flow_StatusQuo,file.path(path_hydro,"IBMR_FlowData_StatusQuo.csv"),row.names=F)

#Create X2 information
#data_X2_StatusQuo <- data_flow_StatusQuo %>% select(scenario,year,month,X2) %>% filter(year %in% c(1995:2014)) %>%
#  spread(month,X2)
data_X2 <- data_flow_IBMR %>% select(scenario,year,month,X2) %>% filter(year %in% c(1995:2014)) %>%
  spread(month,X2)

#Create OMR information
data_OMR <- data_flow_IBMR %>% select(scenario,year,month,OMR) %>% filter(year %in% c(1995:2014)) %>%
  spread(month,OMR)

#Export X2 and OMR data for IBMR input
write.csv(data_X2,file.path(path_output,"IBMR_X2_SF2025_input.csv"),row.names=F)
write.csv(data_OMR,file.path(path_output,"IBMR_OMR_SF2025_input.csv"),row.names=F)

#write.csv(data_X2_StatusQuo,file.path(path_output,"IBMR_X2_StatusQuo_input.csv"),row.names=F)
#write.csv(data_OMR_StatusQuo,file.path(path_output,"IBMR_OMR_StatusQuo_input.csv"),row.names=F)




