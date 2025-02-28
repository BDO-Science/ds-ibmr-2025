root <- "~/GitHub/ds-ibmr-2025/scripts/CalSim3_Zooplankton"
setwd(root)

require(conflicted)
require(MASS)
require(dplyr)
require(zooper)
require(lubridate)
require(readr)
require(tidyr)
require(ggplot2)
require(sf)
require(readxl)
require(stringr)
require(mgcv)
require(purrr)
require(deltamapr)
require(scales)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Load and wrangle data
# Data was acquired from the 'zooper' package.
# Citation: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0265402
zoop_data<-Zoopsynther(Data_type="Community", Sources=c("EMP", "STN", "20mm", "FMWT"), Time_consistency = FALSE)

#Read in zoop mass conversions using excel sheet.
zoop_mass_conversions<-read_excel(file.path("Data","Biomass conversions.xlsx"), sheet="Micro and Meso-zooplankton") %>%
  mutate(Taxname=case_when(Taxname=="Sinocalanus"~"Sinocalanus doerrii", # Change to help this match to zoop data
                           TRUE ~ Taxname),
         Taxlifestage=paste(Taxname, Lifestage))%>%
  select(Taxlifestage, CarbonWeight_ug)

#Read in zoop groupings
zoop_groups<-read_csv(file.path("Data","zoopcrosswalk3.csv"), col_types=cols_only(Taxlifestage="c", IBMR="c"))%>%
  distinct()

#Load Mysid biomass data
zoop_mysid<-read_excel(file.path("Data","1972-2020MysidBPUEMatrix.xlsx"), # EMP
                       sheet="Mysid_BPUE_matrix_1972-2020", na = "NA",
                       col_types = c(rep("numeric", 4), "date", "text", "text", rep("text", 7), rep("numeric", 8)))%>%
  select(Date=SampleDate, Station=StationNZ, BPUE=`Hyperacanthomysis longirostris`)%>% # Only select Hyperacanthomysis longirostris
  mutate(Source="EMP")%>%
  bind_rows(read_csv(file.path("Data","FMWT STN 2007to2019 Mysid BPUE.csv"), # FMWT/STN
                     col_types=cols_only(Station="c", SampleDate="c", Project="c", `Hyperacanthomysis longirostris`="d"))%>% 
              rename(Date=SampleDate, Source=Project, BPUE=`Hyperacanthomysis longirostris`)%>% # Only select Hyperacanthomysis longirostris
              mutate(Date=mdy(Date),
                     Station=recode(Station, MONT="Mont", HONK="Honk")))%>% #Get station names to match to main dataset
  mutate(BPUE_mysid=BPUE*1000, # Convert to ug
         Taxlifestage="Hyperacanthomysis longirostris Adult",
         SampleID=paste(Source, Station, Date),
         SizeClass="Macro")%>%
  select(SampleID, Taxlifestage, SizeClass, BPUE_mysid)

#Start processing the zoop data
zoop_data_mass<-zoop_data%>%
  mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
  filter(
    !(SizeClass=="Meso" & #eliminating species which are counted in meso and micro and retained better in the micro net from the meso calcs
        
        Taxlifestage%in%c("Asplanchna Adult", "Copepoda Larva","Cyclopoida Juvenile", "Eurytemora Larva", "Harpacticoida Undifferentiated",
                          "Keratella Adult", "Limnoithona Adult", "Limnoithona Juvenile", "Limnoithona sinenesis Adult", "Limnoithona tetraspina
                                    Adult", "Oithona Adult", "Oithona Juvenile", "Oithona davisae Adult", "Polyarthra Adult","Pseudodiaptomus Larva", 
                          "Rotifera Adult", "Sinocalanus doerrii Larva", "Synchaeta Adult", "Synchaeta bicornis Adult", "Trichocerca Adult")) &
      
      !(SizeClass=="Micro" &Taxlifestage%in%c("Cirripedia Larva", "Cyclopoida Adult", "Oithona similis")) & #removing categories better retained in meso net from micro net matrix
      (is.na(Order) | Order!="Amphipoda") & # Remove amphipods
      (is.na(Order) | Order!="Mysida" | Taxlifestage=="Hyperacanthomysis longirostris Adult"))%>% #Only retain Hyperacanthomysis longirostris
  mutate(Taxlifestage=recode(Taxlifestage, `Synchaeta bicornis Adult`="Synchaeta Adult", # Change some names to match to biomass conversion dataset
                             `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult",
                             `Acanthocyclops vernalis Adult`="Acanthocyclops Adult"))%>%
  left_join(zoop_mass_conversions, by="Taxlifestage")%>% # Add biomass conversions
  left_join(zoop_mysid, by=c("SampleID", "Taxlifestage", "SizeClass"))%>% # Add mysid biomass
  left_join(zoop_groups, by="Taxlifestage")%>% # Add IBMR categories
  mutate(BPUE=if_else(Taxlifestage=="Hyperacanthomysis longirostris Adult", BPUE_mysid, CPUE*CarbonWeight_ug))%>% # Create 1 BPUE variable
  filter(!is.na(BPUE) & !is.na(Latitude) & !is.na(Longitude) & !is.na(SalSurf))%>% # Removes any data without BPUE, which is currently restricted to Rotifera Adult, Copepoda Larva, and H. longirostris from STN. Also removes 20mm and EMP EZ stations without coordinates
  group_by(IBMR)%>%
  mutate(flag=if_else(all(c("Micro", "Meso")%in%SizeClass), "Remove", "Keep"))%>% # This and the next 2 lines are meant to ensure that all categories are consistent across the surveys. Since only EMP samples microzoops, only EMP data can be used for categories that include both micro and mesozoops.
  ungroup()%>%
  filter(!(flag=="Remove" & Source!="EMP"))%>%
  select(SampleID, Station, Latitude, Longitude, SalSurf, Date, Year, IBMR, BPUE)%>%
  group_by(across(-BPUE))%>%
  summarise(BPUE=sum(BPUE), .groups="drop")%>% # Sum each IBMR categories
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
  st_transform(crs=st_crs(deltamapr::R_DSIBM)) %>% 
  st_join(deltamapr::R_DSIBM %>%
            select(SUBREGION)) %>%
  st_drop_geometry() %>% 
  filter(SUBREGION %in% c("NW Suisun","SW Suisun","NE Suisun","SE Suisun","Confluence", "Suisun Marsh"))%>%
  mutate(doy=yday(Date), #Day of year
         Month=month(Date), # Month
         Year_fac=factor(Year), # Factor year for model random effect
         Station_fac=factor(Station), # Factor station for model random effect
         across(c(SalSurf, doy), list(s=~(.x-mean(.x))/sd(.x))), # Center and standardize predictors
         BPUE_log1p=log(BPUE+1)) # log1p transform BPUE for model


#All the remaining brackish regions have sufficient sample size with the exception of NE Suisun. 
#As such, NE Suisun is to be combined with SE Suisun while the rest of the regions are to be analyzed on their own.
#Create a new column with IBMR edited regions to accomodate combination of NE and SE Suisun regions.
zoop_data_mass$Subregion_edit<-ifelse(zoop_data_mass$SUBREGION%in%c("NE Suisun", "SE Suisun"), "East Suisun", zoop_data_mass$SUBREGION)

# Model
## Prediction data
#Set up prediction data for model

# Min year to start models
year_min<-1995

newdata_function<-function(region, data=zoop_data_mass, quant=0.99){
  
  lower<-(1-quant)/(2)
  upper<-1-lower
  
  data_filt<-data%>%
    filter(Subregion_edit%in%region & Year >= year_min)
  
  # Calculate monthly quantiles of salinity
  month_sal<-data_filt%>%
    group_by(Month)%>%
    summarise(l=quantile(SalSurf, lower),
              u=quantile(SalSurf, upper), .groups="drop")
  
  newdata<-expand_grid(date=mdy(paste(1:12, 15, 2001, sep="/")), # The 15th of each month on a non-leap year
                       SalSurf=seq(round(min(data_filt$SalSurf), 1), 
                                   round(max(data_filt$SalSurf), 1), by=0.1))%>% # Salinity sequence nicely rounded to 1 decimal
    mutate(Month=month(date),
           doy=yday(date), # Day of year
           SalSurf_s=(SalSurf-mean(data$SalSurf))/sd(data$SalSurf), # center and standardize salinity to match data
           doy_s=(doy-mean(data$doy))/sd(data$doy))%>% # center and standardize doy to match data
    left_join(month_sal, by="Month")%>%
    filter(SalSurf >= l & SalSurf <= u)%>% # Remove any salinity values outside the quantiles for each month
    select(Month, doy, doy_s, SalSurf, SalSurf_s)
  
}

newdata<-map(set_names(unique(zoop_data_mass$Subregion_edit)), newdata_function)

## Posterior prediction function

# Function to generate posterior predictions from a gam model
# From https://stats.stackexchange.com/questions/190348/can-i-use-bootstrapping-to-estimate-the-uncertainty-in-a-maximum-value-of-a-gam
predict_posterior<-function(model, newdata, exclude, n=1e3, seed=999){
  Xp <- predict(model, newdata=newdata, type="lpmatrix", exclude=exclude, newdata.guaranteed=TRUE) ## map coefs to fitted curves
  beta <- coef(model)
  Vb   <- vcov(model) ## posterior mean and cov of coefs
  set.seed(seed)
  mrand <- mvrnorm(n, beta, Vb) ## simulate n rep coef vectors from posterior
  pred<-matrix(nrow=nrow(newdata), ncol=n)
  ilink <- family(model)$linkinv
  for (i in seq_len(n)) { 
    pred[,i]   <- ilink(Xp %*% mrand[i, ])
  }
  colnames(pred)<-paste("draw", 1:n, sep="_")
  pred<-as_tibble(pred)
  return(pred)
}


## Model fitting
sal_model<-function(group,region,new_data=newdata){
  
  cat("<<<<<<<<<<<<<<<<<<<<<<< modeling", group, region, ">>>>>>>>>>>>>>>>>>>>>>>>>\n\n")
  
  new_data<-new_data[[region]]
  
  data<-filter(zoop_data_mass, IBMR==group & Subregion_edit==region & Year>=year_min)
  
  par(mfrow=c(2,2))
  
  if(length(unique(data$Station_fac))>1){
    model<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                 s(Year_fac, bs="re") + s(Station_fac, bs="re"),
               data=data, 
               method="REML")
    
    random_effects<-c("s(Year_fac)", "s(Station_fac)")
    
  }else{
    
    model<-gam(BPUE_log1p ~ te(SalSurf_s, doy_s, k=c(5,5), bs=c("cs", "cc")) + 
                 s(Year_fac, bs="re"),
               data=data, 
               method="REML")
    
    random_effects<-c("s(Year_fac)")
  }
  
  cat("-------------gam check-------------\n")
  gam.check(model)
  
  cat("\n\n-------------summary-------------\n")
  print(summary(model))
  
  sal<-predict_posterior(model, new_data, random_effects)%>%
    bind_cols(new_data%>% # Add covariate columns before these columns
                select(-doy_s, -SalSurf_s), 
              .)
  return(sal)
}

#Apply model to all groups and regions
model_factors<-expand_grid(IBMR=unique(zoop_data_mass$IBMR),
                           Subregion_edit=unique(zoop_data_mass$Subregion_edit))%>%
  mutate(IBMR=set_names(IBMR, paste(IBMR, Subregion_edit)))

sal_conversions<-pmap_dfr(model_factors, function(IBMR, Subregion_edit) sal_model(IBMR, Subregion_edit), .id = "IBMR_region")%>%
  mutate(IBMR=sapply(IBMR_region, function(x) str_split(x, " ", n=2)[[1]][1]),
         Region=factor(sapply(IBMR_region, function(x) str_split(x, " ", n=2)[[1]][2]),
                       levels=c("Confluence", "Suisun Marsh", "East Suisun", 
                                "NW Suisun", "SW Suisun")),
         Month=as.integer(Month))%>%
  select(-IBMR_region, -doy)%>%
  relocate(Region, Month, IBMR, SalSurf)

sal_conversions


# Save the data frame as an RDS file
#saveRDS(sal_conversions, file.path(path_hydro,"salinity_zooplankton_conversion.rds"))

# Save the objects as an RData file
#save(sal_base, model_factors, sal_conversions, zoop_data_mass, file.path(path_hydro,"salinity_zooplankton_conversion.RData"))

# Load imputation package
library(imputeTS)

#Read in salinity data

salinity_data<-read.csv("converted_salinity_data_SF2025.csv")


#Original file from CSAMP SDM
original_scenario_file<-read.csv(file.path("Data","base_salinity_data_for_X2salmodel.csv")) %>% select(c(2:5))  %>% rename(sal_base=sal) %>%
  add_row(year = 1997, month=1, region = "NE Suisun", sal_base=0.217891455)

#Join the two datasets
scenario_file <- original_scenario_file  %>% left_join(salinity_data)


# Apply model
# Load in the modeled salinity

scenario_names<-tibble(name=colnames(scenario_file))%>%
  filter(str_detect(name, "sal_"))%>%
  rev()

scenario_sal<-scenario_file%>%
  select(region, year, month, starts_with("sal_"))%>%
  mutate(across(c(year, month), as.integer),
         across(starts_with("sal_"), ~if_else(is.na(.x), sal_base, .x)))%>%
  filter(region%in%unique(zoop_data_mass$SUBREGION))%>%
  mutate(region=factor(region, 
                       levels=c("Confluence", "Suisun Marsh", "NE Suisun", 
                                "SE Suisun", "NW Suisun", "SW Suisun")))%>%
  pivot_longer(cols=starts_with("sal_"), names_to="Scenario", values_to="Salinity")%>% # Prepare data for easier plotting
  mutate(Scenario=factor(Scenario, 
                         levels=scenario_names$name),
         Salinity=round(Salinity, 1))

zoop_saladjusted<-scenario_sal%>%
  mutate(Salinity=as.character(Salinity),
         IBMR=unique(model_factors$IBMR)[1])%>%
  complete(region, year, month, Scenario, IBMR=unique(model_factors$IBMR))%>%
  group_by(region, year, month, Scenario)%>%
  mutate(Salinity=na.exclude(Salinity),
         region2=if_else(region%in%c("NE Suisun", "SE Suisun"), "East Suisun", as.character(region)))%>%
  ungroup()%>%
  left_join(sal_conversions%>%
              mutate(SalSurf=as.character(SalSurf)),
            by=c("region2"="Region",
                 "month"="Month",
                 "Salinity"="SalSurf",
                 "IBMR"="IBMR"))%>%
  select(-Salinity, -region2)%>%
  mutate(across(starts_with("draw_"), ~exp(.x)-1))%>%
  pivot_longer(starts_with("draw_"), names_prefix="draw_", names_to="draw", values_to="fit")%>%
  mutate(fit=if_else(fit<0, 0, fit))%>%
  pivot_wider(names_from="Scenario", values_from="fit")%>%
  mutate(across(starts_with("sal_"), ~.x/sal_base))%>%
  group_by(region, year, month, IBMR)%>%
  summarise(across(starts_with("sal_"), 
                   list(median=~median(.x, na.rm=T), 
                        l95=~quantile(.x, 0.025, na.rm=T), 
                        u95=~quantile(.x, 0.975, na.rm=T))), 
            .groups="drop")

#Load zooplankton model output and strata volume from Will Smith
model_data <- zoop_saladjusted
IBMR_data_format <- read.csv(file.path("Data","prey_taxa_inputs_IBMR.csv"))

# Impute infinity and N/A with linear interpolation
# When 0s were introduced for baseline predictions, it resulted in infinite values for the scalars. 
# We replaced these infinities with linear interpolations from the time series of model predictions for a specific alternative, taxon, and subregion. 
# This step yielded no more infinite scalar values.

## Impute data using linear interpolation

# Change inf and NaN to NA
model_data[sapply(model_data, is.infinite)] <- NA
model_data[sapply(model_data, is.nan)] <- NA

# Split data into list
zoop_model_data_arranged_median <- model_data %>% 
  arrange(year, month) %>%
  select("region","year","month","IBMR",ends_with("median")) %>%
  gather("scenario","value",ends_with("median"))

zoop_model_data_median_split <- split(zoop_model_data_arranged_median , list(zoop_model_data_arranged_median$region,zoop_model_data_arranged_median$IBMR))

# Impute data using linear interpolation
for(i in seq_along(zoop_model_data_median_split)){ 
  zoop_model_data_median_split[[i]]$value <- na_interpolation(zoop_model_data_median_split[[i]]$value)
}

# Rejoin into a single data frame
zoop_model_data_median_imputed<-bind_rows(zoop_model_data_median_split) %>%
  mutate(Date=as.Date(paste(year,month,"01",sep="-")))

ggplot(data=zoop_model_data_median_imputed %>% filter(region=="NW Suisun")) + geom_line(aes(x=Date,y=value,color=scenario),alpha=0.5) + theme_bw() + facet_wrap(~IBMR, scales = "free")

# Use pdiapfor prediction for pdiapjuv
# Remove mysid since it is not used
zoop_model_data_median_imputed <- zoop_model_data_median_imputed %>% filter(IBMR!="mysid")
str(zoop_model_data_median_imputed)

# Create pdiapjuv dataframe
pdiapjuv_data <- zoop_model_data_median_imputed %>% filter(IBMR=="pdiapfor") %>% mutate(IBMR=as.character("pdiapjuv"))

# Add pdiapjuv back into the main prediction data
zoop_model_data_median_imputed <- bind_rows(zoop_model_data_median_imputed,pdiapjuv_data)

# Format data to allow for easy input into IBMR and export

# Get the strata and taxa code in case they are necessary
IBMR_code <- IBMR_data_format %>% rename(year=Year,month=Month,region=Region,IBMR=Taxa) %>%
  select(year,month,region,IBMR,Strata_code,Taxa_code) %>% distinct()

# Spread zoop scalar data
zoop_model_data_median_imputed_spread <- spread(zoop_model_data_median_imputed,scenario,value)

# Add strata and taxa code
zoop_model_data_median_imputed_spread <- zoop_model_data_median_imputed_spread %>% left_join(IBMR_code)

# Remove years that are not present in IBMR input
zoop_model_data_median_imputed_spread <- zoop_model_data_median_imputed_spread %>% filter(!is.na(Strata_code))

# Export data
write.csv(zoop_model_data_median_imputed_spread,file=paste("zoop_scalar_output_SF2025_",Sys.Date(),".csv",sep=""))
