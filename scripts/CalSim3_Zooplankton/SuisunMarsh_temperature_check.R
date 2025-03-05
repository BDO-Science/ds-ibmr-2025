library(discretewq)
library(tidyverse)

# Script to estimate 'average' temperature at Belden's Landing for conversation to practical salinity unit
data_SuisunMarsh <- wq(
  Sources = c(
    "Suisun"
  )
)

# Grabe temperature data from Suisun Marsh Survey
# Station MZ6 is the 
tempdata <- data_SuisunMarsh %>% filter(!is.na(Temperature)&Station=="MZ6") %>%
  mutate(Month = month(Date)) %>% group_by(Month) %>% summarise(Temperature=mean(Temperature))

tempdata
#    1        9.51
#   2       11.5 
#    3       13.6 
#   4       17.2 
#    5       18.7 
#    6       21.7 
#    7       22.4 
#    8       22.9 
#    9       21.1 
#    10       18.6 
#    11       15.4 
#    12       11.2 