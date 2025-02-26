# ds-ibmr-2025
Repository for IBMR (Individual Based Model - R) runs for 2025 Summer-Fall Habitat Action SDM

## data

* **data_raw/**: raw data
  * list specific files here
* **data_processed/**: data extracted from CalSim, processed through zooplankton model to be input into IBMR
  * naming: **altx_shortdescription_input.csv**, e.g. *alt1_max-ds-even_input.csv*
  * list specific files here if useful to describe what values were modified

## scripts

* **IBMR_control.model_v2.4_parallel.R**: script for running IBMR in parallel mode 
* **Delta smelt data functions_v9_2.R**: functions for running IBMR
* **run_ibmr.R** (this is what I hope our cleaned up script will be named)

## output

* **model_outputs/**: output files from IBMR
  * naming: **altx_shortdescription_output.Rds**, e.g. *alt1_max-ds-even_output.Rds*
* **figures/**: figure files from IBMR outputs
  * list specific files here

## docs

* **IBMR README**: Documentation on demo run for IBMR (probably will remove this later and have our own README)
* **IBMR documentation_v4_2**: Documentation on IBMR