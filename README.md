# ds-ibmr-2025
Repository for IBMR (Individual Based Model - R) runs for 2025 Summer-Fall Habitat Action SDM

## data

* **data_raw/**: raw data
  * **demo_inputs/**: original model inputs
* **data_processed/**: data extracted from CalSim, processed through zooplankton model to be input into IBMR
  * OMR inputs: 
    * IBMR_OMR_SF2022MED_input.csv
    * IBMR_OMR_SF2025_input.csv
  * X2 inputs: 
    * IBMR_X2_SF2022MED_input.csv
    * IBMR_X2_SF2025_input.csv
  * Zooplankton inputs:
    * zoop_scalar_output_SF2022MED_2025-03-13.csv
    * zoop_scalar_output_SF2025_2025-03-13.csv

## scripts

* **altx_IBMR_control.model_v2.4_parallel.R**: script for running IBMR in parallel mode - there is one for each alternative 
* **Delta smelt data functions_v9_2.R**: functions for running IBMR
* **CalSim3_Zooplankton/**: CalSim data and code to convert to IBMR inputs

## output

* **model_outputs/**: output files from IBMR
  * **outputs_2022MED**: output from 2022 Median hydrology
  * **outputs_AdjHist**: output from Adjusted Historical hydrology
  * **summarized_output**: summarized abundance and lambda output data
* **figures/**: figure files from IBMR outputs

## docs

* **IBMR README**: Documentation on demo run for IBMR (probably will remove this later and have our own README)
* **IBMR documentation_v4_2**: Documentation on IBMR
* **Explore inputs and outputs here**: [IBMR inputs and outputs](https://bdo-science.github.io/ds-ibmr-2025/)