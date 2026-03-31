#########################################################################
# Name of file - SSCQ_multiyear_weighting.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.4.2
#
# This master script orchestrates the full SSCQ multiyear weighting
# workflow by sequentially running all component scripts responsible
# for:
#
#   1. Initial setup, configuration, and loading packages/functions.
#   2. Construction of multiyear household post-weights.
#   3. Construction of multiyear individual post-weights.
#   4. Construction of multiyear crime post-weights.
#   5. Age standardisation of the resulting weights and final export
#      of combined weighting outputs.
#
# The script assumes all data, functions, and configuration files are
# located in the project `scripts/` and `functions/` directories and 
# sourced using the {here} package for reproducible paths.
#
# Running this master script from start to finish will produce the
# full suite of SSCQ multiyear calibrated weights.
#########################################################################

### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.
install.packages("here")
library(here)

source(here("scripts", "00_setup.R"))


### 1 - Household postweight ----

# Creates multi-year household weights
source(here("scripts", "01_hh_postweight.R"))


### 2 - Individual postweight ----

# Creates multi-year individual weights
source(here("scripts", "02_ind_postweight.R"))


### 3 - Crime postweight ----

# Creates multi-year crime weights
source(here("scripts", "03_crime_postweight.R"))


### 4 - Age standardisation and export ----

# Standardises the ages and combines all weights
source(here("scripts", "04_combine_weights.R"))
