# =============================================================
# 00_setup.R
# Loads all data required libraries and functions for the
# SSCQ multiyear weighting process
# =============================================================

### 1 - Load packages ----

library(haven)
library(readr)
library(tidyverse)
library(tidyr)
library(stringr)
library(rlang)
library(survey)
library(srvyr)
library(janitor)
library(here)

### 2 - Load functions from functions folder of SHS Weighting RAP ----

walk(list.files(here("functions"), pattern = "\\.R$", full.names = TRUE), 
     source)

### 3 - Load config file from code folder of SHS Weighting RAP ----

# The config.R script is the only file which needs to be updated before 
# the RAP can be run. 

source(here("scripts", "config.R"))

### 4 - Create folders ----

# If output folders for syear specified above 
# don't already exist, create folders

folders <- paste0(
  here("output")
)

walk(folders,
     ~ if(!file.exists(.x)) dir.create(.x, recursive = TRUE)
)


  