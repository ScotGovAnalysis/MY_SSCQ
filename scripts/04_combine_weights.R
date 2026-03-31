# =============================================================
# 04_combine_weights.R
# Combine household, individual and crime calibrated weights
# into a unified 'simplex' weighting file.
# =============================================================

# Run setup script which loads all required packages and functions and 
# executes the config.R script.
source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message("Execute combine weights script")

# COMBINE LA WEIGHTS ----

simplex_wt <- hh_weights %>%
  
  # Select required variables
  select(SSCQid, year, hh_preweight1_sc, elward, sParlCon14, ukParlCon, LA) %>%
  
  # Add individual weights
  # Using full_join ensures that respondents present in one module
  # but not another are still retained (SSCQ modules have different
  # eligibility patterns).
  full_join(
    ind_weights %>% 
      select(SSCQid, year, ind_preweight1_sc, elward, sParlCon14, ukParlCon, LA),
    by = c("SSCQid", "year", "elward", "sParlCon14", "ukParlCon", "LA")
  ) %>%
  
  # Add crime weights
  full_join(
    crim_weights %>% 
      select(SSCQid, year, crim_preweight1_sc, elward, sParlCon14, ukParlCon, LA),
    by = c("SSCQid", "year", "elward", "sParlCon14", "ukParlCon", "LA")
  ) %>%
  
  # Rename weights to include year labels in the final naming format:
  #   hh<YY1><YY2>_LA
  #   ind<YY1><YY2>_LA
  #   crim<YY1><YY2>_LA
  #
  # prev1_year and current_year come from your config and represent
  # the two pooled years (e.g., 22 and 23).
  rename(
    !!paste0("hh", prev1_year, current_year, "_LA")   := hh_preweight1_sc,
    !!paste0("ind", prev1_year, current_year, "_LA")  := ind_preweight1_sc,
    !!paste0("crim", prev1_year, current_year, "_LA") := crim_preweight1_sc
  )

# Store dynamic names for convenience
w_hh   <- paste0("hh", prev1_year, current_year, "_LA")
w_ind  <- paste0("ind", prev1_year, current_year, "_LA")
w_crim <- paste0("crim", prev1_year, current_year, "_LA")


# Summary of the three main weight variables — basic integrity check
#
# The summarise() call calculates:
#   - N: number of non-missing weights
#   - sum: total weight value
#
# These must be consistent before proceeding.
simple_summary <- simplex_wt %>%
  summarise(
    across(all_of(c(w_hh, w_ind, w_crim)),
           list(N = ~sum(!is.na(.)), sum = ~sum(., na.rm = TRUE)))
  ) 


# Paired column consistency check:
#
# Checking that:
#   w_hh_N == w_ind_N == w_crim_N
#   w_hh_sum == w_ind_sum == w_crim_sum
#
# mapply(`==`, ...) compares the 1st, 3rd, 5th elements (Ns)
# with the 2nd, 4th, 6th (sums). If they all match, continue.
checks <- mapply(`==`,
                 simple_summary[c(1,3,5)],
                 simple_summary[c(2,4,6)])


if(all(checks)) {
  message("All paired columns match exactly. Proceed...")
} else {
  stop(print('Paired columns do not match when combining weights - please investigate.'))
}


# AGE STANDARDISATION ----
#
# Apply age-standardisation separately for:
#   - individual weights by UK Parliamentary Constituency
#   - individual weights by Scottish Parliamentary Constituency (2014 map)
#   - crime weights by both geographies
#
# Output columns are automatically named:
#   <weight_var>_<geotype>
# e.g. "ind_preweight1_sc_ukParlCon"

message("Standardise ages")

ind_ukPC_std <- age_standardise(
  df = ind_weights,       
  geotype = "ukParlCon",
  weight_var = "ind_preweight1_sc",
  sape_df = sape_files$ukParlCon_age
)

ind_sPC_std <- age_standardise(
  df = ind_weights,       
  geotype = "sParlCon14",
  weight_var = "ind_preweight1_sc",
  sape_df = sape_files$sParlCon14_age
)

crim_ukPC_std <- age_standardise(
  df = crim_weights,       
  geotype = "ukParlCon",
  weight_var = "crim_preweight1_sc",
  sape_df = sape_files$ukParlCon_age
)

crim_sPC_std <- age_standardise(
  df = crim_weights,       
  geotype = "sParlCon14",
  weight_var = "crim_preweight1_sc",
  sape_df = sape_files$sParlCon14_age
)


# COMBINE ALL FINAL WEIGHTS ----

# Merge:
#   - pooled HH / IND / CRIM LA-level weights   (simplex_wt)
#   - IND UKParlCon-standardised weights
#   - IND sParlCon14-standardised weights
#   - CRIM UKParlCon-standardised weights
#   - CRIM sParlCon14-standardised weights
# Select all final variables and order nicely.

final_weights <- simplex_wt %>%
  full_join(ind_ukPC_std %>% select(SSCQid, ends_with('_ukParlCon')), by = 'SSCQid') %>%
  full_join(ind_sPC_std %>% select(SSCQid, ends_with('_sParlCon14')), by = 'SSCQid') %>%
  full_join(crim_ukPC_std %>% select(SSCQid, ends_with('_ukParlCon')), by = 'SSCQid') %>%
  full_join(crim_sPC_std %>% select(SSCQid, ends_with('_sParlCon14')), by = 'SSCQid') %>%
  select(SSCQid, LA, year, ukParlCon, sParlCon14, elward, everything())

# EXPORT FINAL WEIGHT FILE ----

# Output filename format:
#   SSCQ_<prev-year><current-year>wts.csv
# Example:
#   SSCQ_2223wts.csv

message("Export weights")

write.csv(final_weights, paste0(here('output'), "/SSCQ_", prev1_year %% 100,
                                current_year %% 100, "wts.csv"), 
          row.names = FALSE)


