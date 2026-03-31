# =============================================================
# 03_crime_postweight.R
# =============================================================

# Run setup script which loads all required packages and functions and 
# executes the config.R script.
source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message("Execute crime postweight script")

# build SSCQ data
sscq_crim_list <- lapply(years, function(y) build_sscq(y, 
                                                      weight_var = "pooled_crim_wt_sc", 
                                                      varlist = varlist_crim, 
                                                      sscq_data))
names(sscq_crim_list) <- years

# Add message to inform user about progress
message("Calculate design weight for crime responses")

# calculate design weight
deff_crim_list <- lapply(seq_along(years), function(i) {
  compute_deffs(sscq_crim_list[[i]], 
                varlist = varlist_crim, 
                weight_var = "pooled_crim_wt_sc")
})
names(deff_crim_list) <- years


# get total sample size
ntotal_crim <- sum(sapply(sscq_crim_list, function(d) nrow(d)))

# Effective n
n_eff <- sapply(deff_crim_list, function(d) round_half_up(sum(d$n[1] / mean(d$median_deff))))
y_factors <- n_eff / sum(n_eff)
names(y_factors) <- years

# Add message to inform user about progress
message("Calibrate crime weights")

# calibrate weights
crim_weights <- calibrate_weights(
  sscq_list      = sscq_crim_list,
  weight_var     = "pooled_crim_wt_sc",
  preweight_name = "crim_preweight1_sc",
  y_factors      = y_factors,
  nT             = ntotal_crim
)

# Add message to inform user about progress
message("Export crime weights")

# export
write.csv(crim_weights, 
          paste0(here('output'), "/crim_", year_suffix, "_weights.csv"), 
          row.names = FALSE)
