# =============================================================
# 01_hh_postweight.R
# =============================================================

# Run setup script which loads all required packages and functions and 
# executes the config.R script.
source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message("Execute household postweight script")

# build SSCQ data
sscq_hh_list <- lapply(years, function(y) build_sscq(y, 
                                                  weight_var = "pooled_hh_wt_sc", 
                                                  varlist = varlist_hh, 
                                                  sscq_data))
names(sscq_hh_list) <- years

# Add message to inform user about progress
message("Calculate design weight for hh responses")

# calculate design weight
deff_hh_list <- lapply(seq_along(years), function(i) {
  compute_deffs(sscq_hh_list[[i]], 
                varlist = varlist_hh, 
                weight_var = "pooled_hh_wt_sc")
})
names(deff_hh_list) <- years


# get total sample size
ntotal_hh <- sum(sapply(sscq_hh_list, function(d) nrow(d)))

# Effective n
n_eff <- sapply(deff_hh_list, function(d) round_half_up(sum(d$n[1] / mean(d$median_deff))))
y_factors <- n_eff / sum(n_eff)
names(y_factors) <- years

# Add message to inform user about progress
message("Calibrate hh weights")

# calibrate weights
hh_weights <- calibrate_weights(
  sscq_list      = sscq_hh_list,
  weight_var     = "pooled_hh_wt_sc",
  preweight_name = "hh_preweight1_sc",
  y_factors      = y_factors,
  nT             = ntotal_hh
)

# Add message to inform user about progress
message("Export hh weights")

# export
write.csv(hh_weights, 
          paste0(here('output'), "/hh_", year_suffix, "_weights.csv"), 
          row.names = FALSE)

