# =============================================================
# 02_ind_postweight.R
# =============================================================

# Run setup script which loads all required packages and functions and 
# executes the config.R script.
source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message("Execute ind postweight script")

# build SSCQ data
sscq_ind_list <- lapply(years, function(y) build_sscq(y, 
                                                     weight_var = "pooled_ind_wt_sc", 
                                                     varlist = varlist_ind, 
                                                     sscq_data))
names(sscq_ind_list) <- years

# Add message to inform user about progress
message("Calculate design weight for ind responses")

# calculate design weight
deff_ind_list <- lapply(seq_along(years), function(i) {
  compute_deffs(sscq_ind_list[[i]] %>% recode_birthcountry(), 
                varlist = varlist_ind, 
                weight_var = "pooled_ind_wt_sc")
})
names(deff_ind_list) <- years


# get total sample size
ntotal_ind <- sum(sapply(sscq_ind_list, function(d) nrow(d)))

# Effective n
n_eff <- sapply(deff_ind_list, function(d) round_half_up(sum(d$n[1] / mean(d$median_deff))))
y_factors <- n_eff / sum(n_eff)
names(y_factors) <- years

# Add message to inform user about progress
message("Calibrate ind weights")

# calibrate weights
ind_weights <- calibrate_weights(
  sscq_list      = sscq_ind_list,
  weight_var     = "pooled_ind_wt_sc",
  preweight_name = "ind_preweight1_sc",
  y_factors      = y_factors,
  nT             = ntotal_ind
)

# Add message to inform user about progress
message("Export ind weights")

# export
write.csv(ind_weights, 
          paste0(here('output'), "/ind_", 
                 year_suffix,
                 "_weights.csv"), 
          row.names = FALSE)
