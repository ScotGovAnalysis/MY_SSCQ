# =============================================================
# 01_hh_postweight.R
# =============================================================

# Run setup script which loads all required packages and functions and 
# executes the config.R script.
source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message("Execute household postweight script")

# build SSCQ data
sscqprev1_hh <- build_sscq(prev1_year, "pooled_hh_wt_sc", varlist_hh, sscq_data)
sscqcurrent_hh <- build_sscq(current_year, "pooled_hh_wt_sc", varlist_hh, sscq_data)

# calculate design weight
deff_prev1_hh <- compute_deffs(sscqprev1_hh, varlist_hh, "pooled_hh_wt_sc")
deff_current_hh <- compute_deffs(sscqcurrent_hh, varlist_hh, "pooled_hh_wt_sc")

# get sample sizes
nprev1 <- nrow(sscqprev1_hh %>% filter(pooled_hh_wt_sc > 0))
ncur <- nrow(sscqcurrent_hh %>% filter(pooled_hh_wt_sc > 0))
ntotal_hh <- nprev1+ncur

# Effective n
eff_prev1 <- janitor::round_half_up(sum(deff_prev1_hh$n[1] / mean(deff_prev1_hh$median_deff)))
eff_current <- janitor::round_half_up(sum(deff_current_hh$n[1] / mean(deff_current_hh$median_deff)))
eff_total = eff_prev1 + eff_current

# proportion of each year
y1fact <- eff_prev1 / (eff_prev1 + eff_current)
y2fact <- eff_current / (eff_prev1 + eff_current)

# calibrate weights
hh_weights <- calibrate_weights(
  df1 = sscqprev1_hh,
  df2 = sscqcurrent_hh,
  weight_var = "pooled_hh_wt_sc",
  preweight_name = "hh_preweight1_sc",
  y1 = y1fact,
  y2 = y2fact,
  nT = ntotal_hh
)

# export
save(hh_weights, file = paste0(here('output'), "/calibrated_household.RData"))
