# =============================================================
# 03_crime_postweight.R
# =============================================================

# Run setup script which loads all required packages and functions and 
# executes the config.R script.
source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message("Execute crime postweight script")

# build SSCQ data
sscqprev1_crim <- build_sscq(prev1_year, "pooled_crim_wt_sc", varlist_crim, sscq_data)
sscqcurrent_crim <- build_sscq(current_year, "pooled_crim_wt_sc", varlist_crim, sscq_data)

# calculate design weight
deff_prev1_crim <- compute_deffs(sscqprev1_crim, varlist_crim, "pooled_crim_wt_sc")
deff_current_crim <- compute_deffs(sscqcurrent_crim, varlist_crim, "pooled_crim_wt_sc")

# get sample sizes
nprev1 <- nrow(sscqprev1_crim %>% filter(pooled_crim_wt_sc > 0))
ncur <- nrow(sscqcurrent_crim %>% filter(pooled_crim_wt_sc > 0))
ntotal_crim <- nprev1+ncur

# Effective n
eff_prev1 <- janitor::round_half_up(sum(deff_prev1_crim$n[1] / mean(deff_prev1_crim$median_deff)))
eff_current <- janitor::round_half_up(sum(deff_current_crim$n[1] / mean(deff_current_crim$median_deff)))
eff_total = eff_prev1 + eff_current

# proportion of each year
y1fact <- eff_prev1 / (eff_prev1 + eff_current)
y2fact <- eff_current / (eff_prev1 + eff_current)

# calibrate weights
crim_weights <- calibrate_weights(
  df1 = sscqprev1_crim,
  df2 = sscqcurrent_crim,
  weight_var = "pooled_crim_wt_sc",
  preweight_name = "crim_preweight1_sc",
  y1 = y1fact,
  y2 = y2fact,
  nT = ntotal_crim
)

# export
save(crim_weights, file = paste0(here('output'), "/calibrated_crimividual.RData"))
