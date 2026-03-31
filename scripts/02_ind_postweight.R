# =============================================================
# 02_ind_postweight.R
# =============================================================

# Run setup script which loads all required packages and functions and 
# executes the config.R script.
source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message("Execute ind postweight script")

# build SSCQ data
sscqprev1_ind <- build_sscq(prev1_year, "pooled_ind_wt_sc", varlist_ind, sscq_data) %>%
  recode_birthcountry
sscqcurrent_ind <- build_sscq(current_year, "pooled_ind_wt_sc", varlist_ind, sscq_data) %>%
  recode_birthcountry

# calculate design weight
deff_prev1_ind <- compute_deffs(sscqprev1_ind, varlist_ind, "pooled_ind_wt_sc")
deff_current_ind <- compute_deffs(sscqcurrent_ind, varlist_ind, "pooled_ind_wt_sc")

# get sample sizes
nprev1 <- nrow(sscqprev1_ind %>% filter(pooled_ind_wt_sc > 0))
ncur <- nrow(sscqcurrent_ind %>% filter(pooled_ind_wt_sc > 0))
ntotal_ind <- nprev1+ncur

# Effective n
eff_prev1 <- janitor::round_half_up(sum(deff_prev1_ind$n[1] / mean(deff_prev1_ind$median_deff)))
eff_current <- janitor::round_half_up(sum(deff_current_ind$n[1] / mean(deff_current_ind$median_deff)))
eff_total = eff_prev1 + eff_current

# proportion of each year
y1fact <- eff_prev1 / (eff_prev1 + eff_current)
y2fact <- eff_current / (eff_prev1 + eff_current)

# calibrate weights
ind_weights <- calibrate_weights(
  df1 = sscqprev1_ind,
  df2 = sscqcurrent_ind,
  weight_var = "pooled_ind_wt_sc",
  preweight_name = "ind_preweight1_sc",
  y1 = y1fact,
  y2 = y2fact,
  nT = ntotal_ind
)

# export
save(ind_weights, file = paste0(here('output'), "/calibrated_individual.RData"))
