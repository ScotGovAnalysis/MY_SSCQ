#' Compute design effects (DEFFs) for SSCQ survey variables
#'
#' @description
#' The `compute_deffs()` function calculates design effects (DEFF) for a set of
#' categorical variables under a complex SSCQ survey design.  
#'
#' It is a direct R analogue of the SAS `%deffs` macro using:
#' * `svymean()` with `deff = TRUE`  
#' * `svydesign()` with cluster, strata, and weights  
#'
#' For each variable in `varlist`, the function:
#'   1. Fits a survey design with the specified weight, cluster, and strata.  
#'   2. Computes design effects for the distribution of that variable.  
#'   3. Extracts the per-category DEFF values.  
#'   4. Returns the **median DEFF** for the variable (matching the SAS logic).  
#'   5. Records the unweighted sample size (`n`) for that variable.  
#'
#' The output is typically used to compute *effective sample sizes* and  
#' subsequently year factors (`y1`, `y2`) for SSCQ multi-year weighting.
#'
#' @param df
#'   A tibble containing SSCQ microdata for a single year and weighting group.
#'   Must include the following columns:
#'     * `cluster` — primary sampling unit  
#'     * `LA`      — strata  
#'     * `{{weight_var}}` — analytic weight  
#'     * all variables listed in `varlist`  
#'
#' @param varlist
#'   A character vector of variable names for which DEFFs should be computed.
#'   Variables must be categorical or treatable as factors.
#'
#' @param weight_var
#'   A string naming the survey weight variable to use in the design
#'   (e.g., `"pooled_hh_wt_sc"` or `"pooled_ind_wt_sc"`).
#'
#' @return
#'   A tibble with one row per variable in `varlist`, containing:
#'     * `variable` — the variable name  
#'     * `median_deff` — the median design effect  
#'     * `n` — the unweighted sample size for that variable  
#'
#' @examples
#' \dontrun{
#'   varlist_hh <- c("htype", "htype2a", "tenure", "outten", "CarAccess")
#'   deff_2022_hh <- compute_deffs(
#'       df = sscq2022_hh,
#'       varlist = varlist_hh,
#'       weight_var = "pooled_hh_wt_sc"
#'   )
#' }
#'
#' @export

compute_deffs <- function(df, varlist, weight_var) {
  
  # Build the survey design object
  # nest = TRUE is necessary because SSCQ pooled samples may include
  # clusters that appear in more than one stratum across survey years.
  des <- df %>%
    filter(!!sym(weight_var) > 0) %>%
    as_survey_design(
      ids = cluster,
      strata = LA,
      weights = !!sym(weight_var)
    )
  
  # Loop over variables and compute DEFF using svymean()
  # For each variable:
  #   - convert to factor
  #   - calculate weighted means (proportions)
  #   - extract DEFF values from attribute "deff"
  #   - extract unweighted sample size from attribute "
  
  results <- map_df(varlist, function(v) {
    
    # Build formula for svymean (acts like PROC SURVEYFREQ OneWay)
    f <- reformulate(paste0("factor(", v, ")"))
    
    # Compute weighted proportions + DEFF + frequencies
    m <- svymean(f, des, deff = 'replace')
    freq <- svytable(reformulate(v), design = des)
    
    tibble(
      Table = v,
      Level = names(m),
      DesignEffect = as_tibble(m) %>% pull(deff),
      Frequency = as.numeric(freq)
    )
  })
  
  # Equivalent of PROC MEANS MEDIAN
  meds <- results %>%
    group_by(Table) %>%
    summarise(median_deff = median(DesignEffect, na.rm = TRUE), .groups = "drop")
  
  # Equivalent of PROC MEANS SUM
  sums <- results %>%
    group_by(Table) %>%
    summarise(n = sum(Frequency, na.rm = TRUE), .groups = "drop")
  
  # Equivalent of the DATA step MERGE
  final <- left_join(sums, meds, by = "Table")
  
  return(final)
}