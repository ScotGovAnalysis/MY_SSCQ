#' Calibrate SSCQ pooled weights across multiple years
#'
#' @description
#' The `calibrate_weights()` function implements the SAS logic used in
#' multi-year SSCQ weighting, where separate survey years contribute to a
#' pooled dataset with weights scaled according to their *effective sample
#' size share*.  
#'
#' The function:
#'   1. Combines two SSCQ years into a single dataset.
#'   2. Applies the year factor (`y1` for year 1, `y2` for year 2) to the
#'      base weight (`weight_var`) to produce `preweight0`.
#'   3. Normalises `preweight0` so that the sum of the resulting weight
#'      equals the pooled target sample size `nT`.
#'   4. Returns the dataset with a final calibrated weight:
#'         `<preweight_name>`
#'
#' This is the direct R equivalent of the SAS code:
#'   `&nT * (preweight0 / sum(preweight0))`
#'
#' @param df22
#'   Tibble containing SSCQ microdata for the first year (e.g., 2022).
#'
#' @param df23
#'   Tibble containing SSCQ microdata for the second year (e.g., 2023).
#'
#' @param weight_var
#'   A string giving the name of the original weight to be calibrated,
#'   e.g. `"pooled_hh_wt_sc"` or `"pooled_ind_wt_sc"`.
#'
#' @param preweight_name
#'   A string giving the name of the final calibrated weight to be created,
#'   e.g. `"hh_preweight1_sc"` or `"ind_preweight1_sc"`.
#'
#' @param y1
#'   Numeric year factor representing the share of the *effective sample size*
#'   for the first year.
#'
#' @param y2
#'   Numeric year factor representing the share of the *effective sample size*
#'   for the second year.
#'
#' @param nT
#'   Numeric total sample size for the pooled period, used to normalise the
#'   preweights so that they sum to `nT`.
#'
#' @return
#'   A tibble combining the two years and containing a new calibrated weight
#'   variable:
#'
#'       `<preweight_name>`
#'
#'   whose sum equals `nT`.
#'
#' @examples
#' \dontrun{
#'   hh_weights <- calibrate_weights(
#'       df22 = sscq2022_hh,
#'       df23 = sscq2023_hh,
#'       weight_var = "pooled_hh_wt_sc",
#'       preweight_name = "hh_preweight1_sc",
#'       y1 = 0.51,
#'       y2 = 0.49,
#'       nT = 39276
#'   )
#' }
#'
#' @export

calibrate_weights <- function(df1, df2, weight_var, preweight_name, y1, y2, nT) {
  
  bind_rows(
    # Pooled multi-year SSCQ datasets must be combined before computing
    # normalisation. We attach the appropriate year factor to each.
    df1 %>% mutate(preweight0 = y1 * !!sym(weight_var)),
    df2 %>% mutate(preweight0 = y2 * !!sym(weight_var))
  ) %>%
    
    mutate(
      # Normalise so that the pooled weight sums to nT
      preweight1 = nT * (preweight0 / sum(preweight0))
    ) %>%
    rename(!!preweight_name := preweight1)
}