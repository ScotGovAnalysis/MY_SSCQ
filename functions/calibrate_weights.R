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
#' @param sscq_list 
#'    List containing all SSCQ data
#'
#' @param weight_var
#'   A string giving the name of the original weight to be calibrated,
#'   e.g. `"pooled_hh_wt_sc"` or `"pooled_ind_wt_sc"`.
#'
#' @param preweight_name
#'   A string giving the name of the final calibrated weight to be created,
#'   e.g. `"hh_preweight1_sc"` or `"ind_preweight1_sc"`.
#'
#' @param y_factors
#'    named numeric vector, e.g. c("2022"=0.33,"2023"=0.30,"2024"=0.37
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
#'       sscq_list =   list of yearly SSCQ datasets (already built),
#'       weight_var = "pooled_hh_wt_sc",
#'       preweight_name = "hh_preweight1_sc",
#'       y_factors = y_factors,
#'       nT = 39276
#'   )
#' }
#'
#' @export

calibrate_weights <- function(sscq_list, weight_var, preweight_name, y_factors, nT) {
  
  
  # Apply year factor to each dataset
  cal_list <- lapply(seq_along(sscq_list), function(i) {
    df <- sscq_list[[i]]
    yr <- as.character(df$year[1])      # extract year value inside dataset
    
    df %>%
      mutate(preweight0 = y_factors[yr] * .data[[weight_var]])
  })
  

  # Combine all years into a pooled dataset
  pooled <- bind_rows(cal_list)
  
  # Normalise the pooled weight to sum to nT
  pooled <- pooled %>%
    mutate(
      !!preweight_name := nT * (preweight0 / sum(preweight0))
    )
  
  return(pooled)
  
}

