
#' Build SSCQ analysis dataset for a specific year and domain
#'
#' @description
#' The `build_sscq()` function replicates the behaviour of the SAS `%databuild`
#' macro. It creates a clean, merged, analysis-ready SSCQ dataset for a given
#' year (e.g., 2022, 2023) and weighting group (hh, ind, crim, care).  
#'
#' The function:
#'   * selects required variables from the SSCQ core dataset  
#'   * attaches cluster information from the corresponding XREF file  
#'   * joins DZ11 geographical lookup information  
#'   * harmonises variable names and derives the full survey year  
#'   * keeps only observations with non-missing weights  
#'
#' @param year  
#'   A character string or numeric year identifier (e.g., `"2022"` or `2022`).
#'   This must match the names in the `sscq_data` list (e.g. `"sscq_2022"` and
#'   `"xref_2022"`).
#'
#' @param group  
#'   A character label describing the weighting group (e.g. `"hh"`, `"ind"`).  
#'   Used only for naming and organisation; not required for computation.
#'
#' @param weight_var  
#'   Name of the analytic weight column in the SSCQ dataset (e.g.
#'   `"pooled_hh_wt_sc"`, `"pooled_ind_wt_sc"`). Only rows where this is
#'   non-missing are included.
#'
#' @param varlist  
#'   A character vector of variable names to extract from the SSCQ dataset for
#'   calibration and DEFF analysis.
#'
#' @param sscq_data  
#'   A named list containing all SSCQ files loaded in advance (as created in
#'   `01_load_data.R`). Must contain:
#'   - `sscq_<year>` : main SSCQ dataset  
#'   - `xref_<year>` : cluster linking file  
#'   - `geo_dz11`    : DZ11 → geography lookup  
#'
#' @return  
#' A tibble containing:
#'   * SSCQ microdata for the requested year  
#'   * merged cluster and DZ11 lookup information  
#'   * a `year` column of form `2022`, `2023`, etc.  
#'   * only the essential variables required for DEFF and calibration  
#'
#' @examples
#' \dontrun{
#' build_sscq("2022", "hh", "pooled_hh_wt_sc", varlist_hh, sscq_data)
#' }
#'
#' @export

build_sscq <- function(year, weight_var, varlist, sscq_data) {
  
  # Retrieve datasets from the loaded list
  base <- sscq_data[[paste0("sscq_", year)]]
  xref <- sscq_data[[paste0("xref_", year)]]
  geo <- sscq_data[[paste0("geo_", year)]]
  
  # Add proper full year, rename urban rural, and select required columns
  base <- base %>%
    mutate(year = as.integer(year))  %>%
    {
      if (all(c("UrbRur13Code", "UrbRur20Code") %in% names(.))) {
        # If BOTH exist, drop the 2013 version
        select(., -UrbRur13Code)
      } else {
        .
      }
    } %>%
    rename_with(~ "UrbRurCode",
                .cols = any_of(c("UrbRur20Code", "UrbRur13Code"))) %>%
    select(SSCQid, LA, UrbRurCode, all_of(varlist), !!sym(weight_var), year)
  
  # Merge SSCQ & XREF to obtain cluster + DZ11 code AND merge dz11 geography lookup
  # Filter out missing weight rows
  df <- base %>%
    left_join(xref %>% select(SSCQid, cluster, dz11), by = "SSCQid") %>%
    left_join(geo, by = "dz11") %>%
    filter(!!sym(weight_var) > 0)
    
  return(df)
}
