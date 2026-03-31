#' Apply age-standardisation to SSCQ post-weighted survey data
#'
#' @description
#' The `age_standardise()` function produces age-standardised survey
#' weights for SSCQ (Scottish Surveys Core Questions) data at a chosen
#' geographic level.  
#'
#' It performs the following steps:
#'   1. Sets up a complex survey design using cluster, LA strata, and the
#'      supplied base weight (with `nest = TRUE` to allow multi-year SSCQ).
#'   2. Computes weighted age-group proportions for each geographic area
#'      using `svyby()` + `svymean()` (stable and efficient for large data).
#'   3. Reshapes SAPE population age distributions into long format.
#'   4. Joins survey and SAPE proportions by (geotype × ageG).
#'   5. Computes standardisation factors:
#'           `factor = sapeProp / surveyProp`
#'   6. Applies the factor to the supplied base weight to create a new,
#'      age-standardised weight column with name:
#'           `<weight_var>_<geotype>`
#'
#' @param df
#'   A tibble containing SSCQ microdata for one or more years.  
#'   Must include columns:
#'     * `cluster`  — primary sampling unit  
#'     * `LA`       — local authority (strata)  
#'     * `ageG`     — age group (coded 1–7)  
#'     * `sscqID`   — respondent ID  
#'     * `{{geotype}}` — the chosen geography variable  
#'     * `{{weight_var}}` — the base analytic weight to adjust  
#'
#' @param geotype
#'   A string giving the name of the geographic classification variable
#'   (e.g., `"ukParlCon"`, `"sParlCon14"`).  
#'   This variable must be present in `df` and in `sape_df`.
#'
#' @param weight_var
#'   A string giving the name of the base weight to be age-standardised,
#'   such as `"ind_preweight1_sc"`, `"crim_preweight1_sc"`, etc.
#'
#' @param sape_df
#'   A tibble containing SAPE (Small Area Population Estimates) age
#'   distributions for the same geographic level as `geotype`.  
#'   Expected to include one row per area and columns:
#'       `a16-24`, `a25-34`, `a35-44`, `a45-54`,
#'       `a55-64`, `a65-74`, `a75plus`
#'
#' @return
#'   A tibble identical to `df` but with one additional column:
#'
#'       `<weight_var>_<geotype>`
#'
#'   containing the age-standardised weight appropriate for the chosen
#'   geography.
#'
#' @examples
#' \dontrun{
#'   ind_std <- age_standardise(
#'      df       = ind_weights,
#'      geotype  = "ukParlCon",
#'      weight_var = "ind_preweight1_sc",
#'      sape_df  = ukParlCon_agebase
#'   )
#' }
#'
#' @export

age_standardise <- function(df, geotype, weight_var, sape_df) {
  
  # 1. Build survey design ----
  # nest = TRUE is required because clusters are not guaranteed to be
  # unique within strata across pooled SSCQ survey years.
  design <- df %>%
    as_survey_design(ids = cluster, strata = LA, weights = !!sym(weight_var), nest = TRUE)
  
  # Convert grouping variables to factors for svyby()
  design$variables[[geotype]] <- as.factor(design$variables[[geotype]])
  design$variables$ageG <- as.factor(design$variables$ageG)
  
  # 2. Compute weighted survey age proportions by (geotype × ageG) ----
  # We use svyby + svymean because it is fast and avoids performance
  # problems seen with srvyr::survey_prop() on large SSCQ files.
  survey_age <- svyby(
    ~ageG,
    by = design$variables[[geotype]],
    design = design,
    FUN = svymean,
    keep.names = TRUE,
    deff = FALSE
  )
  
  # Convert wide columns ageG1, ageG2, ... into long format
  survey_age <- survey_age %>%
    as_tibble() %>%
    pivot_longer(
      cols = starts_with("ageG"),
      names_to = "ageG",
      values_to = "p"
    ) %>%
    mutate(ageG = as.integer(str_remove(ageG, "ageG")),
           by = as.character(by)) %>%
    rename(!!geotype := by)
  
  
  # 3. Prepare SAPE age structure ----
  # SAPE file is assumed wide (a16-24, a25-34, ...). We reshape it.
  sape_long <- sape_df %>%
    pivot_longer(starts_with("a"), names_to="ageband", values_to="sapeProp") %>%
    mutate(ageG = case_when(
      ageband == "a16-24" ~ 1,
      ageband == "a25-34" ~ 2,
      ageband == "a35-44" ~ 3,
      ageband == "a45-54" ~ 4,
      ageband == "a55-64" ~ 5,
      ageband == "a65-74" ~ 6,
      ageband == "a75+" ~ 7
    ))
  
  
  # 4. Merge survey proportions with SAPE proportions ----
  # Join by (geotype, ageG)
  joined <- survey_age %>%
    left_join(sape_long, by=c(geotype, "ageG")) %>%
    mutate(factor = sapeProp / p)
  
  
  # 5. Apply factor to base weight for each respondent ----
  # Resulting weight name is: "<weight_var>_<geotype>"
  df %>%
    left_join(joined %>% select(!!sym(geotype), ageG, factor),
              by = c(geotype,"ageG")) %>%
    mutate(!!paste0(sub("_.*$", "", weight_var), 
                    prev1_year %% 100,
                    current_year %% 100,
                    "_", 
                    geotype) := factor * !!sym(weight_var))
}
