
# ---- Set paths (adjust as needed) ----

datashare <- filepath
sasdata <- filepath
sape_path <- filepath

years <- c(20XX, 20XX, ...)

# ---- Load microdata ----

sscq_data <- list(
  sscq_20XX = read_sas(file.path(sasdata, "FILENAME.sas7bdat")),
  sscq_20XX = read_sas(file.path(sasdata, "FILENAME.sas7bdat")),
  xref_20XX = read_sas(file.path(sasdata, "FILENAME.sas7bdat")),
  xref_20XX = read_sas(file.path(sasdata, "FILENAME.sas7bdat")),
  geo_dz11  = read_csv(file.path(datashare, "FILENAME.csv"), show_col_types = FALSE) %>% rename(datazone = DZ11),
  geo_dzold = read_csv(file.path(datashare, "FILENAME.csv"), show_col_types = FALSE)
)

# ---- Load SAPE age base files when needed ----
# (Used later in age standardisation script)

sape_files <- list(
  ukParlCon_age = read_csv(file.path(datashare, "FILENAME.csv"), show_col_types = FALSE),
  sParlCon14_age = read_csv(file.path(datashare, "FILENAME.csv"), show_col_types = FALSE)
)

# ---- Specify variables ----

varlist_hh  <- c("VARNAME", "VARNAME", ...)

varlist_ind <- c("VARNAME", "VARNAME", ...)

varlist_crim <- c("VARNAME", "VARNAME", ...)


# ---- Birth Country recoding ----
# (R gets stuck otherwise due to lagre number of categories)

recode_birthcountry <- function(df, var = BirthCountry) {
  df %>%
    mutate({{ var }} :=
             case_when(
               {{ var }} %in% c(0:371, 9999) ~ "Other",
               {{ var }} %in% 372 ~ "Republic of Ireland",
               {{ var }} %in% 373:920 ~ "Other",
               {{ var }} %in% 921 ~ "England",
               {{ var }} %in% 922 ~ "Northern Ireland",
               {{ var }} %in% 923 ~ "Scotland",
               {{ var }} %in% 924 ~ "Wales",
               {{ var }} %in% 925:998 ~ "Other",
               {{ var }} == 1001 ~ "SHS Missing",
               {{ var }} %in% c(-1, -2) ~ "DK/Refused",
               TRUE ~ as.character({{ var }})
             )
    )
}


