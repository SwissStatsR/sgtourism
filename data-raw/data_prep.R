#' Data cleaning and saving as RDS object

library(readr)
library(dplyr)

df_ueberblick <- readr::read_delim(file = "data-raw/LN_HK_agg_mun.csv", delim = ";") |>
  mutate(Date = paste(Jahr, Monat, "1", sep = "-")) # create date

usethis::use_data(df_ueberblick, overwrite = TRUE)

lookup_aggregat <- readr::read_csv(file = "data-raw/lookup_aggregat.csv")

usethis::use_data(lookup_aggregat, overwrite = TRUE)

meta_countries <- readr::read_csv(file = "data-raw/meta_countries.csv")

usethis::use_data(meta_countries, overwrite = TRUE)
