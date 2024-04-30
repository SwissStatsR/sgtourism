#' Data cleaning and saving as RDS object

library(readr)
library(dplyr)

df_ueberblick <- readr::read_delim(file = "data-raw/LN_HK_agg_mun.csv", delim = ";") |>
  mutate(Date = paste(Jahr, Monat, "1", sep = "-")) # create date

usethis::use_data(df_ueberblick, overwrite = TRUE)
