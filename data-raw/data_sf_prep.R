library(sf)
library(dplyr)
library(stringr)

# reading and joining spatial data -----
url <- "https://data.geo.admin.ch/ch.swisstopo.swissboundaries3d/swissboundaries3d_2024-01/swissboundaries3d_2024-01_2056_5728.shp.zip"
temp_file <- tempfile(fileext = ".zip")
temp_dir <- tempdir()
curl::curl_download(url, temp_file)
path_dir_unzip <- unzip(temp_file, exdir = temp_dir)

mun <- sf::st_read(paste0(temp_dir,"/swissBOUNDARIES3D_1_5_TLM_HOHEITSGEBIET.shp"))|>
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") |>
  sf::st_zm() # drop z coordinates

kt <- sf::st_read(paste0(temp_dir,"/swissBOUNDARIES3D_1_5_TLM_KANTONSGEBIET.shp"))|>
  sf::st_transform(crs = "+proj=longlat +datum=WGS84")|>
  sf::st_zm()

key.dest <- read.csv("data-raw/key_dest_2024_10.csv", sep = ";") |>
  dplyr::rename(BFS_NUMMER = bfs_nr)

dest_mun <- dplyr::left_join(mun, key.dest) |>
  dplyr::filter(dest_cd %in% c("FH", "SGBS", "TB", "ZS"))

dest <- dest_mun |>
  summarise(geometry = st_union(geometry), .by = c(dest_cd, dest_nr, dest_name)) |>
  dplyr::filter(dest_cd %in% c("FH", "SGBS", "TB", "ZS")) |>
  select(dest_nr, dest_name, geometry) |>
  mutate(dest_name = stringr::str_replace_all(dest_name, "ü", "\u00fc"))

dest_sub <- dest_mun |>
  summarise(geometry = st_union(geometry), .by = c(dest_cd, dest_nr, dest_name, aggregat, aggregat_name)) |>
  dplyr::filter(dest_cd %in% c("FH", "SGBS", "TB", "ZS")) |>
  select(dest_nr, aggregat_name, geometry) |>
  mutate(aggregat_name = stringr::str_replace_all(aggregat_name, "ü", "\u00fc"))

# select only necessary variables
dest_mun <- dest_mun |>
  select(dest_nr, NAME, geometry) |>
  mutate(NAME = stringr::str_replace_all(NAME, "ü", "\u00fc"))

# get bounding box ----
bbox_perimeter <- sf::st_bbox(dest_mun) |>
  as.vector()

# simplify sf objects to reduce size
dest_mun <- st_simplify(dest_mun, preserveTopology = TRUE, dTolerance = 100)
dest <- st_simplify(dest, preserveTopology = TRUE, dTolerance = 100)
dest_sub <- st_simplify(dest_sub, preserveTopology = TRUE, dTolerance = 100)

usethis::use_data(dest_mun, overwrite = TRUE)
usethis::use_data(dest, overwrite = TRUE)
usethis::use_data(dest_sub, overwrite = TRUE)
usethis::use_data(bbox_perimeter, overwrite = TRUE)
