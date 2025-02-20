# Code for the article "Possible Effects of the Judicial Redefinition of Rural 
# Property Size: A Case Study in the Legal Amazon" submitted to GeoInfo 2025

# This script requires a folder called amz_fc, which is available at
# https://figshare.com/articles/dataset/Forest_restoration_challenges_in_Brazilian_Amazonia_Data_and_code/22129325?file=39337139

require(dplyr)
require(terra)
require(sf)

###########################################################################
# Processing
###########################################################################

data <- terra::readRDS("amz_fc/tab/prodes/tab4id.rds") |>
  dplyr::filter(year == 2021) |>
  dplyr::filter(area_ha > 0) |> # 4 properties have area zero and therefore will be removed
  dplyr::mutate(old_class = dplyr::case_when(
    area_fm <= 4 ~ "small",
    area_fm > 4 & area_fm <= 15 ~ "medium",
    area_fm > 15 ~ "large"
  )) |>
  dplyr::mutate(lr_porc = (lr_area / lr_forest)) |>
  dplyr::mutate(new_fm = area_fm * (1 - lr_porc)) |>
  dplyr::mutate(new_class = dplyr::case_when(
    new_fm <= 4 ~ "small",
    new_fm > 4 & new_fm <= 15 ~ "medium",
    new_fm > 15 ~ "large"
  ))

dim(data) # 470.645 rural properties

sf::read_sf("amz_fc/shape/amz_land_tenure.shp") |>
  dplyr::inner_join(data, by = "id") |>
  sf::write_sf("result/car-legal-reserve.gpkg")

###########################################################################
# Plotting
###########################################################################

# Figure XX of the paper, histogram of legal reserve percentage
hist(data$lr_porc)

ticks <- c(0.1, 1, 10, 100)

log_v1 = log10(data$area_fm)
log_v2 = log10(result$new_fm)

breaks <- seq(floor(min(c(log_v1, log_v2), na.rm = T)), 
              ceiling(max(c(log_v1, log_v2), na.rm = T)), by = 0.25)

# Figure XX of the paper, histogram of area of rural properties in fiscal modules
hist1 <- hist(log_v1, breaks = breaks, col="blue", xaxt = "n")
axis(1, at = log10(ticks), labels = round(ticks, 1))

# Figure XX plotting two histograms of areas together
hist1 <- hist(log_v1, breaks = breaks, col="blue", xaxt = "n")
hist2 <- hist(log_v2, breaks = breaks, col=rgb(1, 0, 0, 0.5), add=T, xaxt = "n" )
axis(1, at = log10(ticks), labels = round(ticks, 1))

# Table with the changes in the size of the properties

before <- table(data$old_class)
after <- table(data$new_class)

mytable <- rbind(before, after, delta = after - before, diff_perc = round(after / before * 100) - 100)
mytable <- mytable[, 3:1]

kableExtra::kable(mytable, format = "latex")


uf_map <- tibble(
  uf_code = paste0(c(12, 13, 16, 15, 21, 51, 11, 14, 17)),
  sigla_uf = c("AC", "AM", "AP", "PA", "MA", "MT", "RO", "RR", "TO")
)

df <- sf::read_sf("result/car-legal-reserve.gpkg") |>
  sf::st_drop_geometry(shp) |>
  dplyr::left_join(uf_map, by = "uf_code")

result <- df |> 
  dplyr::group_by(sigla_uf, old_class, new_class) |>
  dplyr::summarise(n = n(), .groups = "drop") |>
  dplyr::filter(!is.na(new_class)) 


View(result)
