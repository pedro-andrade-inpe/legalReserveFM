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

sf::sf_use_s2(FALSE)

# properties in the border of two states were split in the original data
# here we join them so that there is only a single property with a given rid
shp <- sf::read_sf("amz_fc/shape/amz_land_tenure.shp") |>
  dplyr::group_by(rid) |>
  summarize(geometry = sf::st_union(geometry), .groups = "drop")

sf::write_sf(shp, "amz_land_tenure_union.gpkg")

shp <- sf::read_sf("amz_land_tenure_union.gpkg")

data <- terra::readRDS("amz_fc/tab/prodes/tab4id.rds") |>
  dplyr::filter(year == 2021) |>
  dplyr::mutate(rid = id) |>
  dplyr::select(-area_ha) # we will recompute the area

join <- dplyr::left_join(shp, data, by = "rid") |>
  dplyr::mutate(area_ha = units::set_units(sf::st_area(geom), "ha")) |>
  dplyr::mutate(old_class = dplyr::case_when(
    area_fm <= 4 ~ "small",
    area_fm > 4 & area_fm <= 15 ~ "medium",
    area_fm > 15 ~ "large",
    is.na(area_fm) ~ "protected"
  )) |>
  # prot_forest => area of a property that belongs to an area of integral
  # protection, so we dont use it
  dplyr::mutate(lr_porc = (lr_area / lr_forest)) |>
  dplyr::mutate(new_fm = area_fm * (1 - lr_porc)) |>
  dplyr::mutate(new_class = dplyr::case_when(
    new_fm <= 4 ~ "small",
    new_fm > 4 & new_fm <= 15 ~ "medium",
    new_fm > 15 ~ "large",
    is.na(area_fm) ~ "protected"
  ))

# remove all the data that cannot be used to our analysis
result <- join |>
  dplyr::filter(old_class != "protected") |>
  dplyr::filter(!is.na(lr_porc)) |>
  dplyr::filter(area_fm > 0) |>
  dplyr::filter(new_fm > 0)

sf::write_sf(result, "result-car-legal-reserve.gpkg")

###########################################################################
# Plotting
###########################################################################

require(ggplot2)

result <- sf::read_sf("result-car-legal-reserve.gpkg")

### Figure 2, histogram of legal reserve percentage

pdf("legal-reserve-percentage.pdf", width = 4.5, height = 3)
ggplot(result, aes(lr_porc)) +
  geom_histogram(color = "#000000", fill = "#2c7fb8", bins = 16) +
  labs(
    x = "Legal reserve percentage",
    y = "Number of properties"
  ) +
  theme(
    text = element_text(size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )
dev.off()
  
### Figure 3, histogram of legal reserve percentage

ticks <- c(0.001, 0.01, 0.1, 1, 10, 100)
log_v1 <- log10(result$area_fm)
log_v2 <- log10(result$new_fm)

length(which(log_v2 < 0))


breaks <- seq(floor(min(c(log_v1, log_v2), na.rm = TRUE)), 
              ceiling(max(c(log_v1, log_v2), na.rm = TRUE)), by = 0.25)

data_combined <- data.frame(
  value = c(log_v1, log_v2),
  group = rep(c("area_fm", "new_fm"), c(length(log_v1), length(log_v2))))

pdf("histogram-fiscal-modules.pdf", width = 5.5, height = 3)

ggplot(data_combined, aes(x = value, fill = group)) +
  geom_histogram(breaks = breaks, alpha = 0.5, position = "identity", color = "black") +
  scale_x_continuous(breaks = log10(ticks), labels = ticks) +
  scale_fill_manual(values = c("area_fm" = "blue", "new_fm" = "red"),
                    labels = c("area_fm" = "Original", "new_fm" = "New")) +
  labs(x = "Area in Fiscal Modules (in log scale)", y = "Frequency", fill = "Area") +
  theme(text = element_text(size = 11),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

dev.off()

### Table 1, with the changes in the size of the properties

before <- table(result$old_class)
after <- table(result$new_class)

mytable <- rbind(before, after, delta = after - before, diff_perc = round(after / before * 100, 2) - 100)
mytable <- mytable[, 3:1]

area_before_summary <- result |>
  sf::st_drop_geometry() |>
  dplyr::group_by(old_class) |>
  dplyr::summarize(total_area = sum(area_ha)) |>
  dplyr::mutate(total_area = total_area / 1e6)

area_after_summary <- result |>
  sf::st_drop_geometry() |>
  dplyr::group_by(new_class) |>
  dplyr::summarize(total_area = sum(area_ha)) |>
  dplyr::mutate(total_area = total_area / 1e6)

before <- t(units::drop_units(area_before_summary$total_area))
after <- t(units::drop_units(area_after_summary$total_area))

mytable2 <- round(rbind(before, after, delta = after - before, diff_perc = after / before * 100 - 100), 2)
mytable2 <- mytable2[, 3:1]
rownames(mytable2) <- rownames(mytable)

kableExtra::kable(rbind(mytable, mytable2), format = "latex")
