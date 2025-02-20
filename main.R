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

sf::read_sf("amz_fc/shape/amz_land_tenure.shp") |>
  dplyr::inner_join(data, by = "id") |>
  sf::write_sf("result/car-legal-reserve.shp")

###########################################################################
# Plotting
###########################################################################

# Figure XX of the paper, histogram of legal reserve percentage
hist(data$lr_porc)

dim(data) # 470.645 rural properties

antes = table(data$tamanho)
depois = table(data$n_tamanho)

ticks <- c(0.1, 1, 10, 100)

log_valores1 = log10(data$area_fm)
log_valores2 = log10(result$new_fm)

breaks <- seq(floor(min(c(log_valores1, log_valores2), na.rm=T)), 
              ceiling(max(c(log_valores1, log_valores2), na.rm=T)), by = 0.25)

# Figure XX of the paper, histogram of area of rural properties in fiscal modules
hist1 <- hist(log_valores1, breaks = breaks, col="blue", xaxt = "n")
axis(1, at = log10(ticks), labels = round(ticks, 1))

# Figure XX plotting two histograms of areas together
hist1 <- hist(log_valores1, breaks = breaks, col="blue", xaxt = "n")
hist2 <- hist(log_valores2, breaks = breaks, col=rgb(1, 0, 0, 0.5), add=T, xaxt = "n" )
axis(1, at = log10(ticks), labels = round(ticks, 1))

# Combine frequencies into a matrix
counts <- rbind(hist1$counts, hist2$counts)

# Create side-by-side barplot
barplot(counts, beside = TRUE, col = c("red", "blue"),
        legend = c("Dataset 1", "Dataset 2"),
        main = "Side-by-Side Histogram", xlab = "Value", ylab = "Frequency")
ticks <- c(0.1, 1, 10, 100, 1000)
axis(1, at = log10(ticks), labels = round(ticks, 1))



# tabela com as mudancas dos tamanhos das propriedades
mytable = rbind(antes, depois, delta = depois - antes, diff_perc = round(depois / antes * 100) - 100)

kableExtra::kable(mytable, format = "latex")

par(mfcol = c(1, 2))
hist(log(data$area_fm), ylim = c(0, 140000), main = "Antes")
hist(log(result$new_fm), ylim = c(0, 140000), main = "Depois")

data

r2 <- result |>
  dplyr::filter(area_fm < 75)
hist(r2$area_fm)

r2 <- result |>
  dplyr::filter(area_fm < 10)
hist(r2$area_fm)


change_to_less_than_4 <- result |> 
  dplyr::filter(new_fm < 4) |>
  dplyr::filter(area_fm > 4) 

# 26.179 propriedades tinham mais de 4MF e passaram a ter menos
# ou 5.57% de todas as propriedades
dim(change_to_less_than_4)
nrow(change_to_less_than_4) / nrow(result) * 100

change_to_less_than_4

names(change_to_less_than_4)

change_to_less_than_4$inside = 1

sum(change_to_less_than_4$lr_balance)


amz <- sf::read_sf("amz_land_tenure.shp")
amz$id = as.integer(amz$id)

uf_map <- tibble(
  uf_code = paste0(c(12, 13, 16, 15, 21, 51, 11, 14, 17)),
  sigla_uf = c("AC", "AM", "AP", "PA", "MA", "MT", "RO", "RR", "TO")
)


fresult <- amz %>%
  inner_join(change_to_less_than_4, by = "id") |>
  left_join(uf_map, by = "uf_code")

table(fresult$sigla_uf)

