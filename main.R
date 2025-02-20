# code for the article "Possible Effects of the Judicial Redefinition of Rural 
# Property Size: A Case Study in the Legal Amazon" submitted to GeoInfo 2025

require(dplyr)

data <- terra::readRDS("C:/Users/pedro/Downloads/amz_fc/tab/prodes/tab4id.rds")
px_ha <- 34.4523421771659 * 34.4524159329805 * 1e-04

data <- data |>
  dplyr::filter(year == 2021) |>
#  dplyr::mutate(lr_porc = (lr_area * !!px_ha) / area_ha) 
  dplyr::mutate(lr_porc = (lr_area / lr_forest))


zeros = data |>
  dplyr::filter(area_ha == 0) 

zeros # 4 objetos que possuem area zero e serao removidos

#data2 <- data |>
#  dplyr::filter(lr_porc > 0.8)

# Figure XX of the paper, histogram of legal reserve percentage
hist(data$lr_porc)


# depois retirar estas linhas
data <- data |>
  dplyr::filter(area_ha > 0) |>
  mutate(lr_porc = ifelse(lr_porc > 0.8, 0.8, lr_porc))

dim(data) # sobraram 470.645 propriedades


#data <- data |>
#  dplyr::mutate(lr_porc = dplyr::case_when(
#    lr_porc < 0.5 ~ 0,
#    lr_porc >= 0.5 & lr_porc < 0.8 ~ 0.5,
#    lr_porc >= 0.8 ~ 0.8
#  ))

data <- data |>
  dplyr::mutate(tamanho = dplyr::case_when(
    area_fm <= 4 ~ "pequena",
    area_fm > 4 & area_fm <= 15 ~ "media",
    area_fm > 15 ~ "grande"
  ))

antes = table(data$tamanho)

result <- data |>
  dplyr::mutate(new_fm = area_fm * (1 - lr_porc)) |>
  dplyr::mutate(n_tamanho = dplyr::case_when(
    new_fm <= 4 ~ "pequena",
    new_fm > 4 & new_fm <= 15 ~ "media",
    new_fm > 15 ~ "grande"
  ))

depois = table(result$tamanho)

result
max(result$id)
min(result$id)
amz <- sf::read_sf("amz_land_tenure.shp")

amz
max(amz$id)
min(amz$id)

shp <- dplyr::inner_join(amz, result, by = "id")

sf::write_sf(shp, "car-legal-reserve.shp")

names(shp)
unique(shp$tamanho)

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

