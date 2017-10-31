#############################################
### Data import and tidy for MSc research ###
#############################################

library(odbc)
library(DBI)
library(tidyverse)
library(forcats)
library(stringr)
library(sp)
# library(ggmap)
library(tmap)
# vignette("tmap-nutshell")
data(Europe, rivers) # loading data of Europe to plot it later as a background
vistula <- subset(rivers, name == "Vistula") # Vistula river to plot is as a reference

###
### Czego potrzebuję?
# 1. Wybieram Wszystkie punkty z drugiego cyklu w których sosna dominuje
# 2. Łączę je z danymi o poszczególnych (3) warstwach i danych GPS - pomyśleć o sensownym nazewnictwie
# 3. Mapka wszystkich powierzchni próbnych zajętych w danej warstwie - heatmap?
# 4. Wykres pokazujący udział gatunków, stopień pokrycia i rodzaje uszkodzeń.
# 5. Do wykresów mapki dla kraju.

# 1. Pokazania na mapie wszystkich powierzchni próbnych oraz tych z dominującą sosną (wybranych).
# 1a. Udział badanych do niebadanych.
# 2. Podziału na 3 kategorie i mapek panującego gatunku odnowienia na każdej.
# 3. Pokazanie udziału 5 lub 10 najważniejszych gatunków w grupie (może procenty?).
# 4. Analiza w zależności od krainy/mezoregionu oraz siedliska.
# 5. Pokazanie składu gatunkowego pierwszego piętra.
# 6. Sprawdzić udziały dla poszczególnych powierzchni próbnych.

###
### Do artykułu:
# porównanie dwóch cykli

# database connection -----
testdb <- file.path("D:\\Praca\\Badania\\WISL\\WISL_10lat_SGGW") #determining database filepath
con <- dbConnect(odbc::odbc(), dsn = "WISL", encoding = "Windows-1250")#connecting to db
# dbListTables(con) #listing all tables available in th database

# data loading -----
trees_05_raw <- dbReadTable(con, "DRZEWA_DO_05") #trees below 0.5 m tall
trees_05_3_raw <- dbReadTable(con, "DRZEWA_05_DO_3") #trees between 0.5 m tall and 3 cm dbh
trees_3_7_raw <- dbReadTable(con, "DRZEWA_3_DO_7") #trees between 3 and 7 cm dbh
trees_7_raw <- dbReadTable(con, "DRZEWA_OD_7") #trees between 3 and 7 cm dbh
plot_raw <- dbReadTable(con, "POW_A_B") #plot data
gps_coord_raw <- dbReadTable(con, "PUNKTY_TRAKTU") # GPS coordinates
sites_raw <- dbReadTable(con, "ADRES_POW") #site description

# data wrangling -----
# I am querying for area of sample plot needed later
plot_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) -> plot # filtering out only second cycle & creating a new tibble

# I need doubles for later use of Spatial Data 
gps_coord_raw %>%
  as_tibble(.) %>%
  type_convert(.) %>%
  select(nr_punktu = NR_PUNKTU,
         lat = SZEROKOSC,
         lon = DLUGOSC) -> gps_coord

# I am loading site description data
sites_raw %>%
  as_tibble(.) %>%
  rename_all(tolower) %>% # changing all columns to lower case
  select(nr_punktu, nr_cyklu, nr_podpow, rok_w_cyklu, rdlp, nadl, kraina, gat_pan_pr, wiek_pan_pr, 
         b_pion_pow_pr, tsl, okr_tsl, stan_siedl) %>%
  dplyr::filter(nr_cyklu == 2, gat_pan_pr == "SO") %>%
  select(-nr_cyklu) %>%
  type_convert(., col_types = cols_only(gat_pan_pr = col_factor(levels = NULL))) -> sites # converting column to factor

# lowest trees data loading and wrangling ------
trees_05_col <- c("gat", "pokr", "war", "uszk_rodz1", "uszk_proc1")
trees_05_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  # filter(war == 1) %>% # we want both trees and bushes therefore no filter here
  dplyr::select(-c(nr_cyklu, lp, id, uszk_rodz2, uszk_proc2)) %>%
  mutate_at(trees_low_col, funs(factor(.))) -> trees_05 # creating a new tibble
summary(trees_05)

# str_c(levels(trees_05$gat), collapse = "', '")
# levels(trees_05$gat) <- c('AK', 'BK', 'BRZ', 'BRZ', 'BST', 'CZR', 'CZR', 'DB', 'DB', 'DB.C', 'DB', 'DG', 'GB', 'GR', 'IWA', 
                     # 'JB', 'JD', 'JKL', 'JS', 'JW', 'KL', 'KL', 'KL.T', 'KSZ', 'LP', 'MD', 'OL', 'OL', 'OS', 'SO', 
                     # 'SO.B', 'SO.C', 'SO.K', 'SO.S', 'SO.WE', 'ŚL', 'ŚL.A', 'ŚL.L', 'ŚW', 'TP', 'TP.C', 'WB', 'WB.NO', 
                     # 'WIŚ', 'WZ', 'WZ.P')

trees_05$pokr <- factor(trees_05$pokr, ordered = TRUE, levels = c("+", "1", "5", "10", "15", "20", "25", "30", "35", "40", "45",
                                                                "50", "55", "60", "65", "70", "75", "80", "85", "90", 
                                                                "95", "100"))



# checking if there is any sample plot with dubbled species
trees_05 %>% group_by(nr_podpow, gat) %>% summarise(n = n_distinct(gat)) %>% arrange(desc(n))

trees_05$gat %>% 
  factor() %>% 
  fct_lump(n = 10) %>%
  fct_infreq() %>% 
  fct_relevel("Other", after = Inf) %>% 
  fct_count() %>% 
  ggplot(., aes(f, n)) + 
  geom_bar(stat = "identity")

# test for one species
trees_05 %>%
  filter(gat == "CZM.P") %>%
  left_join(., gps_coord, by = "nr_punktu") -> trees_05_gps

# data for species that are more abundant
trees_05 %>%
  group_by(gat) %>%
  filter(n() > 100) %>%
  ungroup() %>%
  left_join(., gps_coord, by = "nr_punktu") -> trees_05_gps

coordinates(trees_05_gps) <- ~ lon + lat #adding sptial relationship
proj4string(trees_05_gps) <- "+init=epsg:4326" #adding WGS84 projection


tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
  tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
  qtm(trees_05_gps, dots.alpha = 0.5) +
  # tm_compass(position = c("left", "bottom")) +
  # tm_scale_bar(position = c("left", "bottom")) + 
  tm_style_white(title = "") +
  tm_facets("gat", free.coords=TRUE, drop.units=TRUE)

draw_map <- function(facet = FALSE) {
  map <- tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
    tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
    qtm(trees_05_gps, dots.alpha = 0.5) +
    # tm_compass(position = c("left", "bottom")) +
    # tm_scale_bar(position = c("left", "bottom")) + 
    tm_style_white(title = "")
    if (facet == TRUE) 
      map + tm_facets("gat", free.coords=TRUE, drop.units=TRUE) 
  else 
    map
}

# medium trees data loading and wrangling -----
trees_05_3_col <- c("gat", "war", "uszk_rodz1", "uszk_proc1")
trees_05_3_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  # filter(war == 1) %>%
  select(-c(nr_cyklu, lp, id, uszk_rodz2, uszk_proc2)) %>%
  mutate_at(trees_medium_col, funs(factor(.))) -> trees_05_3 # creating a new tibble
summary(trees_05_3)

# str_c(levels(trees_05_3$gat), collapse = "', '")
# levels(trees_05_3$gat) <- c('AK', 'BK', 'BRZ', 'BRZ', 'BST', 'CIS', 'CZR', 'CZR', 'DB', 'DB', 'DB.C', 'DB', 'DG', 
#                               'GB', 'GR', 'IWA', 'JB', 'JD', 'JKL', 'JRZ.B', 'JS', 'JW', 'KL', 'KL', 'KL.T', 'KSZ', 
#                               'LP', 'MD', 'OL', 'OL', 'ORZ.C', 'ORZ.W', 'OS', 'SAL.FR.BU', 'SO', 'SO.B', 'SO.C', 
#                               'SO.K', 'SO.S', 'SO.W', 'SO.WE', 'ŚL', 'ŚL.A', 'ŚL.L', 'ŚW', 'TP', 'WB', 'WB.K', 'WB.NO', 
#                               'WIŚ', 'WZ', 'WZ', 'ŻWC.J')


trees_05_3 %>% group_by(nr_podpow, gat) %>% summarise(n = n_distinct(gat)) %>% arrange(desc(n))

trees_05_3$gat %>% 
  factor() %>% 
  fct_lump(n = 10) %>%
  fct_infreq() %>% 
  fct_relevel("Other", after = Inf) %>% 
  fct_count() %>% 
  ggplot(., aes(f, n)) + 
  geom_bar(stat = "identity")

trees_05_3 %>%
  group_by(gat) %>%
  filter(n() > 100) %>%
  ungroup() %>%
  left_join(., gps_coord, by = "nr_punktu") -> trees_05_3_gps

coordinates(trees_05_3_gps) <- ~ lon + lat #adding sptial relationship
proj4string(trees_05_3_gps) <- "+init=epsg:4326" #adding WGS84 projection

tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
  tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
  qtm(trees_05_3_gps, dots.alpha = 0.5) +
  # tm_compass(position = c("left", "bottom")) +
  # tm_scale_bar(position = c("left", "bottom")) + 
  tm_style_white(title = "") +
  tm_facets("gat", free.coords=TRUE, drop.units=TRUE)

# high trees data loading and wrangling -----
trees_3_7_col <- c("gat", "war", "uszk_rodz1", "uszk_proc1")
trees_3_7_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  # filter(war == 1) %>%
  select(-c(nr_cyklu, lp, id, uszk_rodz2, uszk_proc2)) %>%
  mutate_at(trees_high_col, funs(factor(.))) -> trees_3_7 # creating a new tibble
summary(trees_3_7)

# str_c(levels(trees_3_7$gat), collapse = "', '")
# levels(trees_3_7$gat) <- c('AK', 'BK', 'BRZ', 'BRZ', 'BST', 'CIS', 'CZR', 'CZR', 'DB', 'DB', 'DB.BI', 'DB.C', 
#                             'DB', 'DG', 'GB', 'GR', 'IWA', 'JB', 'JD', 'JKL', 'JS', 'JW', 'KL', 'KL', 'KSZ', 'LP', 
#                             'MD', 'OL', 'OL', 'OS', 'SO', 'SO.B', 'SO.C', 'SO.K', 'SO.L', 'SO.S', 'SO.WE', 'ŚL', 
#                             'ŚL.L', 'ŚW', 'TP', 'WB', 'WB.K', 'WB.NO', 'WZ', 'WZ')


trees_3_7 %>% group_by(nr_podpow, gat) %>% summarise(n = n_distinct(gat)) %>% arrange(desc(n))

trees_3_7$gat %>% 
  factor() %>% 
  fct_lump(n = 10) %>%
  fct_infreq() %>% 
  fct_relevel("Other", after = Inf) %>% 
  fct_count() %>% 
  ggplot(., aes(f, n)) + 
  geom_bar(stat = "identity")

trees_3_7 %>%
  group_by(gat) %>%
  filter(n() > 100) %>%
  ungroup() %>%
  left_join(., gps_coord, by = "nr_punktu") -> trees_3_7_gps

coordinates(trees_3_7_gps) <- ~ lon + lat #adding sptial relationship
proj4string(trees_3_7_gps) <- "+init=epsg:4326" #adding WGS84 projection

tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
  tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
  qtm(trees_3_7_gps, dots.alpha = 0.5) +
  # tm_compass(position = c("left", "bottom")) +
  # tm_scale_bar(position = c("left", "bottom")) + 
  tm_style_white(title = "") +
  tm_facets("gat", free.coords=TRUE, drop.units=TRUE)




# generating raster density map -----
# library(MASS)
# k = kde2d(trees_05_gps$lon, trees_05_gps$lat, h = 0.4, n = 100)
# 
# library(raster)
# r = raster(k)
# plot(r)














##########
### Grupowanie krainami
##########

dane2 %>%
  group_by(KRAINA, GAT_DO_05) %>%
  summarise(n = n_distinct(NR_PODPOW)) %>%
  filter(GAT_DO_05 != "") %>%
  rename(gat = GAT_DO_05, kraina = KRAINA) %>%
  arrange(kraina, desc(n)) %>%
  filter(row_number() %in% c(1, 2, 3)) -> kr_gat_do_05

ggplot(data = kr_gat_do_05, aes(x = gat, y = n)) + geom_bar(stat="identity", position = "dodge") + 
  facet_grid(kraina ~ .) + 
  theme_minimal()

#############
### MAPKI ### 
#############

# devtools::install_github("nowosad/geostatbook")
library('sp')
library('rgdal')
library('dplyr')
library('proj4')
library('raster')
library('gstat')
library('dismo')
library('rgeos')
library('ggplot2')
############
### Loading and projecting data from MS Access DB
############
punkty <- readOGR(dsn="dane", layer="punkty2")
punkty92 <- spTransform(punkty, CRS("+init=epsg:2180"))
rdlp <- readOGR(dsn='dane', layer='Rdlp')
# plot(rdlp)
# plot(punkty92, add = TRUE)

punkty92$id <- 1:nrow(punkty92)
# spplot(punkty92, 'id', colorkey=TRUE)

siatka_n <- raster(extent(gBuffer(punkty92, width = 500)))
res(siatka_n) <- c(16000, 16000)
siatka_n[] <- 0
proj4string(siatka_n) <- CRS(proj4string(punkty92))
siatka_n <- as(siatka_n, 'SpatialPolygonsDataFrame')
siatka_n <- siatka_n[!is.na(siatka_n@data$layer), ]
plot(siatka_n)
# plot(punkty92, add=TRUE)

punkty92$liczebnosc <- rep(0, length(punkty92))
siatka_nr <- aggregate(punkty92['liczebnosc'], by = siatka_n, FUN = length) 
sp::spplot(siatka_nr, 'liczebnosc')


