### Data import and tidy for MSc research  -------------------------------------------------------------------------

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

# database connection -----------------------------------------------------------------------------------------------
testdb <- file.path("D:\\Praca\\Badania\\WISL\\WISL_10lat_SGGW") # determining database filepath
con <- dbConnect(odbc::odbc(), dsn = "WISL", encoding = "Windows-1250")# connecting to db
# dbListTables(con) # listing all tables available in th database

# data loading -------------------------------------------------------------------------------------------------------
trees_05_raw <- dbReadTable(con, "DRZEWA_DO_05") # trees shorter than 0.5 m
trees_05_3_raw <- dbReadTable(con, "DRZEWA_05_DO_3") # trees teller than 0.5 m and smaller than 3 cm dbh
trees_3_7_raw <- dbReadTable(con, "DRZEWA_3_DO_7") # trees between 3 and 7 cm dbh
trees_7_raw <- dbReadTable(con, "DRZEWA_OD_7") # trees above 7 cm dbh
plot_raw <- dbReadTable(con, "POW_A_B") # sample plot data
gps_coord_raw <- dbReadTable(con, "PUNKTY_TRAKTU") # GPS coordinates
sites_raw <- dbReadTable(con, "ADRES_POW") # site description

# general data wrangling ---------------------------------------------------------------------------------------------

# I am querying for area of sample plot needed later
plot_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) -> plot # filtering out only second cycle & creating a new tibble

# I need doubles for later use of Spatial Data 
gps_coord_raw %>%
  as_tibble(.) %>%
  type_convert(.) %>%
  dplyr::select(nr_punktu = NR_PUNKTU,
         lon = DLUGOSC,
         lat = SZEROKOSC) -> gps_coord

# I am loading site description data
sites_raw %>%
  as_tibble(.) %>%
  rename_all(tolower) %>% # changing all columns to lower case
  dplyr::select(nr_punktu, nr_cyklu, nr_podpow, rok_w_cyklu, rdlp, nadl, kraina, gat_pan_pr, wiek_pan_pr, 
         b_pion_pow_pr, tsl, okr_tsl, stan_siedl) %>%
  dplyr::filter(nr_cyklu == 2, gat_pan_pr == "SO") %>%
  dplyr::select(-nr_cyklu) %>%
  type_convert(., col_types = cols_only(gat_pan_pr = col_factor(levels = NULL))) -> sites # converting column to factor

# lowest trees (h < 0.5 m) data loading and wrangling ----------------------------------------------------------------
trees_05_col <- c("gat", "pokr", "war", "uszk_rodz1", "uszk_proc1")
trees_05_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  # filter(war == 1) %>% # we want both trees and bushes therefore no filter here
  dplyr::select(-c(nr_cyklu, lp, id, uszk_rodz2, uszk_proc2)) %>%
  mutate_at(trees_05_col, funs(factor(.))) %>%
  mutate(gat = fct_collapse(gat, 
                            DB = c("DB", "DB.S", "DB.B"),
                            BRZ = c("BRZ", "BRZ.O"))) -> trees_05 # creating a new tibble
summary(trees_05)

trees_05$pokr <- factor(trees_05$pokr, ordered = TRUE, levels = c("+", "1", "5", "10", "15", "20", "25", "30", "35", "40", "45",
                                                                "50", "55", "60", "65", "70", "75", "80", "85", "90", 
                                                                "95", "100"))



# checking if there is any sample plot with dubbled species
# trees_05 %>% group_by(nr_podpow, gat) %>% summarise(n = n_distinct(gat)) %>% arrange(desc(n))

trees_05$gat %>% 
  factor() %>% 
  fct_lump(n = 10) %>%
  fct_infreq() %>% 
  fct_relevel("Other", after = Inf) %>% 
  fct_count() %>% 
  ggplot(., aes(f, n)) + 
  geom_bar(stat = "identity")

# # test for one species
# trees_05 %>%
#   filter(gat == "BRZ.O") %>%
#   left_join(., gps_coord, by = "nr_punktu") -> trees_05_gps

# data for species that are more abundant
f <- . %>% 
  group_by(gat) %>%
  filter(n() > 100) %>%
  ungroup() %>%
  left_join(., gps_coord, by = "nr_punktu")

f(trees_05)

trees_05 %>%
  group_by(gat) %>%
  filter(n() > 100) %>%
  ungroup() %>%
  left_join(., gps_coord, by = "nr_punktu") -> trees_05_gps

trees_05_gps <- making_gps(trees_05)

coordinates(trees_05_gps) <- ~ lon + lat #adding sptial relationship
proj4string(trees_05_gps) <- "+init=epsg:4326" #adding WGS84 projection


tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
  tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
  qtm(trees_05_gps, dots.alpha = 0.5) +
  # tm_compass(position = c("left", "bottom")) +
  # tm_scale_bar(position = c("left", "bottom")) + 
  tm_style_white(title = "") +
  tm_facets("gat", free.coords=TRUE, drop.units=TRUE)

# medium trees (h > 0.5 m & dbh < 3 cm) data loading and wrangling -----------------------------------------------------

trees_05_3_col <- c("gat", "war", "uszk_rodz1", "uszk_proc1")
trees_05_3_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  # filter(war == 1) %>%
  dplyr::select(-c(nr_cyklu, lp, id, uszk_rodz2, uszk_proc2)) %>%
  mutate_at(trees_05_3_col, funs(factor(.))) %>%
  mutate(gat = fct_collapse(gat, 
                            DB = c("DB", "DB.S", "DB.B"),
                            BRZ = c("BRZ", "BRZ.O"))) -> trees_05_3 # creating a new tibble
summary(trees_05_3)

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

# high trees (3 cm < dbh < 7 cm) data loading and wrangling ---------------------------------------------------------
trees_3_7_col <- c("gat", "war", "uszk_rodz1", "uszk_proc1")
trees_3_7_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  # filter(war == 1) %>%
  dplyr::select(-c(nr_cyklu, lp, id, uszk_rodz2, uszk_proc2)) %>%
  mutate_at(trees_3_7_col, funs(factor(.))) %>%
  mutate(gat = fct_collapse(gat, 
                            DB = c("DB", "DB.S", "DB.B"),
                            BRZ = c("BRZ", "BRZ.O"))) -> trees_3_7 # creating a new tibble
summary(trees_3_7)

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





# highest trees (dbh > 7 cm) data loading and wrangling -------------------------------------------------------------
trees_7_col <- c("gat", "war")
trees_7_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  filter(war == 2) %>%
  dplyr::select(nr_punktu, nr_cyklu, nr_podpow, gat, wiek, war, azymut, odl, h, d13) %>%
  mutate_at(trees_7_col, funs(factor(.))) %>%
  type_convert(col_types = cols_only(nr_punktu = "i")) %>%
  mutate(gat = fct_collapse(gat, 
                            DB = c("DB", "DB.S", "DB.B"),
                            BRZ = c("BRZ", "BRZ.O"))) -> trees_7 # creating a new tibble
summary(trees_7)

trees_7$gat %>% 
  factor() %>% 
  fct_lump(n = 10) %>%
  fct_infreq() %>% 
  fct_relevel("Other", after = Inf) %>% 
  fct_count() %>% 
  ggplot(., aes(f, n)) + 
  geom_bar(stat = "identity")

trees_7 %>%
  group_by(gat) %>%
  filter(n() > 100) %>%
  ungroup() %>%
  left_join(., gps_coord, by = "nr_punktu") -> trees_7_gps

coordinates(trees_7_gps) <- ~ lon + lat #adding sptial relationship
proj4string(trees_7_gps) <- "+init=epsg:4326" #adding WGS84 projection

tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
  tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
  qtm(trees_7_gps, dots.alpha = 0.5) +
  # tm_compass(position = c("left", "bottom")) +
  # tm_scale_bar(position = c("left", "bottom")) + 
  tm_style_white(title = "") +
  tm_facets("gat", free.coords = TRUE, drop.units = TRUE)


# ### MAPKI  -------------------------------------------------------------------------------------------------------
# 
# # devtools::install_github("nowosad/geostatbook")
# library('sp')
# library('rgdal')
# library('dplyr')
# library('proj4')
# library('raster')
# library('gstat')
# library('dismo')
# library('rgeos')
# library('ggplot2')

# ### Loading and projecting data from MS Access DB  --------------------------------------------------------------

# punkty <- readOGR(dsn="dane", layer="punkty2")
# punkty92 <- spTransform(punkty, CRS("+init=epsg:2180"))
# rdlp <- readOGR(dsn='dane', layer='Rdlp')
# # plot(rdlp)
# # plot(punkty92, add = TRUE)
# 
# punkty92$id <- 1:nrow(punkty92)
# # spplot(punkty92, 'id', colorkey=TRUE)
# 
# siatka_n <- raster(extent(gBuffer(punkty92, width = 500)))
# res(siatka_n) <- c(3000, 3000)
# siatka_n[] <- 0
# proj4string(siatka_n) <- CRS(proj4string(punkty92))
# siatka_n <- as(siatka_n, 'SpatialPolygonsDataFrame')
# siatka_n <- siatka_n[!is.na(siatka_n@data$layer), ]
# plot(siatka_n)
# # plot(punkty92)#, add=TRUE)
# 
# punkty92$liczebnosc <- rep(0, length(punkty92))
# siatka_nr <- aggregate(punkty92['liczebnosc'], by = siatka_n, FUN = length) 
# sp::spplot(siatka_nr, 'liczebnosc')
# 
# library(raster)
# r <- raster(xmn=0, ymn=0, xmx=10, ymx=10, res=1)
# xy <- spsample(as(extent(r), 'SpatialPolygons'), 100, 'random')
# 
# x <- rasterize(punkty92, siatka_n, fun='count')
# plot(x) 


# # generating raster density map ----------------------------------------------------------------------------------------------
# # test for one species
# trees_7 %>%
#   filter(gat == "JD") %>%
#   left_join(., gps_coord, by = "nr_punktu") -> trees_7_gps
# 
# coordinates(trees_7_gps) <- ~ lon + lat #adding sptial relationship
# proj4string(trees_7_gps) <- "+init=epsg:4326" #adding WGS84 projection
# 
# 
# library(MASS)
# k = kde2d(trees_7_gps$lon, trees_7_gps$lat, h = 3, n = 1000)
# r = raster(k)
# plot(r)
# 
# tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
#   qtm(r) +
#   tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
#   qtm(trees_7_gps, dots.alpha = 0.5) +
#   # tm_compass(position = c("left", "bottom")) +
#   # tm_scale_bar(position = c("left", "bottom")) + 
#   tm_style_white(title = "") 

# function that filters species and add GPS position to a sample plot
add_gps <- function(dataset, col_name = "gat") {
  dataset %>% 
    group_by_(col_name) %>% 
    filter(n() > 100) %>%
    ungroup() %>%
    left_join(., gps_coord, by = "nr_punktu") -> y
  coordinates(y) <- ~ lon + lat #adding sptial relationship
  proj4string(y) <- "+init=epsg:4326" #adding WGS84 projection
  return(y)
}

add_gps(trees_05)

loop <- list(trees_05, trees_05_3)

testing <- lapply(loop, add_gps)

qtm(testing[[1]])
 
draw_map <- function(facet = FALSE) {
  map <- tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
    tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
    qtm(testing[[2]], dots.alpha = 0.5) +
    # tm_compass(position = c("left", "bottom")) +
    # tm_scale_bar(position = c("left", "bottom")) + 
    tm_style_white(title = "")
  if (facet == TRUE) 
    map + tm_facets("gat", free.coords=TRUE, drop.units=TRUE) 
  else 
    map
}



mapka <- 
  tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
  tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4)
  # tm_shape(subset(trees_05_gps, gat == "JD")) + tm_dots(alpha = 0.5)
  # tm_layout(panel.show = TRUE, panel.labels = "aaa")
  # tm_compass(position = c("left", "bottom")) 
  # tm_scale_bar(position = c("left", "bottom")) + 

  
draw_maps2 <- function(x){  
map1 <- mapka + qtm(subset(trees_05_gps, gat == x), dots.alpha = 0.5) + tm_layout(panel.show = TRUE, panel.labels = "<0.5m")
map2 <- mapka + qtm(subset(trees_05_3_gps, gat == x), dots.alpha = 0.5) + tm_layout(panel.show = TRUE, panel.labels = ">0.5m&<3cm")
map3 <- mapka + qtm(subset(trees_3_7_gps, gat == x), dots.alpha = 0.5) + tm_layout(panel.show = TRUE, panel.labels = "3cm<dbh<7cm")
map4 <- mapka + qtm(subset(trees_7_gps, g == x), dots.alpha = 0.5) + tm_layout(panel.show = TRUE, panel.labels = ">7cm")  
tmap_arrange(map1, map2, map3, map4, asp = NA) 
}

draw_maps2("JD")



