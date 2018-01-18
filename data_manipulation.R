### Data import and tidy for MSc research  -------------------------------------------------------------------------

library(MASS)
library(raster)
library(forcats)
library(stringr)
library(rgdal)
library(ggtern)
library(tmap)
library(tmaptools)
library(sf)
library(ks)
library(feather)
library(tidyverse) # should be loaded as last to make sure that its function work without any additional commands
# vignette("tmap-nutshell")

data(Europe, rivers) # loading data of Europe to plot it later as a background
vistula <- subset(rivers, name == "Vistula") # Vistula river to plot is as a reference
rm(rivers)
# Polska <- read_shape("dane/Polska.shp", as.sf = TRUE)
# Poland administrative boundary
poland <- read_shape("POL_adm_shp/POL_adm0.shp", as.sf = TRUE)

# function that filters species and add GPS position to a sample plot
add_gps <- function(dataset, col_name = "gat") {
  dataset %>% 
    # group_by_(col_name) %>% 
    # filter(n() > 10) %>%
    # ungroup() %>%
    left_join(., gps_coord, by = "nr_punktu") -> y
  y <- st_as_sf(y, coords = c("lon", "lat"), crs = 4326)
  # coordinates(y) <- ~ lon + lat #adding sptial relationship
  # proj4string(y) <- "+init=epsg:4326" #adding WGS84 projection
  return(y)
}

# function for fast map drawing
draw_map <- function(dataframe, facet = FALSE) {
  map <- tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() + # Polska or Europe for performance
    tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
    qtm(dataframe, dots.alpha = 0.5) 
    # tm_compass(position = c("left", "bottom")) +
    # tm_scale_bar(position = c("left", "bottom")) + 
    # tm_style_white(title = "")
  if (facet == TRUE) 
    map + tm_facets("gat", free.coords = TRUE, drop.units = TRUE) 
  else 
    map
}

draw_maps_4 <- function(dataframe, x, facet = FALSE){
  dataframe %>% filter(gat == x) -> subset
  map <- tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
    tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 3) +
    qtm(subset, dots.alpha = 0.5) + 
    tm_layout(asp = 0, outer.margins=0)
  if (facet == TRUE) 
    map + tm_facets(by = "group", free.coords = TRUE, drop.units = TRUE, drop.empty.facets = FALSE)
  else 
    map
}

# draw_maps_4(sites_so_gps, "DB", facet = TRUE)

density <- function(x) {
  min_x <- c(13.8)
  max_x <- c(24.5)
  min_y <- c(49.0)
  max_y <- c(55.0)
  coords <- as.data.frame(st_coordinates(x$geometry))
  # st_geometry(x) <- NULL
  # wwaga <- as.integer(x[[waga]])
  density_x <- kde2d(coords$X, coords$Y, n = 500, h = 1, lims = c(min_x, max_x, min_y, max_y)) # n - number of gridcells for fast calclations
  # density_x <- kde(cbind(coords$X, coords$Y), gridsize = c(500, 500), h = 10, xmin = c(min_x, min_y), xmax = c(max_x, max_y))
  r = raster(density_x)
  return(r)
}

# density(trees_05_gps)

# test_function <- function(x, waga) {
#   st_geometry(x) <- NULL
#   wwaga <- as.integer(x[[waga]])
#   return(wwaga)
# }
# 
# test_function(trees_05_gps, w = "pokr")


draw_density_plot <- function(dataframe, x) {
  dataframe %>% filter(gat == x) -> subset
r <- density(subset)
rr <- mask(r, as(poland, "Spatial"))
map <- tm_shape(rr) + tm_raster("layer", breaks = seq(0, 0.1, by=0.02),  legend.show = TRUE) +
  tm_shape(poland) + tm_borders() +
  tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 3) +
  tm_shape(subset) + tm_dots(alpha = 0.1) +
  tm_layout(asp = 0, outer.margins=0, legend.position = c("LEFT", "BOTTOM"))
map
}

draw_density_plot(sites_so_gps, "BK")

# draw_density_plot_4 <- function(x) {
# map1 <- draw_density_plot(trees_05_gps, x)
# map2 <- draw_density_plot(trees_05_3_gps, x)
# map3 <- draw_density_plot(trees_3_7_gps, x)
# map4 <- draw_density_plot(trees_7_gps, x)
# tmap_arrange(map1, map2, map3, map4, ncol = 2, nrow = 2)
# }

# draw_density_plot_4("DB")

wykres <- . %>% 
  factor() %>% 
  fct_lump(n = 10) %>%
  fct_infreq() %>%
  fct_relevel("Other", after = Inf) %>%
  fct_count() 
  # ggplot(., aes(f, n)) +
  # geom_bar(stat = "identity")

###
### Czego potrzebuję?
# 1. Wybieram Wszystkie punkty z drugiego cyklu
# 2. Łączę je z danymi o poszczególnych (3) warstwach i danych GPS 
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
# kriging/IDW zamiast kde?

# data loading -------------------------------------------------------------------------------------------------------
trees_05 <- read_feather("trees_05.feather") # trees shorter than 0.5 m
trees_05_3 <- read_feather("trees_05_3.feather") # trees teller than 0.5 m and smaller than 3 cm dbh
trees_3_7 <- read_feather("trees_3_7.feather") # trees between 3 and 7 cm dbh
trees_7 <- read_feather("trees_7.feather") # trees above 7 cm dbh
plot <- read_feather("plot.feather") # sample plot data
gps_coord <- read_feather("gps_coord.feather") # GPS coordinates
sites <- read_feather("sites.feather") # site description

# lowest trees (h < 0.5 m) data loading and wrangling ----------------------------------------------------------------
# summary(trees_05)

# checking if there is any sample plot with dubled species
# trees_05 %>% group_by(nr_podpow, gat) %>% summarise(n = n_distinct(gat)) %>% arrange(desc(n))

# wykres(trees_05$gat) %>%
#   ggplot(., aes(f, n)) + 
#   geom_bar(stat = "identity")

# # test for one species
# trees_05 %>%
#   filter(gat == "BRZ.O") %>%
#   left_join(., gps_coord, by = "nr_punktu") -> trees_05_gps

trees_05_gps <- add_gps(trees_05)

# draw_map(trees_05_gps)
# draw_map(trees_05_gps, facet = TRUE)

# medium trees (h > 0.5 m & dbh < 3 cm) data loading and wrangling -----------------------------------------------------
# summary(trees_05_3)
# 
# wykres(trees_05_3$gat) %>%
#   ggplot(., aes(f, n)) + 
#   geom_bar(stat = "identity")

trees_05_3_gps <- add_gps(trees_05_3)

# draw_map(trees_05_3_gps)
# draw_map(trees_05_3_gps, facet = TRUE)

# high trees (3 cm < dbh < 7 cm) data loading and wrangling ---------------------------------------------------------
# summary(trees_3_7)

# wykres(trees_3_7$gat) %>%
#   ggplot(., aes(f, n)) + 
#   geom_bar(stat = "identity")

trees_3_7_gps <- add_gps(trees_3_7)

# draw_map(trees_3_7_gps)
# draw_map(trees_3_7_gps, facet = TRUE)

# highest trees (dbh > 7 cm) data loading and wrangling -------------------------------------------------------------
# summary(trees_7)
# 
# wykres(trees_7$gat) %>%
#   ggplot(., aes(f, n)) + 
#   geom_bar(stat = "identity")

trees_7_gps <- add_gps(trees_7)

# draw_map(trees_7_gps)
# draw_map(trees_7_gps, facet = TRUE)


# ### MAPKI  -------------------------------------------------------------------------------------------------------
# # generating raster density map ----------------------------------------------------------------------------------------------
# 
# test <- function(x) {draw_density_plot(sites_so_gps, x) + tm_layout(title = x)}
# test("BK")

# grouping altogether all sample plot layers ------------------------------------------------------------------------
bind_rows("< 0.5m" = trees_05, 
          "> 0.5m & < 3cm" = trees_05_3, 
          "3cm < dbh < 7cm" =  trees_3_7, 
          "dbh > 7cm" = trees_7, 
          .id = "group") %>% 
  group_by(gat) %>% 
  filter(n() > 99) %>% 
  ungroup() -> trees_all

# I need to add age restriction to avoid young pine monocultures
sites %>% select(nr_punktu, nr_podpow, gat_pan_pr, wiek_pan_pr) %>% left_join(trees_all %>% select(-nr_punktu), by = "nr_podpow") -> sites_so

sites_so_gps <- add_gps(sites_so)
sites_so_gps$gat <- factor(sites_so_gps$gat)
sites_so_gps$group <- factor(sites_so_gps$group)

table(cut(sites_so$wiek_pan_pr, breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240)), sites_so$group)

draw_maps_4(sites_so_gps, "BK", facet = TRUE)

# 
# gatunki <- as.list(sort(as.character(unique(sites_so_gps$gat))))
# gatunki_sample <- (gatunki[1:3])
gatunki_sample <- c("DB", "BK", "JD", "ŚW", "GB")
names(gatunki) <- gatunki

lapply(gatunki_sample, draw_maps_4, dataframe = sites_so_gps, facet = TRUE)

# save_tmap(draw_maps_4("BK"), "World_map.png", width=1280, height=1024)

# GRID drawing ------------------------------------------------------------------------------------------------------------
# Poland as 10km squares
# poland <- read_shape("POL_adm_shp/POL_adm0.shp", as.sf = TRUE)
# poland_grid <- read_shape("Poland_shapefile/pl_10km.shp", as.sf = TRUE)
# 
# poland <- set_projection(poland, 4326)
# poland_grid <- set_projection(poland_grid, 4326)
# 
# poland_grid %>% st_join(poland) %>% 
#   dplyr::filter(!is.na(ID_0)) %>% dplyr::select(CELLCODE) ->
#   poland_grid
# 
# poland_grid %>% st_join(st_as_sf(set_projection(test, 4326))) %>% 
#   dplyr::group_by(CELLCODE) %>% dplyr::summarise(N = n(), mSI = mean(as.integer(pokr))) -> 
#   poland_agg
# 
# # tmap_mode('view')
# tm_shape(poland_agg) + tm_fill(col = "N", alpha = 0.5)

###
# - po pierwsze, pokazać dane na siatce 10x10km
# - do drugie, ilość alpha wielkość udziału kolor
# - po trzecie, ogarnąć zasiegi gatunków
# - po czwarte, 