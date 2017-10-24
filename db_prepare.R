#############################################
### Data import and tidy for MSc research ###
#############################################

library(odbc)
library(DBI)
library(tidyverse)
library(forcats)
library(sp)
library(ggmap)
library(tmap)
vignette("tmap-nutshell")

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

# database connection -----
testdb <- file.path("D:\\Praca\\Badania\\WISL\\WISL_10lat_SGGW") #determining database filepath
con <- dbConnect(odbc::odbc(), dsn = "WISL", encoding = "Windows-1250")#connecting to db
# dbListTables(con) #listing all tables available in th database

# data loading -----
trees_low_raw <- dbReadTable(con, "DRZEWA_DO_05") #trees below 0.5 m tall
trees_medium_raw <- dbReadTable(con, "DRZEWA_05_DO_3") #trees between 0.5 m tall and 3 cm dbh
trees_high_raw <- dbReadTable(con, "DRZEWA_3_DO_7") #trees between 3 and 7 cm dbh
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

# lowest trees data loading and wrangling
trees_low_col <- c("gat", "pokr", "uszk_rodz1", "uszk_proc1")
trees_low_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  filter(war == 1) %>%
  select(-c(nr_cyklu, lp, id, uszk_rodz2, uszk_proc2, war)) %>%
  mutate_at(trees_low_col, funs(factor(.))) -> trees_low # creating a new tibble
summary(trees_low)

# str_c(levels(DRZEWA_DO_05_filter_gps$GAT), collapse = "', '")
levels(trees_low$gat) <- c('AK', 'BK', 'BRZ', 'BRZ', 'BST', 'CZR', 'CZR', 'DB', 'DB', 'DB.C', 'DB', 'DG', 'GB', 'GR', 'IWA', 
                     'JB', 'JD', 'JKL', 'JS', 'JW', 'KL', 'KL', 'KL.T', 'KSZ', 'LP', 'MD', 'OL', 'OL', 'OS', 'SO', 
                     'SO.B', 'SO.C', 'SO.K', 'SO.S', 'SO.WE', 'ŚL', 'ŚL.A', 'ŚL.L', 'ŚW', 'TP', 'TP.C', 'WB', 'WB.NO', 
                     'WIŚ', 'WZ', 'WZ.P')

trees_low$pokr <- factor(trees_low$pokr, ordered = TRUE, levels = c("+", "1", "5", "10", "15", "20", "25", "30", "35", "40", "45",
                                                                "50", "55", "60", "65", "70", "75", "80", "85", "90", 
                                                                "95", "100"))



# checking if there is any sample plot with dubbled species
trees_low %>% group_by(nr_podpow, gat) %>% summarise(n = n_distinct(gat)) %>% arrange(desc(n))

trees_low$gat %>% 
  factor() %>% 
  fct_lump(n = 10) %>% 
  fct_infreq() %>% 
  fct_relevel("Other", after = Inf) %>% 
  fct_count() %>% 
  ggplot(., aes(f, n)) + 
  geom_bar(stat = "identity")

trees_low %>%
  # filter(gat == "ŚW") %>%
  left_join(., gps_coord, by = "nr_punktu") -> trees_low_gps

trees_low %>%
  group_by(gat) %>%
  filter(n() > 100) %>%
  ungroup() %>%
  left_join(., gps_coord, by = "nr_punktu") -> trees_low_gps

coordinates(trees_low_gps) <- ~ lon + lat #adding sptial relationship
proj4string(trees_low_gps) <- "+init=epsg:4326" #adding WGS84 projection

data(Europe, rivers)
vistula <- subset(rivers, name == "Vistula")
tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
  tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
  qtm(trees_low_gps, dots.alpha = 0.5) +
  # tm_compass(position = c("left", "bottom")) +
  # tm_scale_bar(position = c("left", "bottom")) + 
  tm_style_white(title = "") +
  tm_facets("gat", free.coords=TRUE, drop.units=TRUE)

draw_map <- function(facet = FALSE) {
  map <- tm_shape(Europe, bbox = "Poland", projection="longlat", is.master = TRUE) + tm_borders() +
    tm_shape(vistula) + tm_lines(col = "steelblue", lwd = 4) +
    qtm(trees_low_gps, dots.alpha = 0.5) +
    # tm_compass(position = c("left", "bottom")) +
    # tm_scale_bar(position = c("left", "bottom")) + 
    tm_style_white(title = "")
    if (facet == TRUE) 
      map + tm_facets("gat", free.coords=TRUE, drop.units=TRUE) 
  else 
    map
}




# # joining files -----
# sites_area <- dplyr::left_join(sites, plot_a, by = "nr_podpow") # joining sample plot area to site description
# sites_area_gps <- dplyr::left_join(sites_area, gps_coord, by = "nr_punktu") # adding GPS position data
# 
# # exporting data ----- 
# write_tsv(sites_area_gps, "sites_area_gps.txt") # saving tabular format for later analysis
# write_tsv(trees, "trees.txt")

trees_low %>% 
  count(gat, pokr) %>%  
  ggplot(mapping = aes(x = gat, y = pokr)) +
  geom_tile(mapping = aes(fill = n))

ggplot(data = trees_low) +
  geom_count(mapping = aes(x = gat, y = pokr))
 

##########
### Ujednolicenie nazw gatunków dominujących
##########

levels(dane$GAT_PAN_PR) <- c("", "AK", "BEZ.C", "BK", "BRZ", "BRZ.O", "BST", "CYP.L", "CZM", "CZM.P", "CZR", 
                        "DB", "DB", "DB.C", "DB", "DER", "DER", "DER", "DG", "GB", "GŁG", "GR",
                        "IWA", "JB", "JD", "JKL", "JRZ", "JS", "JW", "KL", "LP", "LSZ", "MD", "OL", "OL", 
                        "OS", "SO", "SO", "SO.B", "SO.C", "SO.K", "SO.WE", "ŚL", "ŚW", "TP", "TP", "WB", 
                        "WB.NO", "WZ")

##########
### Ujednolicenie nazw gatunków w dolnych warstwach
##########

levels(dane$GAT_DO_05) <- c("", "AK", "BK", "BRZ", "BRZ", "BST", "CIS", "CZR", "CZR", "DB", "DB", "DB.C", 
                             "DB", "DB", "GB", "GR", "IWA", "JB", "JD", "JKL", "JS", "JW", "KL", "KL", "KL",
                             "KSZ",  "LP", "MD", "OL", "OL", "OS", "SO", "SO.K", "SO.S", "SO.WE", "ŚL", "ŚL", "ŚW",
                             "TP", "WB", "WB.NO", "WZ", "WZ")

levels(dane$GAT_05_DO_3) <- c("", "AK", "BK", "BRZ", "BRZ", "BST", "CIS", "CZR", "CZR", "DB", "DB", "DB",
                               "DB.C", "DB", "DG", "GB", "GB ", "GR", "IWA", "JB", "JD", "JKL", "JS", "JW", "KL", 
                               "KL", "KL", "KSZ", "LP", "MD", "OL", "OL", "OS", "SO", "SO.B", "SO.C", "SO.K", 
                               "SO.S", "SO.WE", "ŚL", "ŚL", "ŚW", "TP", "WB", "WB.NO", "WIŚ", "WZ", "WZ" )

levels(dane$GAT_3_DO_7) <- c("",   "AK", "BK", "BRZ", "BRZ.O", "CIS", "CZR", "CZR.P", "DB", "DB.B", "DB.C", "DB.S", 
                              "DG", "GB", "GR", "IWA", "JB", "JD", "JKL", "JS", "JW", "KL", "KL.P", "KSZ", "LP", 
                              "MD", "OL", "OL.S", "OS", "PLA.K", "SO", "SO.B", "SO.C", "SO.K", "SO.S", "SO.W", "SO.WE",
                              "ŚL.A", "ŚL.L", "ŚW", "WB", "WB.NO", "WIŚ", "WZ", "WZ.P" )
#########
# wybór powierzchni z panującą sosną oraz pozbycie się upraw
#########

dane %>%
  filter(GAT_PAN_PR == "SO" & POW_A_B_NR_CYKLU == 1 & WIEK_PAN_PR > 20) -> dane1

dane %>%
  filter(GAT_PAN_PR == "SO" & WIEK_PAN_PR > 20) -> dane2












##########
### Grupowanie gatunkami
##########

dane2 %>%
  group_by(GAT_DO_05) %>%
  summarise(n = n_distinct(NR_PODPOW)) %>%
  filter(GAT_DO_05 != "") %>%
  arrange(desc(n)) %>%
  rename(gat = GAT_DO_05)  -> gat_do_05

dane2 %>%
  group_by(GAT_05_DO_3) %>%
  summarise(n = n_distinct(NR_PODPOW)) %>%
  filter(GAT_05_DO_3 != "") %>%
  arrange(desc(n)) %>%
  rename(gat = GAT_05_DO_3)  -> gat_do_3

dane2 %>%
  group_by(GAT_3_DO_7) %>%
  summarise(n = n_distinct(NR_PODPOW)) %>%
  filter(GAT_3_DO_7 != "") %>%
  arrange(desc(n)) %>%
  rename(gat = GAT_3_DO_7) -> gat_do_7

tabela <- full_join(full_join(gat_do_05, gat_do_3, by = "gat"), gat_do_7, by = "gat")
names(tabela) <- c("gat", "gat_do_05", "gat_do_3", "gat_do_7")
tabela$gat <- factor(tabela$gat, levels = tabela$gat[order(-tabela$gat_do_3)])
# levels(tabela$gat) <- c("DB", "SO", "BRZ", "DB", "BK", "GB", "DB", "ŚW", "JW", "OS", "obce-liś", "KL", "JD", "LP", 
# "AK", "JS", "CZR", "WZ", "MD", "OL", "OL", "ŚL", "BRZ", "IWA", "JKL", "obce-igl", "obce-igl", "WB", 
# "CZR.P", "KL.P", "DB", "KSZ", "obce-igl", "ŚL.L", "TP", "WB.NO", "GR", "obce-igl", "DB.BU", "JB", "WIŚ", "obce-igl", "obce-igl" )
tabela <- tabela[1:10,]
tabela <- arrange(tabela, desc(gat_do_05))

tabela2 <- gather(tabela, "wys", "n", 2:4)

ggplot(data = tabela2, aes(x = gat, y = n, fill = gat)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_grid(~ wys) + 
  theme_minimal()

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

# dane_punktowe <- read.csv2("dane/gps.csv", dec = ".") #data loading
# str(dane_punktowe) #data structure
# head(dane_punktowe) #data first 6 rows
# colnames(dane_punktowe) <- c("nr", "rok", "szer", "dl") #data columns name change
# coordinates(dane_punktowe) <- ~ dl + szer #adding sptial relationship
# proj4string(dane_punktowe) <- "+init=epsg:4326" #adding WGS84 projection
# summary(dane_punktowe)
# # plot(dane_punktowe)
# writeOGR(dane_punktowe, dsn="dane", layer="punkty", driver="ESRI Shapefile") #saving projected file

# dane_punktowe <- read.csv2("dane/gps2.csv", dec = ".")
# colnames(dane_punktowe) <- c("nr", "rok", "szer", "dl")
# dane_punktowe %>%
#   filter(dl > 20 & dl < 21 & szer > 50 & szer < 51) -> punkty2
# coordinates(punkty2) <- ~ dl + szer
# proj4string(punkty2) <- "+init=epsg:4326"
# plot(punkty2)
# writeOGR(punkty2, dsn="dane", layer="punkty2", driver="ESRI Shapefile") #saving projected file

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


