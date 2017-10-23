library(tidyverse)
library(odbc)
library(DBI)
library(rgdal)
library(stringr)
library(forcats)

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

#############################################
### Data import and tidy for PhD research ###
#############################################

library(odbc)
library(DBI)
library(tidyverse)

# database connection -----
testdb <- file.path("D:\\Praca\\Badania\\WISL\\WISL_10lat_SGGW") #determining database filepath
con <- dbConnect(odbc::odbc(), dsn = "WISL", encoding = "Windows-1250")#connecting to db
# dbListTables(con) #listing all tables available in th database

# data loading -----
sites_raw <- dbReadTable(con, "ADRES_POW") #site description
trees_raw <- dbReadTable(con, "DRZEWA_OD_7") #tree measurments
plot_a_raw <- dbReadTable(con, "POW_A_B") #plot data
gps_coord_raw <- dbReadTable(con, "PUNKTY_TRAKTU") # GPS coordinates

# data wrangling -----
# I am querying for area of sample plot needed later
plot_a_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  select(nr_podpow = NR_PODPOW, # selecting only interesitng colmuns
         pow = POW_A) -> plot_a # creating a new tibble

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

# loading tree data for the next script
trees_raw %>%
  as_tibble(.) %>%
  rename_all(tolower) %>%
  select(nr_punktu, nr_cyklu, nr_podpow, gat, wiek, war, azymut, odl, h, d13) %>%
  dplyr::filter(nr_cyklu == 2, war == 1) %>%
  type_convert(., col_types = cols(nr_punktu = col_integer()))-> trees

# sample for WMS_GetFeatureInfo.R testing -----
# write_tsv(sample_n(gps_coord, 100), "gps.coord.sample.txt")

# joining files -----
sites_area <- dplyr::left_join(sites, plot_a, by = "nr_podpow") # joining sample plot area to site description
sites_area_gps <- dplyr::left_join(sites_area, gps_coord, by = "nr_punktu") # adding GPS position data

# exporting data ----- 
write_tsv(sites_area_gps, "sites_area_gps.txt") # saving tabular format for later analysis
write_tsv(trees, "trees.txt")

# database loading -----
testdb <- file.path("D:\\Praca\\Badania\\WISL\\WISL_10lat_SGGW") #determining database filepath
con <- dbConnect(odbc::odbc(), dsn = "WISL", encoding = "Windows-1250")#connecting to db
dbListTables(con) #listing all tables available

# GPS positions ------
gps_coord <- type_convert(as_tibble(dbReadTable(con, "PUNKTY_TRAKTU")))
gps_coord_sdf <- SpatialPointsDataFrame(gps_coord[,c(4, 3)], gps_coord)

# drzewa do 0,5 m wysokości -----
DRZEWA_DO_05 <- type_convert(as_tibble(dbReadTable(con, "DRZEWA_DO_05")))
drop_cols <- c("LP", "NR_CYKLU", "WAR", "USZK_RODZ2", "USZK_PROC2", "ID") #list of coulmn to drop later on
DRZEWA_DO_05 %>% filter(NR_CYKLU == "2" & WAR == "1") %>% select(-one_of(drop_cols)) -> DRZEWA_DO_05_filter #filtering only newest cycle and only trees

DRZEWA_DO_05_filter$GAT <- factor(DRZEWA_DO_05_filter$GAT)
DRZEWA_DO_05_filter$POKR <- factor(DRZEWA_DO_05_filter$POKR)
DRZEWA_DO_05_filter$USZK_RODZ1 <- factor(DRZEWA_DO_05_filter$USZK_RODZ1)

DRZEWA_DO_05_filter_gps <- left_join(DRZEWA_DO_05_filter, gps_coord, by = "NR_PUNKTU")

str_c(levels(DRZEWA_DO_05_filter_gps$GAT), collapse = "', '")

levels(DRZEWA_DO_05_filter_gps$GAT) <- c('AK', 'BK', 'BRZ', 'BRZ', 'BST', 'CZR', 'CZR', 'DB', 'DB', 'DB.C', 'DB', 
                                         'DG', 'GB', 'GR', 'IWA', 'JB', 'JD', 'JKL', 'JS', 'JW', 'KL', 'KL', 
                                         'KL.T', 'KSZ', 'LP', 'MD', 'OL', 'OL', 'OS', 'SO', 'SO.B', 'SO.C', 
                                         'SO.K', 'SO.S', 'SO.WE', 'ŚL', 'ŚL.A', 'ŚL.L', 'ŚW', 'TP', 'TP.C', 
                                         'WB', 'WB.NO', 'WIŚ', 'WZ', 'WZ.P')

summary(DRZEWA_DO_05_filter_gps)

wykres <- DRZEWA_DO_05_filter_gps %>% 
  group_by(GAT) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 

ggplot(wykres, aes(GAT)) +
  geom_bar()

test$GAT <- factor(DRZEWA_DO_05_filter_gps, level$GAT)


zrodlo <- "dane\\kwerendaMSGP.txt"

dane <- read.csv2(zrodlo)

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


