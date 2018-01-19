### Data import and tidy for MSc research  -------------------------------------------------------------------------

library(odbc)
library(DBI)
library(tidyverse)
library(feather)

# database connection -----------------------------------------------------------------------------------------------
testdb <- file.path("D:\\Praca\\Badania\\WISL\\WISL_10lat_SGGW") # determining database filepath
con <- dbConnect(odbc::odbc(), dsn = "WISL", encoding = "Windows-1250")# connecting to db
# dbListTables(con) # listing all tables available in th database
 

# data loading ------------------------------------------------------------------------------------------------------
trees_05_raw <- dbReadTable(con, "DRZEWA_DO_05") # trees shorter than 0.5 m
trees_05_3_raw <- dbReadTable(con, "DRZEWA_05_DO_3") # trees teller than 0.5 m and smaller than 3 cm dbh
trees_3_7_raw <- dbReadTable(con, "DRZEWA_3_DO_7") # trees between 3 and 7 cm dbh
trees_7_raw <- dbReadTable(con, "DRZEWA_OD_7") # trees above 7 cm dbh
plot_raw <- dbReadTable(con, "POW_A_B") # sample plot data
gps_coord_raw <- dbReadTable(con, "PUNKTY_TRAKTU") # GPS coordinates
sites_raw <- dbReadTable(con, "ADRES_POW") # site description
dbDisconnect(con)

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

# lowest trees (h < 0.5 m) data wrangling ---------------------------------------------------------------------------
trees_05_col <- c("gat", "pokr", "war", "uszk_rodz1", "uszk_proc1")
trees_05_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  filter(war == 1) %>% # we want only trees
  dplyr::select(-c(nr_cyklu, lp, id, uszk_rodz2, uszk_proc2)) %>%
  mutate_at(trees_05_col, funs(factor(.))) %>%
  mutate(gat = fct_collapse(gat, 
                            DB = c("DB", "DB.S", "DB.B"),
                            BRZ = c("BRZ", "BRZ.O"))) -> trees_05 # creating a new tibble

trees_05$pokr <- factor(trees_05$pokr, ordered = TRUE, levels = c("+", "1", "5", "10", "15", "20", "25", "30", "35", "40", "45",
                                                                  "50", "55", "60", "65", "70", "75", "80", "85", "90", 
                                                                  "95", "100"))

# medium trees (h > 0.5 m & d < 3 cm) data wrangling -----------------------------------------------------------------
trees_05_3_col <- c("gat", "war", "uszk_rodz1", "uszk_proc1")
trees_05_3_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  filter(war == 1) %>%
  dplyr::select(-c(nr_cyklu, lp, id, uszk_rodz2, uszk_proc2)) %>%
  mutate_at(trees_05_3_col, funs(factor(.))) %>%
  mutate(gat = fct_collapse(gat, 
                            DB = c("DB", "DB.S", "DB.B"),
                            BRZ = c("BRZ", "BRZ.O"))) -> trees_05_3 # creating a new tibble

# high trees (d > 3 cm & d < 7 cm) data wrangling -----------------------------------------------------------------
trees_3_7_col <- c("gat", "war", "uszk_rodz1", "uszk_proc1")
trees_3_7_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  filter(war == 1) %>%
  dplyr::select(-c(nr_cyklu, lp, id, uszk_rodz2, uszk_proc2)) %>%
  mutate_at(trees_3_7_col, funs(factor(.))) %>%
  mutate(gat = fct_collapse(gat, 
                            DB = c("DB", "DB.S", "DB.B"),
                            BRZ = c("BRZ", "BRZ.O"))) -> trees_3_7 # creating a new tibble

# higher trees (d > 7 cm, not in canopy level) data wrangling -----------------------------------------------------------------
trees_7_col <- c("gat", "war", "uszk_rodz1", "uszk_proc1")
trees_7_raw %>% # raw data loading to pipe
  as_tibble(.) %>% # changing format to tibble
  filter(NR_CYKLU == 2) %>% # filtering out only second cycle
  rename_all(tolower) %>%
  filter(war == 2 | war == 3) %>%
  filter(odl >= 56 & odl <= 259) %>% # using the same dimensions as for other layers of sample plot
  dplyr::select(nr_punktu, nr_podpow, gat, war, uszk_rodz1, nasil1) %>%
  rename(uszk_proc1 = nasil1) %>%
  mutate_at(trees_7_col, funs(factor(.))) %>%
  type_convert(col_types = cols_only(nr_punktu = "i")) %>%
  mutate(gat = fct_collapse(gat, 
                            DB = c("DB", "DB.S", "DB.B"),
                            BRZ = c("BRZ", "BRZ.O"))) %>%
  filter(gat %in% c("AK", "BK", "BRZ", "CZR", "DB", "DB.C", "DG", "GB", "IWA", "JB", "JD", "JKL", "JRZ", "JS", 
                    "JW", "KL", "KL.P", "LP", "MD", "OL", "OL.S", "OS", "SO", "ŚW", "WB", "WZ", "WZ.P")) -> trees_7 # creating a new tibble

# data saving -------------------------------------------------------------------------------------------------------
write_feather(trees_05, "trees_05.feather")
write_feather(trees_05_3, "trees_05_3.feather")
write_feather(trees_3_7, "trees_3_7.feather")
write_feather(trees_7, "trees_7.feather")
write_feather(plot, "plot.feather")
write_feather(gps_coord, "gps_coord.feather")
write_feather(sites, "sites.feather")
