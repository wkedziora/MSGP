### Data import and tidy for MSc research  -------------------------------------------------------------------------

library(odbc)
library(DBI)
library(tidyverse) 

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

# data saving -------------------------------------------------------------------------------------------------------
write_tsv(trees_05_raw, "trees_05_raw.txt", na = "NA")
write_tsv(trees_05_3_raw, "trees_05_3_raw.txt", na = "NA")
write_tsv(trees_3_7_raw, "trees_3_7_raw.txt", na = "NA")
write_tsv(trees_7_raw, "trees_7_raw.txt", na = "NA")
write_tsv(plot_raw, "plot_raw.txt", na = "NA")
write_tsv(gps_coord_raw, "gps_coord_raw.txt", na = "NA")
write_tsv(sites_raw, "sites_raw.txt", na = "NA")
