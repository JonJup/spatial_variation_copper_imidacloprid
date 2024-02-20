# ——————————————————————————————————— #
# ——— Clean data: UK — MONITORING ——— # 
# ——————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------
source("02_R/91_misc/01_setup_prep_data.R")

taxa  <- fread("01_data/01_raw/uk_monitoring/INV_OPEN_DATA_TAXA_2021-09-03.csv") 
sites <- fread("01_data/01_raw/uk_monitoring/INV_OPEN_DATA_SITE_2021-09-03.csv") 

# prepare data ----------------------------------------------------------------------
taxa <- taxa[TAXON_TYPE == "Other Macroinvertebrates"]
taxa <- taxa[SAMPLE_METHOD_DESCRIPTION == "3-MIN POND NET (BT001): 3-min active sampling, 1-min hand search as per BT001"]
taxa2 <- taxa[,c("SITE_ID", "SAMPLE_DATE", "TAXON_NAME", "TOTAL_ABUNDANCE")]
names(taxa2) <- c("original_site_name", "date", "taxon", "abundance")
taxa2[, date := ymd(date)]
sites <- sites[SITE_ID %in% taxa2$original_site_name]
sites <- sites[,c("SITE_ID", "FULL_EASTING", "FULL_NORTHING")]
# sites_test <- st_as_sf(sites, coords = c("FULL_EASTING", "FULL_NORTHING"), crs = 27700)
# mapview(sites_test)
names(sites) <- c("original_site_name", "x.coord", "y.coord")
data <- sites[taxa2, on = "original_site_name"]

data[, EPSG := 27700]
data[, `:=` (year = year(date))]
data[str_detect(taxon, "\\("), taxon := str_remove(taxon, "\\(.*\\)")]
data[, taxon := str_remove(taxon, "Larval\\ Species\\ C")]
data[str_detect(taxon, "\\/"),  taxon := word(taxon, 1)]
data[, taxon := str_trim(taxon)]
data <- data[taxon != "Nemathelmintha"]

data2 <- data
data.set.name = "uk_monitoring"
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/uk_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/uk_monitoring/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/uk_monitoring/preped.rds")




