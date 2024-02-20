# ——————————————————————————————————————— #
# ——— Clean data: Norway — MONITORING ——— # 
# ——————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------
source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------
data  <- read_excel("01_data/01_raw/norway_moitoring/NO_data_inverts_2017_2018_pres_abs.xlsx")

# PREPARE DATA   ---------------------------------------------------------------------

setDT(data)
data[, sample_id := 1:.N]
sites <- data[, c("year","station","date",                        
                  "season","latitude","longitude", "sample_id")]
#- check sites 
# sites |> 
#         st_as_sf(coords = c("longitude", "latitude"), crs=4326) |> 
#         mapview()

bio <- data[, Acari:sample_id]
bio %<>% pivot_longer(cols = !sample_id, names_to = "taxon", values_to = "abundance")
setDT(bio)
bio[str_detect(taxon, "\\ Ad\\."), taxon := str_remove(taxon, "\\ Ad\\.")]
bio[str_detect(taxon, "\\ ad\\."), taxon := str_remove(taxon, "\\ ad\\.")]
bio[str_detect(taxon, "\\ Lv\\."), taxon := str_remove(taxon, "\\ Lv\\.")]
bio[str_detect(taxon, "\\ lv\\."), taxon := str_remove(taxon, "\\ lv\\.")]
bio[taxon == "Baetis scambus/fuscatus", taxon := "Baetis"]
bio[taxon == "Capniidae/Leuctridae", taxon := "Plecoptera"]
bio[taxon == "Limoniidae/Pediciidae", taxon := "Diptera"]
bio[taxon == "Radix labiata/balthica", taxon := "Radix"]


bio <- bio[abundance != 0]
bio[, abundance := sum(abundance), by = c("sample_id", "taxon")]
bio <- unique(bio, by = c("sample_id", "taxon"))

data <- sites[bio, on = "sample_id"]
data[date == "43027", date := "19.10.2017"]
data[date == "43034", date := "26.10.2017"]
data[date == "43035", date := "27.10.2017"]
data[, date2 := dmy(date)]
data <- data[, date := NULL]

data%<>%rename(original_site_name = station,
               date = date2,
               x.coord = longitude,
               y.coord = latitude)

data[, c("data.set", "EPSG") := .("monitoring_norway", 4326)]

data.set.name = "norway_monitoring"
data2 <- data
source("02_R/91_misc/02_prep_step_1.R")
# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/norway_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/norway_monitoring/updated_types.rds")
source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/norway_monitoring/preped.rds")


