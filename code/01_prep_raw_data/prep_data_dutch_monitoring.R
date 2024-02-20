# ———————————————————————————————————————— #
# ——— Clean data: Belgium — Monitoring ——— # 
# ———————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------
source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------
sites      <- read_csv2("01_data/01_raw/dutch_monitoring/MaFa_Description_Locations_Getreal.csv")
samples    <- read_csv2("01_data/01_raw/dutch_monitoring/MaFa_Getreal_Genus.csv")


# PREPARE DATA --------------------------------------------------------

#- transform objects to data.table 
setDT(sites)
setDT(samples)

#- drop sites that are not from running waters 
#- this reduces the number of sites from 14812 to 4237
#- dropping sites with either x or y coordinates missing further reduces to 4037
sites <- sites[Watertype %in% c("BRN","KRK" ,"OUD" ,"RIV", "STR") & !is.na(X_coord) & !is.na(Y_coord)]

#- join sample and site data 
data <- samples[sites, on = "Location"]
#- there are some rows with empty GenusName
data2 = data[,list(
        original_site_name = Location,
        date = dmy_hms(Date),
        taxon = GenusName,
        abundance = Counts,
        x.coord = X_coord/100,
        y.coord = Y_coord/100,
        EPSG = 28992
)]

#- create year variable  
data2[, year := year(date)]
#- drop rows without taxa denomination 
data2 <- data2[!is.na(taxon)]
#- taxonomy 
data2[, taxon := str_remove(taxon, "_indet$")]
data2[, taxon := str_remove(taxon, "_indete$")]
data2[, taxon := str_remove(taxon, "_INDET$")]
#- Agabus and Iybius are genera in Dytiscidae
data2[taxon == "Agabus / Ilybius indet", taxon := "Dytiscidae"]
# - remove some taxa 
data2 <- data2[! taxon %in% c("Amphichaete", "Aschelminthes", "Giardia", "Salctula")]
data.set.name = "dutch_monitoring"

data2 %<>% filter(!original_site_name %in% c("NL25_220005b","NL25_210204b", "NL25_210016b", 
                                             "NL25_210402b", "NL25_221602b", "NL25_210708b",
                                             "NL25_400005b", "NL25_210821b", "NL25_221601b",
                                             "NL25_211017b", "NL25_240001b", "NL25_300001b", 
                                             "NL25_230001b", "NL25_590801b", "NL25_210812b",
                                             "NL60_OROTB800"))

# data2 |> 
#         unique(by = "original_site_name") |> 
#         st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) -> 
#         sites 
# mapview(sites)

source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/dutch_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/dutch_monitoring/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/dutch_monitoring/preped.rds")
