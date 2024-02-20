# ———————————————————————————————————————————————— #
# ——— Clean data: Finland — koutajoki ——— # 
# ———————————————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")


# LOAD DATA  ------------------------------------------------------------------------

samples    <- read_excel("01_data/01_raw/finland_koutajoki/MI_data_KLHuttunen.xlsx") 
sites      <- read_excel("01_data/01_raw/finland_koutajoki/MI_data_KLHuttunen.xlsx", sheet = 2) 


# PREPARE DATA  ---------------------------------------------------------------------
setDT(samples)
setDT(sites)

samples <- melt(samples, id.vars = c("Site", "Year"),
             value.name = "abundance", 
             variable.name = "taxon")

samples <-  samples[abundance != 0]

# In the paper it is also stated that all sites are pristine 

data <- samples[sites, on = "Site"]

data2 <- data[, list(
        original_site_name = Site,
        year = Year,
        taxon = as.character(taxon),
        abundance,
        x.coord = EUREF_Lon_E,
        y.coord = EUREF_Lat_N,
        EPSG = 4258,
        data.set = "kaisa-leena_huttunen"
)]

data2[, date := paste0("05/05/",year)]
data2[, date := dmy(date)]

#- taxonomy 
data2[, taxon := str_remove_all(taxon, "\\_group$")]
data2[, taxon := str_remove_all(taxon, "\\_other$")]
data2[, taxon := str_remove_all(taxon, "\\ sp\\.$")]
data2[taxon == "Baetis niger/digitatus", taxon := "Baetis"]
data2[taxon == "Hemerodromia/Wiedemannia", taxon := "Empididae"]
data2[taxon == "Leuctra digi/fusc/hipp", taxon := "Leuctra"]
data2[taxon == "Pericomini/Telmatoscopini", taxon := "Psychodidae"]
data2[taxon == "Rhyacophila obliterata/fasciata", taxon := "Rhyacophila"]

data.set.name = "finaland_koutajoki"
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/finland_koutajoki/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/finland_koutajoki/updated_types.rds")
source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/finland_koutajoki/preped.rds")

