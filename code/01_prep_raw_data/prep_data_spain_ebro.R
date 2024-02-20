# —————————————————————————————————————————————————— #
# ——— Clean Ebro Hydrographic Confederation data ——— # 
# —————————————————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")

# load data  ------------------------------------------------------------------------
samples1   <- read_excel("01_data/01_raw/spain_ebro/macroinvertebrates_2000-2009.xlsx") 
samples2   <- read_excel("01_data/01_raw/spain_ebro/macroinvertebrates_2010-2020.xlsx")

# prepare data  ---------------------------------------------------------------------
setDT(samples1)
setDT(samples2)

samples <- rbindlist(list(samples1, samples2))    

#- keep only abundance records 
samples <- samples[Parámetro == "Abundancia macroinvertebrados"]

#- separate indices for pristine sites
names(samples)[c(1, 13, 14)] = c("site_id", "parameter", "taxon")

data <- samples[,
               list(
                       date = dmy(Fecha),
                       original_site_name = site_id,
                       taxon =  taxon,
                       x.coord = ETRS89_X30,
                       y.coord = ETRS89_Y30 ,
                       EPSG = 25830,
                       abundance = Valor
               )]

data[,  year := year(date)]
data <- data[!(taxon %in% c("--"))]

#- taxonomy
data[, taxon := str_remove_all(taxon, "^Fam.\\ ")]
data[, taxon := str_remove_all(taxon, "^Clase\\ ")]
data[, taxon := str_remove_all(taxon, "^Filo\\  ")]
data[, taxon := str_remove_all(taxon, "\\ sp\\.$")]
data[, taxon := str_remove_all(taxon, "^Orden\\ ")]
data[taxon == "Polyarthra vulgaris-dolichoptera", taxon := "Polyarthra"]

data2 <- data

# - check sites on map
data2 |>
        unique(by = "original_site_name") |>
        st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) |>
        mapview()
data.set.name <- "spain_ebro"
data2[abundance == "+", abundance := "1"]
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/01_raw/spain_ebro/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/spain_ebro/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")

# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/spain_ebro/preped.rds")

