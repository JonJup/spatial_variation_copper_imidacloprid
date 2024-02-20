# ————————————————————————————————————— #
# ——— Clean data Hungary  — Ecosurv ——— # 
# ————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------

data       <- read_excel("01_data/01_raw/hungary_ecoserv/Hungary-data2.xlsx") 

# PREPARE DATA   ---------------------------------------------------------------------
setDT(data)

# - information on year and season is taken from  Schmera & Bauer (2011): Testing a
# - typology system of running waters for conservation planning in Hungary

data = data[, 
            list(   original_site_name = SiteID,
                    year =  2005,
                    x.coord = EOVY,
                    y.coord = EOVX,
                    taxon = TAXON,
                    EPSG = 23700,
                    date = as.Date("01/05/2005"), # placeholder,
                    abundance = 1
            )
]

data2 <- data
data.set.name = "hungary_ecosurv"
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/hungary_ecosurv/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/hungary_ecosurv/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")

# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/hungary_ecosurv/preped.rds")

