#----------------------------------- #
#--- Clean data from Sweden Miljö--- # 
#----------------------------------- #

# The raw data were downloaded from: 
# https://miljodata.slu.se/MVM/Search
# on the 28.04.2022

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------
data <- read_excel("01_data/01_raw/sweden/slu_mvm_220428_094841444_data.xlsx", sheet = 2)

# PREPARE DATA  ---------------------------------------------------------------------

# - reshape into table with only relevant variables
data2 <- data.table(
        "original_site_name" = pull(data[,1]) ,
        "date"               = ymd(data$Provdatum),
        "taxon"              = data$Taxonnamn,
        "x.coord"            = data$'Stationskoordinat E/Y',
        "y.coord"            = data$'Stationskoordinat N/X',
        "EPSG"               = 3006,
        "abundance"          = data$'Medelantal per prov'  
)

# - add variables year and season
data2[,c("year") := .(year(date))]

# - fix abundance column
data2[, abundance := str_replace(abundance, ",", "\\.")]
data2[, abundance := as.numeric(abundance)]

# - check sites on map
# data2 |>
#         unique(by = "original_site_name") |>
#         st_as_sf(coords = c("x.coord", "y.coord"), crs = 3006) |>
#         mapview()

data.set.name = "miljodata_sweden"

source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
updated_type <- readRDS("01_data/02_prepared/01_single_data/sweden/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")

# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/sweden/preped.rds")

