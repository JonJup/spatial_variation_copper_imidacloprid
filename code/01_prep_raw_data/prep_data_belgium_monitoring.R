# ———————————————————————————————————————— #
# ——— Clean data: Belgium — Monitoring ——— # 
# ———————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------
source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------

taxa  <- fread("01_data/01_raw/belgium_monitoring/occurrence.txt") 
sites <- fread("01_data/01_raw/belgium_monitoring/event.txt") 

# prepare data ----------------------------------------------------------------------
names(taxa)
names(sites)
names(sites)[1] <- "eventID"
all(taxa$eventID %in% sites$eventID)
data <- sites[taxa, on = "eventID"]
names(data)
sort(table(data$samplingProtocol))
#- keep only handnet (=kicknet) samples 
data <- data[samplingProtocol == "Handnet"]

sort(unique(data$habitat))

data <- data[habitat %in% c("RtNt:River;Water body", "BkK:Small stream Kempen;Water body", "Bk:Small stream;Water body", "Bg:Large stream;Water body", "BgK:Large stream Kempen;Water body",
                            "Rk:Small river;Water body", "Rg:Large river;Water body", "No type:ditch;Water body", "Rzg:Very large river;Water body")]

data2 <- data.table(
        original_site_name = data$locationID,
        date               = ymd(data$eventDate),
        taxon              = data$scientificName,
        x.coord = data$verbatimLongitude,
        y.coord  = data$verbatimLatitude,
        EPSG = 31370,
        data.set = "monitoring_belgium",
        abundance = 1,
        countryCode = data$countryCode
)

# data2 |> 
#         unique(by = "original_site_name") |> 
#         st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) -> 
#         sites 
# mapview(sites)


data2[, `:=` 
     (year = year(date))
]
data.set.name = "belgium_monitoring"
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/belgium_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/belgium_monitoring/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/belgium_monitoring/preped.rds")


