# ———————————————————————————————————————— #
# ——— Clean data: Finland — Monitoring ——— # 
# ———————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------
source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA -------------------------------------------------------------------------

data  <- read_excel("01_data/01_raw/finland_monitoring/Finland_Stream_Macroinvert_Data_N410_ToJupke.xlsx") 

# prepare data ----------------------------------------------------------------------

# - Transform data to data.table format. 
setDT(data)


# - X and Y coordinates are crossed on purpose.
sites <- data.table(
        original_site_name = data$SITENAME,
        date               = ymd_hms(data$SAMPLINGDATE),
        y.coord = data$`P_ETRS-TM35FIN`,
        x.coord  = data$`I_ETRS-TM35FIN`,
        EPSG = 3067 # TM35FIN(E,N) -- Finland
)

# - Is there only one X coordinate per sampling site?
all(sites[,uniqueN(x.coord), by = original_site_name]$V1 == 1)
# - The same for the Y coordinate. 
all(sites[,uniqueN(y.coord), by = original_site_name]$V1 == 1)

sites.plot <- 
        sites |> 
        unique(by="original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])

mapview(sites.plot)

# - biological data 

# - at which column do taxa abundances begin? 
bio.col <- 23

bio <- data[,bio.col:ncol(data)]
bio[, original_site_name := data$SITENAME]
bio %<>% 
        pivot_longer(cols = !original_site_name, names_to = "taxon", values_to = "abundance") %>% 
        filter(!is.na(abundance))

data2 <- left_join(bio, sites)
setDT(data2)


data2[, `:=` (year = year(date))]



#- clean up taxon names 
data2[, taxon := str_remove(taxon, "\\ group$")]
data2[, taxon := str_remove(taxon, "\\.\\.\\..*")]
data2[, taxon := str_trim(taxon)]
data.set.name = "finland_monitoring"
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/finland_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/finland_monitoring/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/finland_monitoring/preped.rds")
