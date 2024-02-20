# ————————————————————————————————————————————————— #
# ——— Clean data: GERMANY — MONITORING - LANDAU ——— # 
# ————————————————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------
source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------

ld.samples          <- fread("01_data/01_raw/germany_landau/mzb_samples.csv")
ld.sites            <- fread("01_data/01_raw/germany_landau/mzb_sites.csv")

# PREPARE DATA  ---------------------------------------------------------------------
#  —————— LANDAU ——————
ld.samples <- 
        ld.samples |> 
        mutate(year = year(date)) %>%
        filter(ind_qm > 0)
#- drop columns  
ld.samples <- ld.samples[, c("date", "taxon", "site_id", "ind_qm") ]

# In samples the sites with TH in their site_id seem to have mistakes. All TH ..
# sites in the samples data go like TH_TH_xxxx while those in the sites data go
# TH_xxxx. Hence I remove the first TH 
ld.samples[, site_id := str_replace_all(site_id, "TH_TH_", "TH_")]
ld.sites  <-  ld.sites[, c("site_id", "stream", "site_name", "geom")]

#fix site coordinates 
#Geometry type PostgreSQL columns They can be converted with sf see
#https://github.com/r-dbi/RPostgres/issues/114 This converts the geom column
#which holds Postgres Geom codes to xy coordinates and also returns the
#projection.
coord <- st_as_sfc(
        structure(
                ld.sites$geom, 
                class = "WKB"
        ),
        EWKB = TRUE
)
coord2 <- 
        coord %>%
        st_coordinates() %>%
        data.frame()
ld.sites2  <- 
        bind_cols(
        ld.sites,
        coord2,
        EPSG = rep(31463, nrow(ld.sites))
) 
ld.sites2  <-  ld.sites2[,-c("geom")]
# join data sets 
data <-  
        left_join(ld.samples, ld.sites2) %>% 
        setDT
rm(coord,coord2,ld.samples,ld.sites,ld.sites2)

# fix date column
data[,c("date", "year", "month") := list(ymd(date), year(date), month(date))]

data2 <- data[,
                    list(
                            original_site_name = site_id,
                            date,
                            year,
                            taxon = taxon,
                            abundance = ind_qm,
                            x.coord = X,
                            y.coord = Y,
                            EPSG
                    )]

#- taxonomy 
data2[, taxon := str_remove_all(taxon, "\\ Lv\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ Ad\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ sp\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ gen\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ Gen\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ spec\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ \\(.*\\)")]
data2[str_detect(taxon, "/"), taxon := word(taxon, 1)]
data2[taxon == "Baetis niger/digitatus", taxon := "Baetis"]
data2[taxon == "Hemerodromia/Wiedemannia", taxon := "Empididae"]
data2[taxon == "Leuctra digi/fusc/hipp", taxon := "Leuctra"]
data2[taxon == "Pericomini/Telmatoscopini", taxon := "Psychodidae"]
data2[taxon == "Rhyacophila obliterata/fasciata", taxon := "Rhyacophila"]
data2 <- data2[taxon != "ungültig: 10057"]
data2 <- data2[taxon != "ungültig: 792"]

#- check on map 
# ld.sites  <- 
#         data2 |> 
#         unique(by = "original_site_name") |> 
#         st_as_sf(coords = c("x.coord", "y.coord"), crs = 31463)
# mapview(ld.sites)
data.set.name = "germany_landau"
# - There is a duplicate entry in the data. The samples from the sites with the original names
# - TH_2033 and TH_4063 are identical. Therefore I drop the site TH_2033. 
data2 <- data2[original_site_name != "TH_2033"]
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#updated_type <- readRDS("01_data/02_prepared/01_single_data/germany_landau/quick_save_9628.rds")
#skip = 9628 
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/germany_landau/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/germany_landau/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/germany_landau/preped.rds")




