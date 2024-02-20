# ——————————————————————————————————————————————— #
# ——— Clean data: Czech Repbulic — monitoring ——— # 
# ——————————————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")

# load data -------------------------------------------------------------------------

data  <- fread("01_data/01_raw/czech_monitoring/CZECH_state_monitoring_updated.csv") 

# prepare data ----------------------------------------------------------------------

# X and Y coordinates are crossed on purpose.
data2 <- data.table(
        original_site_name = data$site_id,
        site_name = data$`site name`,
        date               = dmy(data$date),
        taxon              = data$`taxon name`,
        x.coord = data$JTSK_Y * -1,
        y.coord  = data$JTSK_X * -1,
        EPSG = 2065, # S-JTSK (Ferro) / Krovak
        abundance = 1
)

data2 <- data2[!is.na(x.coord)]
data2 <- data2[x.coord != 0]

data2 |> 
        unique(by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) -> 
        sites 

mapview(sites)

#- some site are outside of Czech Republic 
#- get shapefile for Czech Republic   
#- here I use the naturalearth data set because geodata::gadm() and geodata::world()returned an error 
#- (cannot open URL 'https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_adm0_r5_pk.rds': HTTP status was '403 Forbidden') 
#- apparently something is awry on the gadm website (gadm.org) )(date:04.10.21 @ 7:00 AM)
#- Result: for the outside-czech data; x and y coordinates are swapped. 
czech <- st_read("D://Arbeit/Data/natural_earth/Europe/Europe.shp")
czech %<>% filter(SOV_A3 == "CZE") %>% st_transform(crs = st_crs(sites))

data.in.cz <- st_within(x = sites, 
                        y = st_buffer(czech, dist = units::as_units(15, "km")))
outside_id <- which(lengths(data.in.cz) == 0)

outside_id <- sites[outside_id,"original_site_name"]

# out.sites <- filter(sites, !original_site_name %in% outside_id$original_site_name)
# mapview(out.sites)

data2[original_site_name %in% outside_id$original_site_name, c("y.new", "x.new") := .(x.coord, y.coord)]

data2 |> unique(by="original_site_name") |> filter(!is.na(y.new)) |> st_as_sf(coords = c("y.coord", "x.coord"), crs = data2$EPSG[1]) |> mapview()

data2[!is.na(y.new), c("y.coord", "x.coord") := .(y.new, x.new)]

data2[, c("y.new", "x.new") := NULL]

data2 |> 
  unique(by = "original_site_name") |> 
  st_as_sf(coords = c("x.coord", "y.coord"), crs = data2$EPSG[1]) |> 
  mapview()

data2[, `:=` 
      (year = year(date))
]
#- clean up taxon names 
data2[, taxon := str_remove(taxon, "\\ sp\\.")]
data2[, taxon := str_remove(taxon, "\\ Ad\\.")]
data2[, taxon := str_remove(taxon, "\\ Lv\\.")]
data2[, taxon := str_remove(taxon, "\\ Gen\\.")]
data2[, taxon := str_remove(taxon, "-Gr\\.")]
data2[, taxon := str_remove(taxon, "\\ agg\\.")]
data2[taxon == "Clinocera/Wiedemannia", taxon := "Empididae"]
data2[, taxon := str_trim(taxon)]
data2[str_detect(taxon, "\\/"), taxon := word(taxon, 1)]
data2[str_detect(taxon, "�"), taxon := str_remove(taxon,"�\\ ")]

data.set.name <- "czech_monitoring"
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/czech_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/czech_biodrought/updated_types.rds")
source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/czech_monitoring/preped.rds")

