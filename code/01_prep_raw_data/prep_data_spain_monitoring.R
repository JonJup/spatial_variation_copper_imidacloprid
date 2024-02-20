# —————————————————————————————————————— #
# ——— Clean data: Spain — Monitoring ——— # 
# —————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------

dt.ctln1.1 <- read_excel("01_data/01_raw/spain_monitoring/Catalan data.xlsx", sheet = 1) 
dt.ctln2.1 <- read_excel("01_data/01_raw/spain_monitoring/Catalan data.xlsx", sheet = 2) 
dt.as.1    <- read_excel("01_data/01_raw/spain_monitoring/Data long term Spain.xlsx", sheet = 1)

# PREPARE DATA   ---------------------------------------------------------------------

# Nuria wrote: "For the Spanish data, all should be data collected with the official
# sampling method but better to use the latter years" and "For the Catalan data, the
# period 2013-2018 was also sampled using the official method".

# —————— Data Catalan
#- transform to data.table 
setDT(dt.ctln1.1)
setDT(dt.ctln2.1)


#- all site names refer to more than one x.coordiante value hence I create a new unique identifier 
dt.ctln1.2 = data.table(
        taxon              = dt.ctln1.1$Taxa,
        abundance          = dt.ctln1.1$`Abundancia (ind/mostra, fins un màxim de 100)`,
        date               = ymd(dt.ctln1.1$DataRealització),
        x.coord            = dt.ctln1.1$UTMX,
        y.coord            = dt.ctln1.1$UTMY
                       )

dt.ctln2.2 = data.table(
        taxon              = dt.ctln2.1$Tbl_MacInv_Dic.Nombre,
        abundance          = dt.ctln2.1$Cantidad,
        date               = ymd(dt.ctln2.1$Fecha),
        x.coord            = dt.ctln2.1$UTMX,
        y.coord            = dt.ctln2.1$UTMY
                       )

dt.ctln <- rbindlist(list(dt.ctln1.2, dt.ctln2.2))
dt.ctln[, original_site_name := .GRP, by = c("x.coord", "y.coord")]

#- check that each site has only one coordinate 
all(dt.ctln[,uniqueN(x.coord), by = "original_site_name"]$V1 == 1)

#- assign year and EPSG code
dt.ctln[, year := year(date)]
dt.ctln[, EPSG := 25830]
 
summary(dt.ctln$year)

#- drop observations before 2013 or without date 
dt.ctln <- dt.ctln[!is.na(year) & year > 2012]

#- check sites on a map 
sites <-
        dt.ctln |>
        unique(by = "original_site_name") |>
        st_as_sf(coords = c("x.coord", "y.coord"), crs = dt.ctln$EPSG[1])

mapview(sites)

#- transform to WGS 84
dt.ctln %<>% 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = dt.ctln$EPSG[1]) %>%
        st_transform(crs = 4326)
#- add cat_ prefix to site name
dt.ctln %<>% mutate(original_site_name = paste0("cat_", original_site_name))

# —————— Monitoring all of Spain 
dt.as.2 <- copy(dt.as.1)

names(dt.as.2)%<>% str_remove_all("-")
names(dt.as.2)%<>% str_trim()

for (i in seq_along(names(dt.as.2))){
        if (names(dt.as.2)[i] %in% c("Site", "EU_water body", "surfaceWaterBodyName", "longitude", "latitude", "Sampling date")) next()
        x <- names(dt.as.2)[i]
        x1 <- word(x, -1)
        if (
                substring(x1, 1,1) == str_to_upper(substring(x1,1,1))
        ) {
                x2 <- x1
        } else {
                x1b <- word(x, -5)
                x2 <- paste(x1b, x1)
        }
        names(dt.as.2)[i] <- x2        
}
#- fix problems 
names(dt.as.2)[which(names(dt.as.2) == " caurelensis")]  <- "Philopotamus montanus"
names(dt.as.2)[which(names(dt.as.2) ==  " lusitanicus")] <- "Austropotamobius pallipes"
names(dt.as.2)[which(names(dt.as.2) ==  " saturniae")]   <- "Silo mediterraneus"
#- drop emtpy taxon column
dt.as.2 <- dt.as.2[,-which(names(dt.as.2) == "")]
#- reshape into long format 
dt.as.3 <- pivot_longer(data = dt.as.2, 
                        cols = Dytiscidae:Hydrochus,
                        names_to = "taxon", 
                        values_to = "abundance")
#- reshape into common format 
dt.as.4 <- data.table(
        taxon              = dt.as.3$taxon,
        abundance          = dt.as.3$abundance,
        x.coord            = dt.as.3$longitude,
        y.coord            = dt.as.3$latitude,
        EPSG               = 4326,
        date               = ymd(dt.as.3$`Sampling date`)
        )
dt.as.4[, original_site_name := .GRP, by = c("x.coord", "y.coord")]
dt.as.4[, original_site_name := paste0("as_",original_site_name)]
dt.as.4 <- dt.as.4[abundance != 0]
dt.as.4[, year := year(date)]
dt.as.4 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = 4326)

#- remove early years 
summary(dt.as.4$year)
dt.as.4 %<>% filter(year > 2012)

data <- bind_rows(dt.ctln, dt.as.4)

setDT(data)

data[, season := ifelse(month(date) %in% c(12,1,2), "winter", ifelse(month(date) %in% c(3,4,5), "spring", ifelse(month(date) %in% c(6,7,8), "summer", ifelse(month(date) %in% c(9,10,11), "autumn", "what?"))))]
any(data$season == "what?")
#- some entries (not many) have no date 
data <- data[season != "what?"]

data[taxon == "Cambaridae (P. clarkii)", taxon := "Procambarus clarkii"]
data[taxon == "Cambaridae (P. clarkii)", taxon := "Procambarus clarkii"]
data[taxon == "Cambaridae (Procambarus clarkii)", taxon := "Procambarus clarkii"]
data[taxon == "Cambaridae al·lòcton", taxon := "Cambaridae"]
data[taxon == "Coleopter sp1", taxon := "Coleopter"]
data[taxon == "Coleopter sp2", taxon := "Coleopter"]
data[taxon == "Corbiculidae (Corbicula fluminea)", taxon := "Corbicula fluminea"]
data[taxon == "Corbiculidae al·lòcton", taxon := "Corbiculidae"]
data[taxon == "Dípter no identificat", taxon := "Diptera"]
data[taxon %in% c("Hydrobiidae (P. antipodarum)", "Hydrobiidae (Potamopyrgus antipodarum)"), taxon := "Potamopygrus antipodarum"]
data <- data[taxon != "Sphaerotillus"]   

data2 <- data
coords <- st_coordinates(st_as_sf(data2))
data2[, x.coord := coords[,1]]
data2[, y.coord := coords[,2]]
data2[, geometry := NULL]
data2$EPSG <- 4326
data.set.name = "spain_monitoring"
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#updated_type <- readRDS("01_data/02_prepared/01_single_data/spain_monitoring/quick_save_250.rds")
#skip = 250
#source("02_R/91_misc/visual_check.R")
#updated_type[new_type == "Rt11", new_type := "RT11"]
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/spain_monitoring/quick_save_250.rds")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/spain_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/spain_monitoring/updated_types.rds")
source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/spain_monitoring/preped.rds")

