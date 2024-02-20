# ————————————————————————————————————————————————— #
# ——— Clean data: France — MONITORING - NAïADES ——— # 
# ————————————————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------
source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------
fauna_table <- fread("01_data/01_raw/france_naiades/fauneflore.csv")
geo_table   <- read.csv("01_data/01_raw/france_naiades//stations.csv", sep = ";")
setDT(geo_table)

# PREPARE DATA  ---------------------------------------------------------------------

# - We subset fauna_table to macroinvertebrate observations. 
fauna_table <- fauna_table[LbSupport == unique(fauna_table$LbSupport)[3] & MnTypTaxRep == "NbrTax"]
# - join macroinvertebrate data with station data in geo_table. The latter contains station coordiantes.  
fauna_table <- geo_table[fauna_table, on = "CdStationMesureEauxSurface"]
data        <- fauna_table[,
             list(
                     "original_site_name" = CdStationMesureEauxSurface,
                     "date"               = ymd(DateDebutOperationPrelBio),
                     "taxon"              = NomLatinAppelTaxon,
                     "x.coord"            = CoordXStationMesureEauxSurface,
                     "y.coord"            = CoordYStationMesureEauxSurface,
                     "EPSG"               = LibelleProjection,
                     "abundance"          = RsTaxRep
             )
]

data[,c("year") := .(year(date))]

#- The data set contains data in four different coordinate reference systems. 
#- Most data are RGF93 / Lambert 93 (EPSG: 2154). These are the only data we will keep. 
#- The remaining data are in French Guyana (CRS: RGFG95 / UTM 22; EPSG: 2972), 
#- Fort-de-France (a Caribbean island; CRS: RRAF 91 (WGS84) / UTM 20; EPSG: 2989) or are lacking 
#- spatial coordinates and hence also a reference system. 

data <- data[EPSG == "RGF93 / Lambert 93"]
data[, c("EPSG", "data.set") := .(2154, "naiades")]
data[, EPSG := as.numeric(EPSG)]
data2 <- data

# - Fix some taxonomic issues

data2[taxon == "Triaenodes/Ylodes", taxon := "Leptoceridae"]
data2[taxon == "Chaetopteryx villosa/fusca", taxon := "Chaetopteryx"]

# - some taxa seem to have received a wrong tag. These are not invertebrates 
data2 <- data2[taxon != "Appellation de Taxon inconnue"]
data2 <- data2[taxon != "Marsilea quadrifolia"]
data2 <- data2[taxon != "Elodes palustris"]
data2 <- data2[taxon != "Merluccius merluccius"]
data2 <- data2[taxon != "Code gelé 1999 (nematomorphes)"]
data2 <- data2[taxon != "Hantzschia amphioxys var. vivax"]
data2 <- data2[taxon != "Nemathelmintha"]
data2 <- data2[taxon != "Cymbella excisa"]
data2 <- data2[taxon != "Cherax"]
data2 <- data2[taxon != "Himantidium"]
data2 <- data2[taxon != "Hygrophila"]
data2 <- data2[taxon != "Krsticiella"]
data2 <- data2[taxon != "Lampriscus"]
data2 <- data2[taxon != "Labellicula"]
data2 <- data2[taxon != "Libelluloidea"]
data2 <- data2[taxon != "Muscoidea"]
data2 <- data2[taxon != "Pleocyemata"]
data2 <- data2[taxon != "Ptychopteroidea"]
data2 <- data2[taxon != "Trachelomonas subverrucosa"]

# - check sites on map 
# data2 |>
#         unique(by = "original_site_name") |>
#         st_as_sf(coords = c("x.coord", "y.coord"), crs = 2154) |>
#         mapview()
data.set.name = "france_naiades"
source("02_R/91_misc/02_prep_step_1.R")
#saveRDS(data2, "01_data/02_prepared/01_single_data/france_naiades/after_step1.rds")
#data2 <- readRDS("01_data/02_prepared/01_single_data/france_naiades/after_step1.rds")
# - visually check that sites belong to river types
#updated_type <- readRDS("01_data/02_prepared/01_single_data/france_naiades/quicksave_2490.rds")
#skip = 2848 
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/france_naiades/quicksave_2490.rds")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/france_naiades/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/france_naiades/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/france_naiades/preped.rds")




