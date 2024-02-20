#----------------------------------- #
#--- Clean data from Sweden AQEM --- # 
#----------------------------------- #

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------
samples    <- read_excel("01_data/01_raw/sweden_aqem/CompleteTaxalist_AQEM_Site_info.xls", skip = 2) 
sites      <- read_excel("01_data/01_raw/sweden_aqem/CompleteTaxalist_AQEM_Site_info.xls", sheet = 2)

# PREPARE DATA  ---------------------------------------------------------------------

setDT(samples)
setDT(sites)

#- rename 
names(samples)[c(1,2)] <- c("taxon", "taxon_supp")

#- Some rows are only summaries of the rows below them, aggregated to a Family
#- level. They need to be removed. In these rows the second column is empty. Some
#- data are only collected at family level though. I want to maintain them but
#- their second column is also empty. 
samples <-
        samples[!(
                str_detect(samples$taxon, "[A-Z]{2,}") |
                        taxon %in% c(
                                "[Kl:Turbellaria]",
                                "Turbellaria",
                                "Nematoda",
                                "[Kl:Nematoda]",
                                "[Kl:Oligochaeta]",
                                "Oligochaeta",
                                "[Ord:Lepidoptera]"
                        )
        )]

samples[taxon_supp %in% c("Gen. sp.", "sp.", "Gen. sp. Lv.", "sp. Lv.", "sp. Ad."), 
        taxon_supp := ""]

samples[, taxon_clean := str_trim(paste(taxon,taxon_supp))]

samples <- samples[,.SD, .SDcols = 7:157]
samples <- samples[-1]

site_names <- colnames(samples)[-151]
samples = data.table::melt.data.table(samples, id.vars = c("taxon_clean"))
samples = samples[value != 0]

sites = data.table(
        site = rep(sites$`sampling site`,2),
        y.coord = as.numeric(rep(sites$`X_RAK (Xnew)`,2)),
        x.coord = as.numeric(rep(sites$`Y_RAK (Xnew)`,2)),
        id = append(sites$`Sample Number Spring`,sites$`Sample Number Autumn`),
        date = append(sites$`1st sampling date`, sites$`2nd sampling date`)
        
)
sites[,date := dmy(date)]
sites[,c("year") := year(date)]
samples$id = as.character(samples$variable)
data <- sites[samples, on = "id"]
data2 <- data[, list(
        original_site_name = site,
        date = ymd(date),
        year,
        taxon = taxon_clean,
        abundance = value,
        x.coord ,
        y.coord,
        EPSG = 2400
)]


data2[, taxon := str_remove_all(taxon, "-Gr\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ Lv\\.$")]
data2[, taxon := str_remove_all(taxon, "\\ Ad\\.$")]

data2[taxon == "Amphinemura standfussi/sulcicollis", taxon := "Amphinemura"]
data2[taxon == "Baetis macani/bundaye", taxon := "Baetis"]
data2[taxon == "Gyraulus acronicus/albus/laevis", taxon := "Gyraulus"]
data2[taxon == "Leuctra fusca/digitata/hippopus", taxon := "Leuctra"]
data2[taxon == "Oulimnius troglodytes/tuberculatus", taxon := "Oulimnius"]
data2[taxon == "Radix peregra/ovata", taxon := "Radix"]
data2[taxon == "Rhyacophila obliterata/nubila", taxon := "Rhyacophila"]
data2[taxon == "Chaetopteryx/Anitella", taxon := "Limnephilidae"]
data2[taxon == "Mystacides longicornis/nigra", taxon := "Mystacides"]

# - check sites on map
# data2 |>
#         unique(by = "original_site_name") |>
#         st_as_sf(coords = c("y.coord", "x.coord"), crs = data2$EPSG[1]) |>
#         mapview()

data.set.name = "aqem_sweden"

source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
# source("02_R/91_misc/visual_check.R")
# saveRDS(updated_type, "01_data/01_raw/sweden_aqem/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/sweden_aqem/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")

# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/sweden_aqem/preped.rds")

