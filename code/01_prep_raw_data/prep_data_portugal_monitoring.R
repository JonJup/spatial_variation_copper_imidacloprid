# ————————————————————————————————————————— #
# ——— Clean data: Portugal — Monitoring ——— # 
# ————————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")
# LOAD DATA  ------------------------------------------------------------------------

source("02_R/01_prep_raw_data/collect_data_portugal.R")
data.set.name = "portugal_monitoring"
data2 <- data
data2 <- filter(data2, !taxon %in% c("IPtIs_Final", "N/A", "NEMATHELMINTHA"))

setDT(data2)

data2[str_detect(taxon, "\\[Kl:"), taxon := str_remove(taxon, "\\[Kl:")]
data2[str_detect(taxon, "\\]"), taxon := str_remove(taxon, "\\]")]
data2[str_detect(taxon, "\\ \\(Ad\\.\\)"), taxon := str_remove(taxon, "\\ \\(Ad\\.\\)")]
data2[str_detect(taxon, "\\ \\(Lv\\.\\)"), taxon := str_remove(taxon, "\\ \\(Lv\\.\\)")]
data2[str_detect(taxon, "\\.\\.\\..*"), taxon := str_remove(taxon, "\\.\\.\\..*")]
data2[, taxon := str_to_title(taxon)]
data2[, taxon := str_trim(taxon)]
data2[taxon == "Limaoniidae", taxon := "Limoniidae"]
data2[taxon == "Hidracarina", taxon := "Hydrachnidae"]
data2[taxon == "Limanephilidae", taxon := "Limnephilidae"]
data2[taxon == "Polycentropididae", taxon := "Polycentropodidae"]

source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/portugal_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/portugal_monitoring/updated_types.rds")
source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/portugal_monitoring/preped.rds")
