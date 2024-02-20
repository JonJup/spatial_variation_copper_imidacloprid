# ——————————————————————————————————————— #
# ——— Clean data: Poland — MONITORING ——— # 
# ——————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------
source("02_R/91_misc/01_setup_prep_data.R")

#- have to source directly from the script. Open with F2
source("02_R/01_prep_raw_data/collect_data_poland.R")
#duplicate_sites <- readRDS("01_data/02_prepared/01_single_data/poland_monitoring/duplicate_sites.rds")
data.set.name = "poland_monitoring"
data2$abundance <- 1
source("02_R/91_misc/02_prep_step_1.R")
# - visually check that sites belong to river types
# source("02_R/91_misc/visual_check.R")
# saveRDS(updated_type, "01_data/02_prepared/01_single_data/poland_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/poland_monitoring/updated_types.rds")
source("02_R/91_misc/03_prep_data_step2.R")

data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/poland_monitoring/preped.rds")


