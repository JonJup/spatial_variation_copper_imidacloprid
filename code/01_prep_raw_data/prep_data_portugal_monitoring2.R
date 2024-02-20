# ———————————————————————————————————————————— #
# ——— Clean data: Portugal — Monitoring  2 ——— # 
# ———————————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------

data <- read_excel("01_data/01_raw/protugal_monitoring2/Macroinvertebrados_2019 .xlsx", sheet = 3)

# PREPARE DATA   ---------------------------------------------------------------------

site_name <- names(data)[-c(1:2)]
original_site_names <- 
        data[3,-c(1:2)] |> 
        unlist() |> 
        unname() 
x.coord <- 
        data[1,-c(1:2)] |> 
        unlist() |> 
        unname() |> 
        as.numeric()
y.coord <- 
        data[2,-c(1:2)] |> 
        unlist() |> 
        unname() |> 
        as.numeric()

data2 <- data.table(original_site_names, site_name, x.coord, y.coord, date = ymd("2019/05/30"), year = 2019, EPSG = 4326)
sites <- st_as_sf(data2, coords=c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)

taxa <- data[-c(1:3), -1]
taxa %<>% 
        rename(taxon = "...2") %>%
        pivot_longer(cols = !taxon, names_to = "site_name", values_to = "abundance") %>%
        filter(abundance != 0)

data <- left_join(taxa, 
                  data2, 
                  by = "site_name")

data <- data.table(original_site_name = data$original_site_names,
                       date = data$date, 
                       taxon = data$taxon,
                       abundance = data$abundance,
                       x.coord = data$x.coord,
                       y.coord = data$y.coord, 
                       EPSG =4326,
                   year = 2019
                      )

data <- data[taxon != "SOMA"]
data[, taxon := str_remove(taxon, "\\(Ad\\)")]
data[, taxon := str_remove(taxon, "\\(Lv\\)")]
data[, taxon := str_trim(taxon)]
data2 <- data
data.set.name = "portugal_monitoring2"
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
# source("02_R/91_misc/visual_check.R")
# saveRDS(updated_type, "01_data/02_prepared/01_single_data/portugal_monitoring2/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/portugal_monitoring2/updated_types.rds")
source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/portugal_monitoring2/preped.rds")
