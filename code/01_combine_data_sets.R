# —————————————————————————————————————— #
# ——— Combine invertebrate data sets ——— # 
# —————————————————————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# 12.02.2024

# setup -----------------------------------------------------------------------------
library(groundhog)
pkgs <- c("conflicted",
          "data.table",
          "dplyr",
          "fs",
          "magrittr",
          "stringr",
          "sf",
          "HDInterval")
groundhog.library(pkgs,'2024-01-01')
conflicts_prefer(dplyr::filter)

# load data -------------------------------------------------------------------------
#- compile a list of all data sets 
data.sets <- dir_ls("data/prepared/single_data_sets/", type = "directory")
data <- list()
for (i in seq_along(data.sets)){
        i.ds <- data.sets[i]
        print(paste("LOADING", str_remove(i.ds, "01_data/02_prepared/01_single_data/")))
        i.files <- dir_ls(i.ds, regexp = "preped.rds")
        i.x     <- readRDS(i.files)
        setDT(i.x)
        data[[i]] <- i.x 
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
}

# prepare data ----------------------------------------------------------------------
#- Make sure all date variables are formatted as such:
data2   <- lapply(data, function(x) x[, date := as.Date(date)])
#- Make data spatial (sf) and transform to common coordinate reference system (LAEA Europe). 
data.st <- lapply(data2, function(x) st_as_sf(x, coords = c("x.coord", "y.coord"), crs = x$EPSG[1]))
data.st %<>% lapply(function(x) st_transform(x, crs = 3035))
coords  <- lapply(data.st, st_coordinates)  
data2   <- lapply(data.st, setDT)%>%rbindlist
coords  %<>% lapply(as.data.frame)%>%lapply(setDT)%>%rbindlist
data2   %<>% bind_cols(coords)
# - drop geometry column from the sf package and add a presence variable that is one for all rows. 
# - This variable will be necessary when we pivot the data wider. 
data2[, geometry := NULL]
data2$presence = 1
data2[brt12 == "Rt3", brt12 := "RT3"]
# - which phyla occur in all data sets 
data2 <- data2[phylum %in% c("Annelida", "Mollusca", "Arthropoda")]
data2 <- data2[class %in% c("Clitellata","Insecta","Malacostraca","Bivalvia","Gastropoda")]
data2 <- data2[!data.set %in% c("czech_monitoring", "finaland_koutajoki")]
data2[, richness := uniqueN(lowest.taxon), by = "sample_id"]
data2[data.set == "norway_monitoring", least.impacted := TRUE]
data2[is.na(least.impacted), least.impacted := FALSE]
#- drop old samples 
data2 <- data2[year >= 2005]

# save to file ----------------------------------------------------------------------
save.name <- "combined_data"
saveRDS(data2, paste0("data/prepared/01_", save.name, ".rds"))
rm(list = ls())








