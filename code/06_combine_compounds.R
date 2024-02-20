# ————————————————————————————————————— #
# ——— Combine hSSD predictions      ——— # 
# ————————————————————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# 13.02.2024

#' In this script we combine the HC5 predictions for different chemicals. 

# setup -----------------------------------------------------------------------------
library(groundhog)
pkgs <- c("data.table", "dplyr", "stringr")
groundhog.library(pkgs,'2024-01-01')
rm(pkgs)

# load data -------------------------------------------------------------------------
chems  <- c("Copper", "Imidacloprid")
copper <- readRDS("data/hssd_predictions/assemblage_hSSD_Cu.rds")
imidac <- readRDS("data/hssd_predictions/assemblage_hSSD_IMD.rds")
proper <- readRDS("data/prepared/02_sample_properties.rds")

# prepare data ----------------------------------------------------------------------

copper[, chem := "copper"]
imidac[, chem := "imidacloprid"]
sum.tox <- rbindlist(list(copper, imidac))
sum.tox[, sample := str_trim(gsub("_tax","", sample))]
sum.tox2 <- left_join(x = sum.tox,
                      y = proper,
                      by = c("sample" = "sample_id"))

# save to file ----------------------------------------------------------------------
saveRDS(sum.tox2 ,"data/hssd_predictions/06_hSSD_predictions_all_chem.rds")
rm(list = ls())

