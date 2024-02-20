### ------------------------------- ###
### --- Analysis: Cliff's delta --- ###
### ------------------------------- ###


# setup -----------------------------------------------------------------------------
library(groundhog)
pkgs <- c("dplyr", "magrittr", "data.table", "effsize", "tidyr")
groundhog.library(pkgs, "2024-01-01")
source("code/functions/cliffs_detla.R")
# load data -------------------------------------------------------------------------
data <- readRDS("data/hssd_predictions/07_hSSD_predictions_all_chem_outlier.rds")

# prepare data ----------------------------------------------------------------------
# - turn original broad river types into a factor. 
data[, brt12 := factor(brt12, levels = paste0("RT", 1:12))]

data <- data[ks_p >= 0.05 & frac_available >= 33]

data[, .N, by = "chem"]
# - ID outliers
prep1 <- function(x){
        x <- data[chem == x]
        y1 <- x
        y2 <- x[outlier_hdi == 0]
        y3 <- x[least.impacted == 1]
        y4 <- x[outlier_hdi == 0 & least.impacted == 1]
        y <- list(y1,y2,y3,y4)
        return(y)
}


cop <- prep1("Cu")
imd <- prep1("IMD")


# - compute cliff's detla with a custom wrapper around effsize::cliff.delta
cop.cliff <- lapply(X = cop, FUN = cliffs_d_custom) %>% rbindlist
imd.cliff <- lapply(X = imd, FUN = cliffs_d_custom) %>% rbindlist
all <- rbindlist(
        list(
                #azt.cliff,
                cop.cliff, 
                #lch.cliff,
                imd.cliff
        )
)
# save to file ----------------------------------------------------------------------
saveRDS(all, "data/results/08_results_cliffs_delta.rds")

# clear environment -----------------------------------------------------------------
rm(list = ls())

