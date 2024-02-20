# —————————————————— #
# ——— ——— # 
# —————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# DATE

# setup -----------------------------------------------------------------------------
library(groundhog)
pkgs <- c("data.table", "dplyr", "magrittr", "tidyr")
groundhog.library(pkgs,'2024-01-01')
rm(pkgs)
# load data -------------------------------------------------------------------------
hc5 <- readRDS("data/hssd_predictions/07_hSSD_predictions_all_chem_outlier.rds")

# prepare data ----------------------------------------------------------------------
hc5 <- hc5[ks_p > 0.05 & frac_available >= 33]
hc5 <- hc5[outlier_hdi == 0 & least.impacted == 1]
# - compute median for each river type, chemical
hc5[, med_HC5 := median(HC5), by = c("brt12", "chem")]
hc5[, brt12 := factor(brt12, levels = paste0("RT", 1:12))]


# - create data set with differences 
cross_table <- crossing(type1 = droplevels(hc5$brt12), type2 = droplevels(hc5$brt12))
setDT(cross_table)
cross_table <- cross_table[type1 != type2]
cross_table[, c("Cu", "IMD") := 0]
uni.types <- unique(hc5$brt12)
n.types   <- uniqueN(hc5$brt12)

for (i in 1:n.types) {
        for(j in 1:n.types) {
                for(k in 1:2) {
                        
                        # - skip if type1 == type2
                        if (i == j)
                                next()
                        # - select chemical compound
                        k.chem <- c("Cu", "IMD")[k]
                        
                        # - check if one of the types is rare
                        # - skip if one of them is
                        # if (nrow(rare[ife == uni.types[i] & chem == k.chem]) == 1 |
                        #     nrow(rare[ife == uni.types[j] & chem == k.chem]) == 1) {
                        #         next()
                        # }
                        # -
                        i.diff1 <- hc5[brt12 == uni.types[i] & chem == k.chem]
                        i.diff2 <- hc5[brt12 == uni.types[j] & chem == k.chem]
                        i.diff3 <- c(i.diff1$med_HC5, i.diff2$med_HC5)
                        id1 <- which.max(i.diff3)
                        id2 <- which.min(i.diff3)
                        
                        i.diff3 <- i.diff3[id1] / i.diff3[id2]
                        cross_table[type1 == uni.types[i] & type2 == uni.types[j], (k.chem) := i.diff3]
                }
        }
}

cross_table %<>% 
        pivot_longer(col = Cu:IMD, names_to = "chem", values_to = "quotient") 

# - drop all rows with zeros in quotient. 
# - Zeros only occur when this combination was skipped, because one was rare. 
cross_table %<>% filter(quotient != 0)


# save to file ----------------------------------------------------------------------
saveRDS(cross_table, "data/results/09_differences_in_median_HC5_li_sites.rds")
# clean environment -----------------------------------------------------------------
rm(list = ls())





