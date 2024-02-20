# ————————————————————————— #
# ——— Identify Outliers ——— # 
# ————————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com) & Thomas Sinclair
# 13.02.2024

# setup -----------------------------------------------------------------------------
library(groundhog)
pkgs <- c("data.table", "HDInterval", "ggplot2", "tidyr", "dplyr", "magrittr")
groundhog.library(pkgs,'2024-01-01')
rm(pkgs)

# load data -------------------------------------------------------------------------
hc5 <- readRDS("data/hssd_predictions/06_hSSD_predictions_all_chem.rds")

# prepare data ----------------------------------------------------------------------

hc5[chem == "copper", chem := "Cu"]
hc5[chem == "imidacloprid", chem := "IMD"]

# - create variable to hold outlier status 
# - final values will be 0 (no outlier) and 1 (outlier). 
# - three is chosen as starting value to identify if procedure failed for any samples
hc5[, outlier_iqr := 3]
hc5[, outlier_hdi := 3]
hc5[, outlier_sdi := 3]
# - split chemicals 
c <- hc5[chem == "Cu"]
i <- hc5[chem == "IMD"]

# Interquartile range ---------------------------------------------------------------
# - if the HC5 is within 1.5 * inter quartile distance of the first or third quartile, 
# - the observation is not an outlier.
c[HC5 > (quantile(HC5, 0.25) - IQR(HC5)) & HC5 < (quantile(HC5, 0.75) + IQR(HC5)), outlier_iqr := 0]
i[HC5 > (quantile(HC5, 0.25) - IQR(HC5)) & HC5 < (quantile(HC5, 0.75) + IQR(HC5)), outlier_iqr := 0]

# - if the HC5 is not within 1.5 * inter quartile distance of the first or third quartile, 
# - the observation is an outlier.
c[HC5 <= (quantile(HC5, 0.25) - IQR(HC5)) | HC5 >= (quantile(HC5, 0.75) + IQR(HC5)), outlier_iqr := 1]
i[HC5 <= (quantile(HC5, 0.25) - IQR(HC5)) | HC5 >= (quantile(HC5, 0.75) + IQR(HC5)), outlier_iqr := 1]

# Density intervals ---------------------------------------------------------------------------
# - compute highest density intervals
hdi.c <- hdi(c$HC5)
hdi.i <- hdi(i$HC5)

# - assign outlier status based on hdi 
c[HC5  > hdi.c[1] & HC5  < hdi.c[2], outlier_hdi := 0]
c[HC5 <= hdi.c[1] | HC5 >= hdi.c[2], outlier_hdi := 1]
i[HC5  > hdi.i[1] & HC5  < hdi.i[2], outlier_hdi := 0]
i[HC5 <= hdi.i[1] | HC5 >= hdi.i[2], outlier_hdi := 1]


# - symmetrical density interval 
sdi.c <- quantile(c$HC5, c(0.025,0.975))
sdi.i <- quantile(i$HC5, c(0.025,0.975))

# - assign outlier status based on sdi 
c[HC5  > sdi.c[1] & HC5  < sdi.c[2], outlier_sdi := 0]
c[HC5 <= sdi.c[1] | HC5 >= sdi.c[2], outlier_sdi := 1]
i[HC5  > sdi.i[1] & HC5  < sdi.i[2], outlier_sdi := 0]
i[HC5 <= sdi.i[1] | HC5 >= sdi.i[2], outlier_sdi := 1]

# combine data  ---------------------------------------------------------------------
hc5 <- rbindlist(list(c,i))
# richness outliers -----------------------------------------------------------------
hid.obj <- hdi(hc5$richness, credMass = 0.9)

hc5[, richness_outlier := 3]
hc5[richness <= hid.obj[1], richness_outlier := 1]
hc5[richness >= hid.obj[2], richness_outlier := 1]
hc5[richness >= hid.obj[1] & richness <= hid.obj[2], richness_outlier := 0]
unique(hc5$richness_outlier)

# drop rare types  ------------------------------------------------------------------
# - how many observations are left per river types? 
n.per.type <- hc5[least.impacted == TRUE & frac_available >= 75 & ks_p > 0.05, .N, by = c("brt12", "chem")]
# - identify combinations of river types and chemicals for which fewer than 20 samples are left. 
n.per.type <- n.per.type[N < 20]
if (nrow(n.per.type) > 1){
        for (i in 1:nrow(n.per.type)){
                hc5 <- hc5[brt12 != n.per.type$brt12[i] | chem != n.per.type$chem[i]]
        }  
}

# save to file ----------------------------------------------------------------------
saveRDS(hc5, "data/hssd_predictions/07_hSSD_predictions_all_chem_outlier.rds")
rm(list = ls())


