# —————————————————— #
# ——— ——— # 
# —————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# 13.02.2024

#' In this script we determine the Species Sensitivity Distributions for the observed 
#' assemblages. In this procedure all communities with less than 8 taxa with predicted 
#' EC50 values are dropped. Additionally, we evaluate the fit of the lognormal distribution
#' to the observed EC50 values with a Kolmogorov Smirnov Test.

# setup -----------------------------------------------------------------------------
library(groundhog)
pkgs <- c("data.table", "dplyr", "magrittr", "MASS", "stringr", "nortest")
groundhog.library(pkgs,'2024-01-01')

# load data -------------------------------------------------------------------------
good.list.fit <- readRDS("data/hssd_predictions/04_good_final_fit.rds")
files         <- list.files(path = "data/hssd_predictions/", pattern = "predicted_tox_")
samples       <- list.files(path = "data/prepared/sample_data//")

# prepare data ----------------------------------------------------------------------
chems <- c("Cu", "IMD")

#' Start a loop over the focal chemicals. 
for(i in 1:length(chems)){
        
        #'select results from iterative fitting for this chemical
        #'and adjust taxonomy 
        i.good.list <- 
                good.list.fit[
                        chem == unique(good.list.fit$chem)[i] &
                        stationary.heidel == 1
                        ]$name
        # i.good.list <- dplyr::intersect(i.good.list.fit, good.list[[i]])
        
        #i.good.list <- i.good.list.fit
        
        i.good.list %<>%
                str_replace_all(pattern = "\\.", replacement = "\\ ") %>% 
                str_replace_all(pattern = "\\ \\ ", replacement = "\\ ") %>% 
                str_replace_all(pattern = "unknown\\ ", replacement = "unknown\\ \\(") %>% 
                str_replace_all(pattern = "genus$", replacement = "genus\\)") %>% 
                str_replace_all(pattern = "family$", replacement = "family\\)")
        
        # - load sensitivity predictions for chemical i
        i.pred.tox <- fread(paste0("data/hssd_predictions/",files[i]))
        # - create empty data table to store data
        i.summary <- list()
        
        # - loop over samples
        for(j in 1:length(samples)){
                #for(j in 1:10){
                
                # - load data for sample j
                j.newspecies <- readRDS(paste0("data/prepared/sample_data/",samples[j]))
                # - how many species are in this community and how many can we predict?
                j.availalble <- sum(j.newspecies$Latin %in% i.good.list)
                j.total      <- nrow(j.newspecies)
                # - drop taxa for which the model did not converge in at least four out of five iterative fittings
                j.newspecies %<>% filter(Latin %in% i.good.list)
                # - filter further to those species that also converged in the focal run 
                # - skip sample if it contains less than eight taxa
                if(nrow(j.newspecies) < 8){
                        #print(samples[j])
                        next
                }
                j.sample.tox     <- i.pred.tox[latin %in% tolower(j.newspecies$Latin),]
                j.sample.tox     <- j.sample.tox[order(j.sample.tox$latin),]
                j.sample.tox[, tox := 10^tox]
                #- fit lognormal distribution to EC50 values 
                j.fit        <- fitdistr(j.sample.tox$tox, 'lognormal')
                #- evaluate fit with Kolmogorov Smirnov Test
                j.ks         <- ks.test(x       = j.sample.tox$tox, 
                                        y       = "plnorm", 
                                        meanlog = j.fit$estimate[1], 
                                        sdlog   = j.fit$estimate[2])  
                #- compute HC5, 25, 50, 75
                j.hc5  <- qlnorm(0.05, meanlog = j.fit$estimate[1], sdlog = j.fit$estimate[2])
                j.hc10 <- qlnorm(0.10, meanlog = j.fit$estimate[1], sdlog = j.fit$estimate[2])
                j.hc25 <- qlnorm(0.25, meanlog = j.fit$estimate[1], sdlog = j.fit$estimate[2])
                j.hc50 <- qlnorm(0.50, meanlog = j.fit$estimate[1], sdlog = j.fit$estimate[2])
                j.hc75 <- qlnorm(0.75, meanlog = j.fit$estimate[1], sdlog = j.fit$estimate[2])
                #set.seed(123)
                #hc5_boot <- replicate(1000, myboot(i.fit, p = 0.05))
                i.summary[[length(i.summary) + 1]] <- 
                        data.table(
                                sample = gsub(".rds","",gsub("sample ","",samples[j])), 
                                HC5  = j.hc5,
                                HC10 = j.hc10,
                                HC25 = j.hc25,
                                HC50 = j.hc50,
                                HC75 = j.hc75,
                                ks_p = j.ks$p.value,
                                availalable_taxa = j.availalble,
                                total_taxa       = j.total
                        )
                print(length(samples)-j)
                rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
        } ## END OF j loop over samples 
        
        i.summary <- rbindlist(i.summary)
        i.summary[, frac_available := availalable_taxa/total_taxa * 100]
        saveRDS(
                i.summary,
                paste0(
                        "data/hssd_predictions/assemblage_hSSD_",
                        chems[i],
                        ".rds"
                )
        )
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
} ## END of i loop over chemicals
rm(list=ls())

