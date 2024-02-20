### ------------------------------------------------ ###
### --- Check MCMC results for chain convergence --- ### 
### ------------------------------------------------ ###

#' Load the markov chains created in the script "05_hSSD_predict.R" and test the chain 
#' convergence, i.e., did the markov chains reach a steady state, or is the mean value 
#' continuously increasing/ decreasing over throughout the samples. 


# setup -----------------------------------------------------------------------------
library(groundhog)
pkgs <- c("data.table", "dplyr", "coda", "purrr", "stringr")
groundhog.library(pkgs, "2024-01-01")

# check convergence -----------------------------------------------------------------

#- Create a vector with the names of the focal chemicals. 
chemicals <- c("Copper", "Imidacloprid")
#- Create an object to store the results of the loops. 
save_list <- list()

#' Start loop. It loops over the three different focal chemicals, stored in the vector 
#' "chemicals". 
for (j in chemicals){
        #' Create a character object with the file name of the file in which the results of 
        #' the markov chain for chemical j are stored.  
        j.file.name <- paste0("data/hssd_predictions/predicted_runs_",j,"_tox_tax.csv")
        #' Load the results of the Markov chain. Convert from data.table to data.frame and
        #' convert to matrix in coda format with the function mcmc from the coda package. 
        #' The Coda package is used here for posterior evaluation. 
        j.posterior <- 
                fread(j.file.name) %>% 
                as.data.frame %>% 
                apply(2, mcmc)
        #' Compute Gewekes statistic for each chain (i.e., species). 
        j.geweke <- j.posterior %>%
                apply(2, geweke.diag) %>%
                purrr::transpose() %>%
                .$z %>% 
                unlist
        #' Compute Heidelberger and Welch's convergence diagnostic 
        j.heidel <- 
                j.posterior %>%
                apply(2, function(x) heidel.diag(x)[[3]])
      
        j.result <- data.table(name = names(j.geweke),
                               geweke = abs(j.geweke),
                               heidel = j.heidel, 
                               stationary.geweke = 1,
                               stationary.heidel = 1,
                               chem = j
        )
        j.result <- j.result[geweke > 3   , stationary.geweke := 0]
        j.result <- j.result[heidel < 0.05, stationary.heidel := 0]
        #' fix names 
        j.result[, name := str_remove(name, "\\.var1")]
        j.result[, name := str_remove(name, "\\.$")]
        
        save_list[[length(save_list) + 1]] <- j.result
        rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
}

#' Aggregate the elements in the list "save_list" to a data.table.  
save_list2 <- rbindlist(save_list)
#' Save aggregated results to file. 
saveRDS(save_list2, "data/hssd_predictions/04_good_final_fit.rds") 
rm(list = ls())
# #- explore results 
# save_list2[, sum(stationary.geweke), by = "chem"]
# save_list2[, sum(stationary.heidel), by = "chem"]
# library(ggplot2)
# ggplot(save_list2, aes(x = chem, y = geweke)) +
#         geom_violin() +
#         geom_hline(yintercept = 3)
