# —————————————————————— #
# ——— Fit hSSD models——— # 
# —————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com) & Thomas Sinclair
# 13.02.2024

# setup -----------------------------------------------------------------------------
library(groundhog)
pkgs <- c("Matrix", "tidyverse", "data.table")
groundhog.library(pkgs,'2024-01-01')
# In the current version groundhog does not support bioconductor packages. 
# The we provide the Source Packages for the respective version in our github. 
# They can be installed with the following code: 
#install.packages("r_packages/BiocGenerics_0.48.1.tar.gz", repos = NULL)
#install.packages("r_packages/S4Vectors_0.40.2.tar.gz", repos = NULL)
#install.packages("r_packages/rhdf5_2.46.1.tar.gz", repos = NULL)
library(rhdf5)
rm(pkgs)

# load data -------------------------------------------------------------------------
# - load in the chemicals
chems <- fread("data/names.csv")

# - Prime R with the hSSD function
source("code/functions/hSSD_function_2-1.R")

# - read in the assemblage for toxicity to be predicted
newspecies <- readRDS("data/prepared/02_unique_taxa.rds")
#- keeping newspecies as a data.table seems to provoke an error 
newspecies <- as.data.frame(newspecies)

# - subset chems to compounds considered in the final version
chems <- chems[c(2,3),]

# - set parameters for Markov chains 
n.samples     <- c(30000, 30000)
n.burn.in     <- c(10000, 10000)
n.thin        <- c(15,15)

for(i in seq_along(chems)){
        
        # - read in the sensitivity data
        i.newdata <- read.csv(paste0("data/sensitivity/", chems[[1]][i], "_tox.csv"))
        # - not needed? Remove 
        #newdata$latin <- tolower(newdata$latin)
        
        # - read in the taxonomy of the tox data
        i.newspecies.testedonly <- read.csv(paste0("data/sensitivity/",chems[[1]][i], "_tox_tax.csv"))
        
        i.result <- MCMC.newchemical(
                i.newdata,
                h5file="data/toms-model122b.h5",
                numeric(0),
                newspecies=newspecies,
                newspecies.testedonly=i.newspecies.testedonly,
                N=n.samples[i], burn=n.burn.in[i], thin=n.thin[i], shout=100,
                detailed.output=TRUE
        )
        
        
        #have a look at the predicted outputs from the runs
        #result$mu0
        
        fwrite(data.frame(i.result$mu0), paste0("data/hssd_predictions/predicted_runs_", chems[[1]][i],"_tox_tax.csv"))
        
        # compute posterior mean for each taxon.
        # Because values are logged, this is the geometric mean.
        i.pred.tox <- data.frame(colnames(i.result$mu0),apply(i.result$mu0, 2, mean))
        
        # - rename columns
        colnames(i.pred.tox) <- c("latin","tox")
        
        # save the files
        fwrite(i.pred.tox, paste0("data/hssd_predictions/predicted_tox_",chems[[1]][i],".csv"))
        rm(list = ls()[grepl("^i\\.", x = ls())])
}
rm(list = ls())
