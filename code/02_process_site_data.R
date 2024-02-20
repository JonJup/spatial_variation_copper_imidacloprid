# ————————————————————————— #
# ——— Process site data ——— # 
# ————————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com) & Thomas Sinclair
# 12.02.2024

#' This script harmonizes the taxonomy between the hSSD model and the observational data.
#' It creates multiple files: 
#'      sample_properties.rds     —— information for each sampling site
#'      unique_taxa.rds           —— A table with the taxonomy of all unique taxa occurring 
#'                                   in the observational data
#'      all files in sample data/ —— subset of unique taxa occurring in the respective site
#'      
#'      
# setup -----------------------------------------------------------------------------
library(groundhog)
pkgs <- c("conflicted",
          "dplyr",
          "data.table",
          "fs",
          "magrittr",
          "stringr")
groundhog.library(pkgs,'2024-01-01')
rm(pkgs)
# load data -------------------------------------------------------------------------
sites <- readRDS("data/prepared/01_combined_data.rds")

# prepare data ----------------------------------------------------------------------

# - before doing anything with taxonomy, get a list of samples and their properties
sample_props <-
        unique(sites, by = "sample_id") %>%
        select(sample_id, site_id, date_id, X, Y, brt12, richness, enz, bgr, ife, data.set, year, least.impacted, richness)

# - rearrange the data by date
setorderv(sample_props, "date_id")
saveRDS(sample_props, "data/prepared/02_sample_properties.rds")
rm(sample_props)

# - Make the taxonomy the same as the in the hSSD model.
        # - Convert taxonomic columns to lower case strings. 
        # - Here, I choose a somewhat more cumbersome way, specifiying every column explicitly. This is 
        # - more robust towards changes in column order.
sites[, c("species",
          "genus",
          "family",
          "order",
          "class",
          "phylum",
          "kingdom") :=
              .(
                      tolower(species),
                      tolower(genus),
                      tolower(family),
                      tolower(order),
                      tolower(class),
                      tolower(phylum),
                      tolower(kingdom)
              )
      ]

# - Sort out issues manually
sites[class == "gastropoda"   & is.na(order), order := paste("unknown", family)]
sites[order == "tricladida"   & is.na(class), class := "turbellaria"]
sites[order == "prorhynchida" & is.na(class) &  !is.na(order), class := "prorhynchida"]

# - fill in missing lower taxonomy with the higher taxonomic classification
sites <- data.frame(sites)
loop1 <- which(names(sites) == "species")
loop2 <- which(names(sites) == "kingdom")

for(i in loop2:loop1){
        
        if (sum(is.na(sites[,i])) == 0) next()
        sites[is.na(sites[,i]),loop1:i] <-
                paste0("unknown (",   sites[is.na(sites[,i]) ,i+1], " " , colnames(sites)[i+1],  ")")
}
rm(i)
setDT(sites)

# - drop observations at the phylum level 
sites <- sites[!species %in% c("unknown (arthropoda phylum)", "unknown (mollusca phylum)")]

# - make a list of unique taxa from the taxonomic data
unique_taxa <- sites[,loop1:loop2]

# - remove duplicates present at multiple sites
unique_taxa <- unique_taxa[!duplicated(unique_taxa),]
unique_taxa %<>% data.frame
# - reorder and rename to be the same as the hSSD model
hSSD_taxa <- data.frame(Kingdom         = "animalia", 
                        Phylum_division = unique_taxa[,6],
                        Subphylum       = NA, 
                        Superclass      = NA, 
                        Class           = unique_taxa[,5], 
                        Order           = unique_taxa[,4], 
                        Family          = unique_taxa[,3], 
                        Genus           = unique_taxa[,2], 
                        Latin           = unique_taxa[,1])
class(hSSD_taxa$Subphylum)  <- "character"
class(hSSD_taxa$Superclass) <- "character"
setDT(hSSD_taxa)
rm(unique_taxa, loop1, loop2)

source("code/02_1_taxonomic_fixes.R")

saveRDS(hSSD_taxa, "data/prepared/02_unique_taxa.rds")

#- reformat the sample data into hSSD format
taxa <- data.table(sample  = sites$sample_id,
                   Kingdom = "animalia",
                   Phylum_division = sites$phylum,
                   Subphylum  = NA,
                   Superclass = NA,
                   Class      = sites$class,
                   Order      = sites$order,
                   Family     = sites$family,
                   Genus      = sites$genus,
                   Latin      = sites$species
)
class(taxa$Subphylum)  <- "character"
class(taxa$Superclass) <- "character"

source("code/02_2_taxonomic_fixes.R")

taxa <- taxa[!is.na(taxa$sample),]

# - make a list of all the sites
sample_ids <- unique(taxa$sample)

# - The following for loop will save one file per sample.
# - Before I run the loop, I will make sure that the folder in which the files are saved is empty.
# - If previous files exist, I remove them.
if (length(dir_ls("data/prepared/sample_data/")) != 0){
        file_delete(dir_ls("data/prepared/sample_data/"))
}

for(i in seq_along(sample_ids)){
        
        # - select focal sample and remove duplicate species (or Latin names)
        i.sample_taxa <- 
                taxa[sample == sample_ids[i],] %>% 
                unique(by = "Latin")
        # - There is still a problem with nested taxonomic ranks,
        # - if there are multiple taxa with the same genus
        if(any(duplicated(i.sample_taxa$Genus))) {
                
                # - extract duplicates
                i.duplicates <- 
                        unique(i.sample_taxa[duplicated(i.sample_taxa$Genus), Genus])
                # - What species belong to genera with multiple entries?
                i.duplicates <-
                        i.sample_taxa[Genus %in% i.duplicates, Latin]
                #-  Remove generic ones (i.e. hydranea sp.)
                if (any(str_detect(i.duplicates, "\\ genus"))){
                        i.duplicates  <- i.duplicates[which(str_detect(i.duplicates, "\\ genus"))]
                        i.sample_taxa <- i.sample_taxa[!Latin %in% i.duplicates]
                }
        }
        # - Repeat the above for taxa with the same family.
        if(any(duplicated(i.sample_taxa$Family))) {
                
                # - extract families with multiple entries
                i.duplicates <- 
                        unique(i.sample_taxa[duplicated(i.sample_taxa$Family), Family])
                # - What species belong to genera with multiple entries?
                i.duplicates <-
                        i.sample_taxa[Family %in% i.duplicates, Latin]
                #-  Remove generic ones (i.e. hydranea sp.)
                if (any(str_detect(i.duplicates, "\\ family"))){
                        i.duplicates  <- i.duplicates[which(str_detect(i.duplicates, "\\ family"))]
                        i.sample_taxa <- i.sample_taxa[!Latin %in% i.duplicates]
                }
        }
        #if (nrow(i.sample_taxa) < 5) print(i)
        saveRDS(i.sample_taxa[,2:length(i.sample_taxa)], paste0("data/prepared/sample_data/", sample_ids[i], "_tax.rds"))
        rm(list = ls()[grepl("^i\\.", x = ls())])
}
rm(list = ls())

