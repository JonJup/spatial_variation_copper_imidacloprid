## -- master script raw data -- ## 

library(fs)
scripts <- dir_ls("02_R/01_prep_raw_data/")
skipper <- c("02_R/01_prep_raw_data/00_master_script_raw_data.R", 
             "02_R/01_prep_raw_data/collect_data_poland.R", 
             "02_R/01_prep_raw_data/collect_data_portugal.R")
for (i in scripts){
        if (i %in% skipper) next(brea)
        print(i)
        source(i)
} 
