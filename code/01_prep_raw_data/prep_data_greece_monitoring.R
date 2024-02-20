# ——————————————————————————————————————— #
# ——— Clean data: GREECE — MONITORING ——— # 
# ——————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------
source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------

data       <- read_excel("01_data/01_raw/greece_monitoring/Database_Macroinvertebrate_GR_08.09.2021.xlsx") 

# PREPARE DATA   ---------------------------------------------------------------------

#- transform to data.table 
setDT(data)

#- the first two columns can be discarded 
data <- data[, -c(1,2)]

#- the first two rows can be discarded 
data <- data[-c(1,2),]

#- there are two site columns ('Station' and 'Site Name'). For most entries they are the same. 
data[`Site Name` != Station, c("Site Name", "Station")]
uniqueN(data$`Site Name`) - uniqueN(data$Station)
#- Site Name has 38 more unique entries so I stick to this ID. 

#- season and year 
data[, season := str_to_lower(str_remove(str_extract(Season, ".*_"), "_"))]
data[, year  := as.numeric(str_remove(str_extract(Season, "_.*"), "_"))]
#- coordinates 
data[,x.coord := data$`Coordinates (wgs84)`]
data[,y.coord := data$`...6`]
#- drop variables 
data %<>% select(c(1,9:ncol(data)))

#- create taxon column in long format 
data %<>% pivot_longer(cols = !c("Site Name", "year", "season", "y.coord", "x.coord"), names_to = "taxon", values_to = "abundance")

#- drop absence records 
data %<>% filter(abundance != 0)
#- rename site variable 
data %<>% rename(original_site_name = 'Site Name')
#- add NA date variable 
data %<>% mutate(date = as.Date(NA))
#- add data set variable 
data %<>% mutate(data.set = "monitoring_greece")

#- verify that there is only one x and y coordinate per site name 
setDT(data)
data[, uniqueN(x.coord), by = "original_site_name"] |> pull(V1) |> table()
data[, uniqueN(y.coord), by = "original_site_name"] |> pull(V1) |> table()

#- except for one all are fine. The one special case needs to be fixed. 
data[, uniqueN(x.coord), by = "original_site_name"][V1 == 3]
data[original_site_name == "SYMVOLI" & year == 2010, original_site_name := "SYMVOLI_1"]
data[original_site_name == "SYMVOLI" & year == 2009, original_site_name := "SYMVOLI_2"]
data[original_site_name == "SYMVOLI" & year == 2008, original_site_name := "SYMVOLI_3"]
#- test spatial coordinates 
sites <- 
        unique(data, by = "original_site_name") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = 4326)

mapview(sites)
#- one site is far removed from all other sites towards the eastern border of Turkey. I will remove it. 
data %<>% filter(original_site_name != "DW_STRATOPEDO")
data.set.name = "greece_monitoring"
# - There is a duplicate entry in the data. The samples from the sites with the original names
# - TH_2033 and TH_4063 are identical. Therefore I drop the site TH_2033. 
data2 <- data

data2[, EPSG := 4236]
data2[, date := ymd(paste0(year, "/05/30"))]
source("02_R/91_misc/02_prep_step_1.R")
# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/greece_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/greece_monitoring/updated_types.rds")

source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/greece_monitoring/preped.rds")




