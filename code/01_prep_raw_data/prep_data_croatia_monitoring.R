# ———————————————————————————————————————— #
# ——— Clean data: Croatia — Monitoring ——— # 
# ———————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  --------------------------------------------------------

bio   <- read_excel("01_data/01_raw/croatia_monitoring/Copy of Baza MZB MED GIG.xlsx") 
sites <- read_excel("01_data/01_raw/croatia_monitoring/MED gig geo.xlsx") 


# prepare data ----------------------------------------------------------------------


bio2 <- 
  bio |> 
  select(-ID_ART) |>
  pivot_longer(cols = 2:52) |> 
  filter(value != 0) |> 
  mutate(value = 1) |> 
  mutate(name = str_remove_all(name, "^[:digit:]+")) |> 
  mutate(name = str_remove_all(name, "^\\.\\ ")) |> 
  setDT()
  

sites2 <- 
  sites |>
  select(c("MJERNA POSTAJA", "X HTRS", "Y HTRS")) |>
  rename(original_site_name = "MJERNA POSTAJA",
         X = "X HTRS",
         Y = "Y HTRS") |>
  mutate(EPSG = 3765) |>
  filter (!is.na(X) & !is.na(Y)) 

#sites3 <- st_as_sf(sites2, coords = c(2:3), crs = 3765)
sites3 = sites2
setDT(sites3)

dates <- 
  sites |> 
  select(c("...17", "...18")) |> 
  rename(original_site_name = "...17", 
         date               = "...18") |> 
  filter(!is.na(date)) |> 
    mutate(date = ifelse(date == "42933", "17.07.2017", date)) |> 
    mutate(date = ifelse(date == "42935", "19.07.2017", date)) |> 
  slice(2:n()) |> 
  mutate(date = lubridate::dmy(date)) |> 
  setDT()


setorderv(sites3, "original_site_name")

sites3[,site_id := 1:.N]
dat.u <- dates$original_site_name |> unique()
dates[original_site_name == dat.u[1], site_id := 4]
dates[original_site_name == dat.u[2], site_id := 19]
dates[original_site_name == dat.u[3], site_id := 12]
dates[original_site_name == dat.u[4], site_id := 6]
dates[original_site_name == dat.u[5], site_id := 11]
dates[original_site_name == dat.u[6], site_id := 18]
dates[original_site_name == dat.u[7], site_id := 5]
dates[original_site_name == dat.u[8], site_id := 22]
dates[original_site_name == dat.u[9], site_id := 1]
dates[original_site_name == dat.u[10], site_id := 20]
dates[original_site_name == dat.u[11], site_id := 14]
dates[original_site_name == dat.u[12], site_id := 7]
dates[original_site_name == dat.u[13], site_id := 8]
dates[original_site_name == dat.u[14], site_id := 10]
dates[original_site_name == dat.u[15], site_id := 21]
dates[original_site_name == dat.u[16], site_id := 23]
dates[original_site_name == dat.u[17], site_id := 13]
dates[original_site_name == dat.u[18], site_id := 24]
dates[original_site_name == dat.u[19], site_id := 9]
dates[original_site_name == dat.u[20], site_id := 15]
dates[original_site_name == dat.u[21], site_id := 16]
dates[original_site_name == dat.u[22], site_id := 66]
dates[original_site_name == dat.u[23], site_id := 25]
dates[original_site_name == dat.u[24], site_id := 69]
dates[original_site_name == dat.u[25], site_id := 62]
dates[original_site_name == dat.u[26], site_id := 49]
dates[original_site_name == dat.u[27], site_id := 65]
dates[original_site_name == dat.u[28], site_id := 51]
dates[original_site_name == dat.u[29], site_id := 45]
dates[original_site_name == dat.u[30], site_id := 33]
dates[original_site_name == dat.u[31], site_id := 60]
dates[original_site_name == dat.u[32], site_id := 58]
dates[original_site_name == dat.u[33], site_id := 55]
dates[original_site_name == dat.u[34], site_id := 56]
dates[original_site_name == dat.u[35], site_id := 28]
dates[original_site_name == dat.u[36], site_id := 57]
dates[original_site_name == dat.u[37], site_id := 63]
dates[original_site_name == dat.u[38], site_id := 64]
dates[original_site_name == dat.u[39], site_id := 27]
dates[original_site_name == dat.u[40], site_id := 26]
dates[original_site_name == dat.u[41], site_id := 48]
dates[original_site_name == dat.u[42], site_id := 29]
dates[original_site_name == dat.u[43], site_id := 46]
dates[original_site_name == dat.u[44], site_id := 30]
dates[original_site_name == dat.u[45], site_id := 41]
dates[original_site_name == dat.u[46], site_id := 39]
dates[original_site_name == dat.u[47], site_id := 42]
dates[original_site_name == dat.u[48], site_id := 35]
dates[original_site_name == dat.u[49], site_id := 44]
dates[original_site_name == dat.u[50], site_id := 38]
dates[original_site_name == dat.u[51], site_id := 32]
dat.u[42]
sites3[!site_id %in% dates$site_id]


sites4 <- sites3 |> 
  left_join(select(dates, - original_site_name), by = "site_id")


sites5 <- filter(sites4, !is.na(date))

bu <- unique(bio2$name)

bio2[name == bu[1], site_id := 19]
bio2[name == bu[2], site_id := 66]
bio2[name == bu[3], site_id := 51]
bio2[name == bu[4], site_id := 65]
bio2[name == bu[5], site_id := 46]
bio2[name == bu[6], site_id := 1]
bio2[name == bu[7], site_id := 69]
bio2[name == bu[8], site_id := 45]
bio2[name == bu[9], site_id := 58]
bio2[name == bu[10], site_id := 29]
bio2[name == bu[11], site_id := 44]
bio2[name == bu[12], site_id := 32]
bio2[name == bu[13], site_id := 62]
bio2[name == bu[14], site_id := 60]
bio2[name == bu[15], site_id := 64]
bio2[name == bu[16], site_id := 38]
bio2[name == bu[17], site_id := 11]
bio2[name == bu[18], site_id := 5]
bio2[name == bu[19], site_id := 14]
bio2[name == bu[20], site_id := 8]
bio2[name == bu[21], site_id := 55]
bio2[name == bu[22], site_id := 56]
bio2[name == bu[23], site_id := 28]
bio2[name == bu[24], site_id := 57]
bio2[name == bu[25], site_id := 26]
bio2[name == bu[26], site_id := 12]
bio2[name == bu[27], site_id := 18]
bio2[name == bu[28], site_id := 20]
bio2[name == bu[29], site_id := 10]
bio2[name == bu[30], site_id := 21]
bio2[name == bu[31], site_id := 23]
bio2[name == bu[32], site_id := 24]
bio2[name == bu[33], site_id := 9]
bio2[name == bu[34], site_id := 15]
bio2[name == bu[35], site_id := 16]
bio2[name == bu[36], site_id := 25]
bio2[name == bu[37], site_id := 49]
bio2[name == bu[38], site_id := 63]
bio2[name == bu[39], site_id := 27]
bio2[name == bu[40], site_id := 3]
bio2[name == bu[41], site_id := 39]
bio2[name == bu[42], site_id := 4]
bio2[name == bu[43], site_id := 22]
bio2[name == bu[44], site_id := 41]
bio2[name == bu[45], site_id := 35]
bio2[name == bu[46], site_id := 7]
bio2[name == bu[47], site_id := 6]
bio2[name == bu[48], site_id := 33]
bio2[name == bu[49], site_id := 48]
bio2[name == bu[50], site_id := 42]

sites5 |> filter(!site_id %in% bio2$site_id)

#- Combine sites with macroinvertebrate samples 
data <- left_join(bio2, 
                  sites5,
                  by = "site_id")
setDT(data)
# The number of rows in data should be the same as in bio2 
nrow(bio2) == nrow(data)

#- Drop columns that are no longer required: name and site_id
data[, c("name", "site_id") := NULL]

#- add year and season 
data[, `:=` (year = year(date))]

data[, taxon := Taxonname]
data[, Taxonnoame := NULL]
data[, taxon := str_remove(taxon, "\\ ssp\\.")]
data[, taxon := str_remove(taxon, "\\ sp\\.")]
data[, taxon := str_remove(taxon, "-Gr\\.")]
data[, taxon := str_remove(taxon, "\\ Gen\\.")]
data[, taxon := str_remove(taxon, "\\ \\(karstic type\\)")]
data[taxon == "Philopotamus variegatus variegatus", taxon := "Philopotamus variegatus"]
data[taxon == "Polycentropus flavomaculatus flavomaculatus", taxon := "Polycentropus flavomaculatus"]
data[taxon == "Tubificidae juv with setae", taxon := "Tubificidae"]

data[, Taxonname := NULL]
data %<>% rename(abundance = value)
data %<>% rename(x.coord = X,
                 y.coord = Y)
data2 <- data
data.set.name = "croatioa_monitoring"
data2 <- data2[!is.na(x.coord)]
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
#source("02_R/91_misc/visual_check.R")
#saveRDS(updated_type, "01_data/02_prepared/01_single_data/croatia_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/croatia_monitoring/updated_types.rds")
source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/croatia_monitoring/preped.rds")
