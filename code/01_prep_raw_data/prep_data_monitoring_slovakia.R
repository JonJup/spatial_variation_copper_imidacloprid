# ————————————————————————————————————————— #
# ——— Clean data: Slovakia — MONITORING ——— # 
# ————————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------
source("02_R/91_misc/01_setup_prep_data.R")


# Load data -------------------------------------------------------------------------
data       <- read_excel("01_data/01_raw/slovakia_monitoring/Data source CZ for Jonathan_River Typology_corrected2.xlsx", sheet = 1) 

# PREPARE DATA   ---------------------------------------------------------------------
# — extract site data 
data1        <- data[1:7, ]
data1.site   <- data1[2,-1] |> unlist() |> unname()
data1.sample <- data1[3,-1] |> unlist() |> unname()
data1.season <- data1[6,-1] |> unlist() |> unname()
data1.year   <- data1[7,-1] |> unlist() |> unname() |> as.numeric()
data1.coords <- data1[4,-1] |> unlist() |> unname()
data1.coords <- data.table(original = data1.coords)
data1.coords[, x.original := str_extract(string = original, pattern = ".*N")]
data1.coords[, y.original := str_extract(string = original, pattern = "N, .*")   |>  str_remove(pattern = "N,")                               |>  str_trim()]
data1.coords[, x.degree   := str_extract(string = x.original, pattern = ".*°")   |>  str_remove(pattern = "°")                                |>  str_trim()]
data1.coords[, y.degree   := str_extract(string = y.original, pattern = ".*°")   |>  str_remove(pattern = "°")                                |>  str_trim()]
data1.coords[, x.minutes  := str_extract(string = x.original, pattern = "°.*'")  |>  str_remove(pattern = "°") |> str_remove(pattern = "'") |>  str_trim()]
data1.coords[, y.minutes  := str_extract(string = y.original, pattern = "°.*'")  |>  str_remove(pattern = "°") |> str_remove(pattern = "'") |>  str_trim()]
data1.coords[, x.seconds  := str_extract(string = x.original, pattern = "'.*\"") |>  str_remove(pattern = "'") |> str_remove(pattern = "\"") |>  str_trim()]
data1.coords[, y.seconds  := str_extract(string = y.original, pattern = "'.*\"") |>  str_remove(pattern = "'") |> str_remove(pattern = "\"") |>  str_trim()]
#- there is a few problematic entry 
unique(data1.coords$x.degree)
unique(data1.coords$y.degree)
unique(data1.coords$x.minute)
unique(data1.coords$y.minute)
unique(data1.coords$x.second)
unique(data1.coords$y.second)
#- put together new coordinates 
data1.coords[, x.new := paste0(x.degree, "°", x.minutes, "!", x.seconds, "§", "E")]
data1.coords[, y.new := paste0(y.degree, "°", y.minutes, "!", y.seconds, "§", "N")]
data1.coords[, x.new := as.numeric(sp::char2dms(data1.coords$x.new, chd = "°", chm = "!", chs = "§"))]
data1.coords[, y.new := as.numeric(sp::char2dms(data1.coords$y.new, chd = "°", chm = "!", chs = "§"))]

#- check on map 
site <-
        data1.coords |> 
        st_as_sf(coords = c("y.new", "x.new"), crs = 4326) 
mapview(site)

data1 <- data.table(original_site_name = data1.site, 
                    sample = data1.sample,
                    date = as.Date(NA),
                    year = data1.year,
                    season = data1.season,
                    x.coord = data1.coords$y.new,
                    y.coord = data1.coords$x.new,
                    EPSG = 4326)


data2 <- data[8:nrow(data), ]
names(data2) <- append("taxon", data1$sample)
data2 <- 
        data2 |> 
        pivot_longer(cols = !taxon,
                     names_to = "sample", 
                     values_to = "abundance"
        ) |> 
        filter(!is.na(abundance))


data3 <- left_join(x = data2, 
                   y = data1,
                   by = "sample")

data3$taxon %<>% str_remove("\\ sp\\.")
data3$taxon %<>% str_remove("\\ Gen\\.")
data3$taxon %<>% str_remove("\\ Ad\\.")
data3$taxon %<>% str_remove("\\ Lv\\.")
data3$taxon %<>% str_remove("\\ L\\.")
data3$taxon %<>% str_remove("\\ s\\.\\ lat\\.")
data3$taxon %<>% str_remove("-Gr\\.")
data3$taxon %<>% str_remove("\\ Gr\\.")
data3$taxon %<>% str_remove("\\ agg\\.")

setDT(data3)

data3[taxon == "Tvetenia bavarica/calvescens", taxon := "Tvetenia"]
data3[taxon == "Thienemanniella vittata/clavicornis", taxon := "Thienemanniella"]
data3[taxon == "Heterotrissocladius grimshawi/scutellatus", taxon := "Heterotrissocladius"]
data3[taxon == "Rhithrogena iridina/picteti", taxon := "Rhithrogena"]
data3[taxon == "Orthocladius obumbratus/oblidens", taxon := "Orthocladius"]


data3 <- data3[!is.na(taxon)]
data2 <- data3
data2[is.na(year), year := 2017]
data2[, date := ymd(paste0(year,"/05/30"))]
data.set.name = "slovakia_monitoring"
source("02_R/91_misc/02_prep_step_1.R")
# - visually check that sites belong to river types
# source("02_R/91_misc/visual_check.R")
# saveRDS(updated_type, "01_data/02_prepared/01_single_data/slovakia_monitoring/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/slovakia_monitoring/updated_types.rds")
source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/slovakia_monitoring/preped.rds")



