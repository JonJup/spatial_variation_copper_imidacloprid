# ———————————————————————————————————————————————— #
# ——— Clean data: Czech Repbulic  — Biodrought ——— # 
# ———————————————————————————————————————————————— #

# SETUP -----------------------------------------------------------------------------

source("02_R/91_misc/01_setup_prep_data.R")

# LOAD DATA  ------------------------------------------------------------------------

data       <- read_excel("01_data/01_raw/czech_biodrought/Data source CZ for Jonathan_River Typology_corrected2.xlsx", sheet = 3) 
coords     <- read_excel("01_data/01_raw/czech_biodrought/Data source CZ for Jonathan_River Typology_corrected2.xlsx", sheet = 2)


# PREPARE DATA   ---------------------------------------------------------------------
#  — — — extract site data 
data1 <- data[1:5, -c(1:2)]
# - site names 
data1.site <- data1[2,] |> unlist() |> unname()
# - sample names 
data1.sample <- data1[1,] |> unlist() |> unname()
# - there is a small mistake in the sample names (see Email Petr Paril 24.08.2021)
data1.sample[6:11] %<>% str_replace("CZ_1", "CZ_2")
# - dates add manually from excel 
data1.date <- c("17.10.2012",	"10.04.2013",	"16.10.2013",	"02.04.2014",	"27.09.2014",	"25.04.2012",	"24.10.2012",	"12.04.2013",	"08.10.2013",	"28.03.2014",	"27.09.2014",	"11.10.2012",	"19.04.2013",	"15.10.2013",	"29.03.2014",	"28.09.2014",	"03.05.2012",	"11.10.2012",	"18.04.2013",	"15.10.2013",	"29.03.2014",	"28.09.2014",	"17.05.2012",	"23.10.2012",	"18.04.2013",	"11.10.2013",	"06.04.2014",	"02.05.2012",	"17.10.2012",	"11.04.2013",	"17.10.2013",	"01.04.2014",	"27.09.2014",	"11.10.2012",	"18.04.2013",	"15.10.2013",	"29.03.2014",	"28.09.2014",	"12.04.2013",	"08.10.2013",	"28.03.2014",	"27.09.2014",	"25.04.2012",	"24.10.2012",	"12.04.2013",	"08.10.2013",	"28.03.2014",	"27.09.2014",	"16.05.2012",	"24.10.2012",	"18.04.2013",	"11.10.2013",	"05.04.2014",	"16.10.2012",	"17.04.2013",	"10.10.2013",	"08.04.2014",	"02.10.2014",	"26.10.2012",	"25.04.2013",	"07.10.2013",	"30.03.2014",	"29.09.2014",	"23.05.2012",	"11.04.2013",	"18.10.2013",	"02.04.2014",	"28.09.2014",	"14.05.2012",	"17.10.2012",	"29.04.2013",	"07.10.2013",	"30.03.2014",	"29.09.2014",	"26.10.2012",	"29.04.2013",	"23.10.2013",	"30.03.2014",	"29.09.2014",	"19.04.2013",	"15.10.2013",	"29.03.2014",	"28.09.2014",	"18.10.2012",	"11.04.2013",	"17.10.2013",	"01.04.2014",	"27.09.2014",	"16.05.2012",	"23.10.2012",	"18.04.2013",	"11.10.2013",	"06.04.2014",	"16.05.2012",	"24.10.2012",	"19.04.2013",	"11.10.2013",	"05.04.2014",	"11.05.2012",	"26.10.2012",	"25.04.2013",	"07.10.2013",	"30.03.2014",	"29.09.2014",	"15.05.2012",	"17.10.2012",	"10.04.2013",	"16.10.2013",	"01.04.2014",	"27.09.2014",	"16.10.2012",	"17.04.2013",	"10.10.2013",	"08.04.2014",	"02.10.2014",	"18.10.2012",	"11.04.2013",	"17.10.2013",	"02.04.2014",	"28.09.2014")

# — — — coordinates 
# - site name from coordinate table. 
coord.site  <- coords[1,-1] |> unlist() |> unname()
# - raw coordinates 
coord.coord <- coords[4,-1] |> unlist() |> unname()
# - decompose into constituent parts 
data.coord <- data.table(site = coord.site, 
                         original = coord.coord)

data.coord[, x.original := str_extract(string = original, pattern = ".*,")      |> str_remove(pattern = "N") |> str_remove(",")             |> str_trim()]
data.coord[, y.original := str_extract(string = original, pattern = ",.*")      |> str_remove(pattern = "E") |> str_remove(",")             |> str_trim()]                       
data.coord[, x.degree   := str_extract(string = x.original, pattern = ".*°")    |> str_remove(pattern = "°")                                |>  str_trim()]
data.coord[, y.degree   := str_extract(string = y.original, pattern = ".*°")    |> str_remove(pattern = "°")                                |>  str_trim()]
data.coord[, x.minutes  := str_extract(string = x.original, pattern = "°.*\\'") |> str_remove(pattern = "°") |> str_remove(pattern = "\\'") |>  str_trim()]
data.coord[, y.minutes  := str_extract(string = y.original, pattern = "°.*\\'") |> str_remove(pattern = "°") |> str_remove(pattern = "\\'") |>  str_trim()]
data.coord[, x.seconds  := str_extract(string = x.original, pattern = "'.*\"")  |> str_remove(pattern = "'") |> str_remove(pattern = "\\.") |>  str_trim()]
data.coord[, y.seconds  := str_extract(string = y.original, pattern = "'.*\"")  |> str_remove(pattern = "'") |> str_remove(pattern = "\\.") |>  str_trim()]
data.coord[is.na(x.seconds), x.seconds := 0]
data.coord[is.na(y.seconds), y.seconds := 0]
# - reassemble 
# - put together new coordinates 
data.coord[, x.new := paste0(x.degree, "°", x.minutes, "!", x.seconds, "§", "E")]
data.coord[, y.new := paste0(y.degree, "°", y.minutes, "!", y.seconds, "§", "N")]
data.coord[, x.new := as.numeric(sp::char2dms(data.coord$x.new, chd = "°", chm = "!", chs = "§"))]
data.coord[, y.new := as.numeric(sp::char2dms(data.coord$y.new, chd = "°", chm = "!", chs = "§"))]

#- check on map 
site <-
        data.coord |> 
        st_as_sf(coords = c("y.new", "x.new"), crs = 4326) 
mapview(site)

#- save in new table 
data1 <- data.table(site = data1.site, 
                    sample = data1.sample,
                    date = dmy(data1.date),
                    EPSG = 4326, 
                    data.set = "biodrought")
#- reformat sites and join to data
data.coord2 <- data.coord[,c("site","x.new", "y.new")]
names(data.coord2) <- c("site", "y.coord", "x.coord")
data1        <- data.coord2[data1, on = "site"]
#- add year and season
data1 %<>% rename(original_site_name = site)
data1$year   <- year(data1$date)
#  — — — extract species data 
data2 <- # subset to species data 
        data[7:nrow(data), -c(1)] |> 
        # remove NA entries 
        filter(state != "NA")
#- rename columns 
names(data2) <- append("taxon", data1$sample)
# reshape data 
data2 <- 
        data2 |> 
        pivot_longer(cols = !taxon,
                     names_to = "sample", 
                     values_to = "abundance"
                 ) |> 
        filter(!is.na(abundance))

#- join site and species data 
data3 <- left_join(x = data2,
                   y = data1,
                   by = "sample")

#- clean taxon names
remove <- c("\\ sp\\.", "\\ Gen\\.", "\\ Ad\\.", "\\ Lv\\.", "\\ L\\.", "\\ s\\.\\ lat\\.", "-Gr\\.", "\\ Gr\\.", "\\ agg\\.")
data3$taxon %<>% str_remove_all(paste(remove, collapse = "|"))
rm(remove)
setDT(data3)
#- replace ambiguous taxa with higher level taxas  
data3[taxon == "Clinocera/Wiedemannia", taxon := "Empididae"]
data3[taxon == "Cricotopus patens/flavocinctus", taxon := "Cricotopus"]
data3[taxon == "Eukiefferiella clypeata/pseudomontana", taxon := "Eukiefferiella"]
data3[taxon == "Henlea ventriculosa/nasuta", taxon := "Henlea"]
data3[taxon == "Henlea/Fridericia", taxon := "Enchytraeidae"]
data3[taxon == "Heterotrissocladius grimshawi/scutellatus", taxon := "Heterotrissocladius"]
data3[taxon == "Hydracarina, Acari, Acarina", taxon := "Trombidiformes"]
data3[taxon == "Hydroporinae, Dytiscidae", taxon := "Dytiscidae"]
data3[taxon == "Nanocladius parvulus/rectinervis", taxon := "Nanocladius"]
data3[taxon == "Tvetenia bavarica/calvescens", taxon := "Tvetenia"]
data3[taxon == "Trichodrilus allobrogum/moravicus", taxon := "Trichodrilus"]
data3[taxon == "Rhyacophila fasciata/obliterata/pascoei/vulgaris", taxon := "Rhyacophila"]
data3[taxon == "Rhithrogena iridina/picteti", taxon := "Rhithrogena"]
#- remove non-macorinvertebrate entries 
data3 <- data3[!taxon %in% c("Salamandra salamandra", "Pisces")]
data3 <- data3[!is.na(taxon)]

data.set.name <- "czech_biodrought"
data2 <- data3
source("02_R/91_misc/02_prep_step_1.R")

# - visually check that sites belong to river types
# source("02_R/91_misc/visual_check.R")
# saveRDS(updated_type, "01_data/02_prepared/01_single_data/czech_biodrought/updated_types.rds")
updated_type <- readRDS("01_data/02_prepared/01_single_data/czech_biodrought/updated_types.rds")
source("02_R/91_misc/03_prep_data_step2.R")
data2
# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/02_prepared/01_single_data/czech_biodrought/preped.rds")

