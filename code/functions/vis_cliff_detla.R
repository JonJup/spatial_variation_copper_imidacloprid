### --- FUNCTION: VISUALIZE CLIFF'S DELTA RESULTS --- ### 


cliffplot <- function(data, outlier_var, li, chem,no.legend=T){
        
       # if (outlier == 1) outlier <- c(0,1)
        
        # - prepare data 
        # - Select outlier state, disturbance level, and chemical.
        # - Additionally remove values from  upper triangle
        prep.data <- 
                data %>%
                dplyr::filter(outlier == outlier_var & least_disturbed == li) %>%
                dplyr::filter(chemical == chem)
        
        prep.data$type1  <- droplevels(prep.data$type1) 
        river_type_numbers1 <-
                unique(prep.data$type1) %>%
                as.character() %>%
                readr::parse_number()
        river_type_numbers2 <-
                unique(prep.data$type2) %>%
                as.character() %>%
                readr::parse_number()
        prep.data$type1 <- factor(stringr::str_remove(as.character(prep.data$type1), "^RT"), levels = stringr::str_remove(as.character(unique(prep.data$type1)), "^RT")[order(river_type_numbers1)])
        prep.data$type2 <- factor(stringr::str_remove(as.character(prep.data$type2), "^RT"), levels = stringr::str_remove(as.character(unique(prep.data$type2)), "^RT")[order(river_type_numbers2)])
        prep.data <-  dplyr::filter(prep.data, readr::parse_number(as.character(type1)) > readr::parse_number(as.character(type2)))
        
        # - Prepare the title of the plot
        chem.long = ifelse(chem == "ATZ", "Atrazine", ifelse(chem == "Cu", "Copper", ifelse(chem == "IMD", "Imidacloprid", ifelse(chem == "LCH", "λ-Cyhalothirn", NA))))
        prep.title <- paste(chem.long)
        
        n.breaks <- unique(prep.data$cliffs_delta_absolut)
        n.breaks <- round(n.breaks, 1)
        n.breaks <- length(unique(n.breaks)) - 1
        
        # Custom palette
        pal <- wesanderson::wes_palette(name = "Zissou1", n = 5)
        
        scale_fill_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "fill", ...) {
                binned_scale("fill", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value, limit = c(0,1), breaks = c(0.125, .25, 0.5, .75), guide = guide, name = "|d|", ...)  
        }
        
        # - create plot 
        out.plot <- 
                ggplot(prep.data, aes(x = type1, y = type2)) +
                geom_raster(aes(fill = cliffs_delta_absolut)) +
                #geom_text(aes(label = cliffs_delta_absolut))
                geom_point(data = filter(prep.data, cliffs_delta_absolut > 0.47), size = 2, shape = 8) + 
                #scale_fill_viridis_c("|d|") +
                #scale_fill_stepsn(n.breaks = 10, colours = terrain.colors(12)) +  
                #scale_fill_fermenter("|d|", n.breaks = n.breaks, palette = "PuOr") + 
                scale_fill_fermenter_custom(pal) + 
                theme(
                        panel.grid.major = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank(),
                        axis.ticks = element_blank(),
                       # legend.justification = c(1, 0),
                        #legend.position = c(0.54, 0.7),
                        #legend.direction = "horizontal", 
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank()
                ) +
                # guides(fill = guide_colorbar(
                #         barwidth = 8,
                #         barheight = 2,
                #         title.position = "top",
                #         title.hjust = 0.5
                # )) + 
                ggtitle(prep.title) 
        out.plot
        if (no.legend == T){
                out.plot <- out.plot + theme(legend.position = "none")
        }
        
        return(out.plot)
}
