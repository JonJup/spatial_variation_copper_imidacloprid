outlier_plot_hdi <- function(chemical) {
        fill.color <-
                ifelse(chemical == "ATZ",
                       "#a0d699",
                       ifelse(
                               chemical == "Cu",
                               "#38bbf1",
                               ifelse(chemical == "IMD", "#d69b34", "#f0eaba")
                       ))
        
        prepdata <- data[chem == chemical]
        hdi.obj  <- hdi(prepdata$HC5)
        
        
        p <-
                ggplot(prepdata, aes(x = HC5, after_stat(scaled))) +
                scale_x_log10() +
                as_reference(geom_density(adjust = 2, fill = fill.color), id = "density") +
                #geom_density(data = filter(prepdata, least.impacted == TRUE), alpha = .6, fill = "white", adjust = 2, linewidth = 1) +
                with_blend(
                        annotate(
                                "rect",
                                xmin = prepdata[, min(HC5)],
                                xmax = hdi.obj[1],
                                ymin = -Inf,
                                ymax = Inf,
                                fill = "black"
                        ),
                        bg_layer = "density",
                        blend_type = "atop"
                ) +
                with_blend(
                        annotate(
                                "rect",
                                xmin = hdi.obj[2],
                                xmax = data[, max(HC5)],
                                ymin = -Inf,
                                ymax = Inf,
                                fill = "black"
                        ),
                        bg_layer = "density",
                        blend_type = "in"
                ) +
                theme(panel.background = element_blank(),
                      legend.position = "none", 
                      axis.title.y = element_blank(), 
                      axis.title.x = element_blank())
        return(p)
        
} 