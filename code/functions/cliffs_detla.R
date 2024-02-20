### --- FUNCTION: Wrapper around cliff's delta function --- ###

cliffs_d_custom <-
        function(data, response = "HC5") {

                river_types = "brt12"
                river_types_var <- data[[river_types]]
                # - rename river types variable in data for loop
                data %<>% rename(focal = all_of(river_types))
                # - create matrix that will save the results.
                # - bring levels into a plausible (ascending number) order
                river_type_numbers <-
                        unique(river_types_var) %>%
                        as.character() %>%
                        readr::parse_number()
                river_types_var <-
                        factor(river_types_var, levels = unique(river_types_var)[order(river_type_numbers)])
                data$focal <- river_types_var
                #res <- factor(x = paste0("RT",1:12), levels = c("RT1", "RT2", "RT3", "RT4", "RT5", "RT6", "RT7", "RT8", "RT9", "RT10", "RT11", "RT12"))
                # - one row for each combination of river types
                res <-
                        crossing(type1 = river_types_var, type2 = river_types_var)
                # - remove rows with twice the same type and add addtional variables
                res %<>%
                        filter(type1 != type2) %>%
                        mutate(
                                least_disturbed = ifelse(uniqueN(data$least.impacted) > 1, 0, 1),
                                outlier = ifelse(all(data$outlier_hdi == 0), 0, 1),
                                chemical = unique(data$chem)
                        ) %>%
                        setDT
                
                # - create a copy for loops
                res_fun <- copy(res)
                j.un <- uniqueN(data$focal)
                
                for (i in 1:j.un) {
                        for (j in 1:j.un) {
                                if (i == j)
                                        next()
                                j.data <-
                                        data %>%
                                        filter(focal %in% c(
                                                levels(data$focal)[i],
                                                levels(data$focal)[j]
                                        )) %>%
                                        droplevels()
                                
                                j.cliff <-
                                        cliff.delta(d = j.data[[response]],
                                                    f = j.data$focal)
                                res_fun[type1 == levels(data$focal)[i] &
                                                type2 == levels(data$focal)[j],
                                        c("cliffs_delta",
                                          "lower_limit",
                                          "upper_limit") :=
                                                .(j.cliff$estimate,
                                                  j.cliff$conf.int[1],
                                                  j.cliff$conf.int[2])]
                        }
                }
                res_fun$cliffs_delta_absolut <- abs(res_fun$cliffs_delta)
                return(res_fun)
        }
