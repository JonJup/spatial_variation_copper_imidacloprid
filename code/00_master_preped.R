## -- master script prepped data -- ## 

## scripts
source("code/01_combine_data_sets.R")
source("code/02_process_site_data.R")
source("code/03_hSSD_predict.R")
source("code/04_check_mcmc.R")
source("code/05_compute_hc5.R")
source("code/06_combine_compounds.R")
source("code/07_id_outliers.R")
source("code/08_analysis_cliffs_delta.R")
source("code/09_differences_in_median_HC5.R")
# source("code/number_of_sites.R")
# source("code/number_of_taxa_after_dropping_non_stationary.R")

#- figures 
source("code/figures/figure_cliffs_delta.R")
source("code/figures/figure_density_w_outliers.R")
source("code/figures/figure_differences_in_median.R")
source("code/figures/figure_hc_rivertype.R")
source("code/figures/figure_map_of_samples.R")

#- Supplementary Materials
source("02_R/03_supplementary/SM01_summary_datasets.R")
source("02_R/03_supplementary/SM02_differences_in_median_HC5_all_sites.R")
source("02_R/03_supplementary/SM03_cliff_ife.R")
source("02_R/03_supplementary/SM04_cliff_enz.R")
source("02_R/03_supplementary/SM05_differences_in_median_HC5_ife.R")
source("02_R/03_supplementary/SM06_differences_in_median_HC5_enz.R")
source("02_R/03_supplementary/SM07_create_SM_tables_w_EC50_values.R")
source("02_R/03_supplementary/SM08_observations_and_taxa_per_river_type.R")
source("02_R/03_supplementary/SM09_sensitivity_of_dropped_taxa.R")

#- Supplementary figures
source("02_R/02_figures/supplementary/SM_figure_cliffs_delta_ife.R")
source("02_R/02_figures/supplementary/SM_figure_hc_rivertype_ife.R")
source("02_R/02_figures/supplementary/SM_figure_hc_rivertype_enz.R")
source("02_R/02_figures/supplementary/SM_figure_hc_rivertype_ife_and_enz.R")
source("02_R/02_figures/supplementary/SM_figure_hc_rivertype_all_sites.R")
source("02_R/02_figures/supplementary/SM_figure_differences_in_median_ife.R")
source("02_R/02_figures/supplementary/SM_figure_differences_in_median_enz.R")
source("02_R/02_figures/supplementary/SM_figure_differences_in_median_all_sites.R")


