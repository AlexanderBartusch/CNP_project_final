
# Content:

This repository contains Python and R code scripts to reproduce the results of the manuscript titled "Human pressures intensify stoichiometrc nitrogen excess in streams and rivers". 




# Required input data:

* 7 water quality datasets
* 7 files containing the corresponding station IDs with coordinates
* HYDROBASINS: layer: BasinATLAS_v10_lev12
* HYDRORIVERS: HydroRIVERS_v10
* TWI: TWI_global.tif file calculated in QGIS

All raw data required for this project is permanently archived on [Zenodo](https://zenodo.org/records/17978253). The Zenodo repository includes:

* raw data (complete and unmodified)
* data description and all information regarding the original data sources and download procedures
* the preprocessed dataset used in the analyses presented here


**To reproduce the results run the given Python and R scripts in the given order:**

# Data preperation:

## A.) python/STEP_A_outlier_remove_raw_data:

1. p_01_detection_and_removal_outlier_raw_data.ipynb
2. p_02_data_analysis_after_outlier_removal.ipynb

## B.) python/STEP_B_merge_datasets:

3. p_03_mergeStationData.ipynb
4. p_04_merge_outlier_removed_RawData.ipynb
5. p_05_filter_dup_rows_merged_RawData.ipynb
6. p_06_assign_stations_to_HydroRivers.ipynb
7. p_07_Assign_stations_subbasins.ipynb
8. p_08_compute_distance_matrix.ipynb
9. p_09_compute_subset_distance_matrix.ipynb
10. p_10_filter_pot_duplicated_stations_add_infos.ipynb
11. p_11_agg_0km_dist_stations_and_wq_data.ipynb

## C.) python/STEP_C_aggregate_data_by_catchments:

12. p_12_generate_table_wq_data_subbasin.ipynb
13. p_13_filter_wq_data_HYBAS_CNP_available.ipynb
14. p_14_extract_basin_attributes_basins_data_ava.ipynb
15. p_15_extract_TWI90_of_all_available_catchments_globTWI.ipynb

please also run the R script r_01_extract_TWI90_of_each_catchment_DOC_DIN_SRP.R here (if the results are similar)

16. p_16_select_relevant_columns_cnp_export.ipynb
17. p_17_datapreperation_MLmodels_input.ipynb
18. p_18_matrix_correlation_features.ipynb
19. p_19_base_statistics_nutrient_data.ipynb


# Machine learning models and analysis of the results:

## D.) python/STEP_D_ML_analysis:

### 1_hyperparameter_tuning:

20. p_20_DIN_boxcox_redfield_randomizedsearch.ipynb
21. p_21_DINabs_boxcox_redfield_randomizedsearch.ipynb
22. p_22_rOC_boxcox_redfield_randomizedsearch.ipynb
23. p_23_rOCabs_boxcox_redfield_randomizedsearch.ipynb
24. p_24_SRP_boxcox_redfield_randomizedsearch.ipynb
25. p_25_SRPabs_boxcox_redfield_randomizedsearch.ipynb
26. p_26_rerun_optimized_models.ipynb


### 2_feature_analysis:

27. p_27_feature_importance_analysis.ipynb
28. p_28_relative_perm_importance_rescaled_100percent.ipynb
29. p_29_relative_perm_importance_barplots_paper.ipynb
30. p_30_final_calculation_partial_dependence_values_orig_scale_paper.ipynb
31. p_31_orig_scale_partial_dependence_plots.ipynb
32. p_32_feature_interaction_calculation.ipynb
33. p_33_model_predictions_optimized_models.ipynb


# To reproduce the ternary plots from the manuscript run the following R scripts:

34. r_02_ternary_plots_SRP_results_paper_redfield.R
35. r_03_ternary_plots_most_important_features.R
36. r04_ternary_plots_varying_OC_bioavailability.R



# Python version and modules

Python version 3.9.12 was used in the data analyses.

The Python code within this repository only uses the following Python modules:

1. pandas
2. numpy
3. matplotlib.pyplot
4. glob
5. plotnine
6. os
7. geopandas as gpd
8. geodatasets
9. shapely
10. from shapely.geometry import Point
11. glob
12. tqdm
13. time
14. pickle
15. multiprocessing
16. seaborn
17. scipy
18. openpyxl
19. scikit-learn
20. lightgbm
21. shap
22. ast
23. pyartemis



# R version and packages

R version 4.4.2 was used in the data analyses.

The R code within this repository only uses the following R libraries:

1. exactextractr
2. terra
3. sf
4. tidyverse
5. ggplot2
6. ggpubr
7. ggtern
8. dplyr
9. lubridate
10. readxl
11. rnaturalearth
12. patchwork
13. latex2exp
14. grid
15. purrr


# Disclaimers

The authors do not take any responsibility for damage done to the user system by executing the code of this repository.
