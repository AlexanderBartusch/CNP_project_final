
###################################################################################
###############################################################################
# author: Alexander Bartusch
# last change: 08.08.2025

# This script is used to transform the calculated partial dependence plots into ternary plots:
#  - these ternary plots are calculated for the five most important catchment 
#    features: 
#      * soil water content: 'swc_pc_uyr'
#      * potential evapotranspiration: 'pet_mm_uyr'
#      * human development index: 'hdi_ix_sav'
#      * wetland extent: 'wet_pc_ug2'
#      * population density: 'ppd_pk_uav'


##################################################################################


# load packages
library(tidyverse)
library(ggtern)
library(ggplot2)

# Settings: ####
# set working directory:
setwd("../output_data/ML_analysis/partial_dependence_plots_paper")


# define outfolder:
outfolder<-"../../../figures"


# Calculate mol ----
c_gmol <- 12.01
n_gmol <- 14.01
p_gmol <- 30.97



# Redfield ratio:
c_imbal_eq_rfr <- function(cmol,nmol,pmol) (cmol / 106) / ((cmol / 106) + (nmol / 16) + (pmol))
n_imbal_eq_rfr <- function(cmol,nmol,pmol) (nmol / 16) / ((cmol / 106) + (nmol / 16) + (pmol))
p_imbal_eq_rfr <- function(cmol,nmol,pmol) pmol / ((cmol / 106) + (nmol / 16) + (pmol))


##### Read in input data:-----

exp_data_daily<-read.csv("../../CNP_data_catchments/DOC_TOC_DIN_SRP_daily_median_sd_subbasins.csv")


# read in catchment attributes:
basin_atlas_lev12_available <- read.csv("../../CNP_data_catchments/basin_atlas_v10_l12_data_available.csv")




# add columns containing catchment attributes to CNP data:
exp_data_daily<-merge(exp_data_daily, basin_atlas_lev12_available[, c("HYBAS_ID", "clz_cl_smj", "crp_pc_sse", "swc_pc_uyr", "pet_mm_uyr", "wet_pc_ug2", "ppd_pk_uav", "hdi_ix_sav")], by.x = "HYBAS_ID", by.y = "HYBAS_ID", all.x = TRUE)



# add column bioavailable OC: factor of bioavailable OC is 0.1008:
exp_data_daily<-exp_data_daily%>%
  mutate(DOC_TOC_gL_bioav = DOC_TOC_gL *0.1008, DOC_TOC_gL_bioav_10_08 = DOC_TOC_gL *0.1008, DOC_TOC_gL_bioav_44_8_sewage = DOC_TOC_gL *0.448)


## AGGREGATE BY MEDIAN PER CATCHMENT:
median_HYBAS_SRP <-exp_data_daily %>%
  group_by(HYBAS_ID)%>%
  summarize(median_DOC_TOC = median(DOC_TOC_gL_bioav), median_DIN = median(DIN_gL), median_SRP = median(SRP_gL),
            centroid_lon = first(centroid_lon), centroid_lat = first(centroid_lat), swc_pc_uyr = first(swc_pc_uyr),
            pet_mm_uyr = first(pet_mm_uyr),
            wet_pc_ug2= first(wet_pc_ug2),
            ppd_pk_uav = first(ppd_pk_uav),
            hdi_ix_sav = first(hdi_ix_sav))

### MEDIAN VALUES: aggregate by HYDROBASIN: -----

# refield ratio normalized: 
median_data_rfr <- median_HYBAS_SRP %>% 
  mutate(docb_mol = median_DOC_TOC / c_gmol) %>% 
  mutate(nreact_mol = median_DIN / n_gmol) %>% 
  mutate(srp_mol = median_SRP / p_gmol) %>% 
  mutate(c_imbal = c_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(n_imbal = n_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(p_imbal = p_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol))




# read data:
data<-read.csv("individual_pdp_values_original_scale_min_max.csv")


# load predicted RFR normalized data:

pred_mod<-read.csv("../predicted_data/predicted_cnp_all_catchments_and_models.csv")


# filter the data from the first model and select predicted values in original scale:
predicted_cnp_rfr <-pred_mod%>%
  filter(nrand == 0)%>%
  select(c("HYBAS_ID", "comp", "preds"))%>%
  pivot_wider(names_from = comp, values_from = preds)%>%
  mutate(HYBAS_ID = as.numeric(HYBAS_ID))







# NOW PLOT TERNARY PLOTS WITH ARROWS FOR THE MOST IMPORTANT CATCHMENT FEATURES:


# most important catchment controls:
perm_imp_features = c('swc_pc_uyr', 'pet_mm_uyr', 'hdi_ix_sav', 'wet_pc_ug2', 'ppd_pk_uav')

feature_names <- c(swc_pc_uyr = "Soil water content", pet_mm_uyr = "Potential evapotranspiration", hdi_ix_sav = "Human development index", wet_pc_ug2 = "Wetland extent", ppd_pk_uav = "Population density")

# different model runs:
nmodel <-0 #seq(0,4,1)

# create an empty list of plots:
tern_plots <-list()




for (feat in perm_imp_features){
  print(feat)
  # retrieve variable name using the feature as the key:
  var_name <- feature_names[feat]
  
  
  
  # compute main directions: 
  data_10_percent_max<-median_data_rfr%>%
    filter(get(feat) >= quantile(median_data_rfr[[feat]], 0.9))%>%
    summarize(c_imbal = mean(c_imbal), n_imbal = mean(n_imbal), p_imbal = mean(p_imbal))%>%
    mutate(nr = 2)
  
  # get HYBAS_ID's of upper 10percent of the corresponding feat:
  HYBAS_10perc_up<-median_data_rfr%>%
    filter(get(feat) >= quantile(median_data_rfr[[feat]], 0.9))%>%
    pull(HYBAS_ID)
  
  # get mean values of predicted RFR normalized ratios:
  preds_10_percent_max<-predicted_cnp_rfr%>%
    filter(HYBAS_ID %in% HYBAS_10perc_up)%>%
    summarize(c_imbal = mean(c_imbal, na.rm = TRUE), 
              n_imbal = mean(n_imbal, na.rm = TRUE), 
              p_imbal = mean(p_imbal, na.rm = TRUE))%>%
    mutate(nr = 2)
  
  # lower 10 percent: 
  data_10_percent_min<-median_data_rfr%>%
    filter(get(feat) <= quantile(median_data_rfr[[feat]], 0.1))%>%
    summarize(c_imbal = mean(c_imbal), n_imbal = mean(n_imbal), p_imbal = mean(p_imbal))%>%
    mutate(nr = 1)
  
  # get HYBAS_ID's of lower 10percent of the corresponding feat:
  HYBAS_10perc_low<-median_data_rfr%>%
    filter(get(feat) <= quantile(median_data_rfr[[feat]], 0.1))%>%
    pull(HYBAS_ID)
  
  # get mean values of predicted RFR normalized ratios:
  preds_10_percent_min<-predicted_cnp_rfr%>%
    filter(HYBAS_ID %in% HYBAS_10perc_low)%>%
    summarize(c_imbal = mean(c_imbal, na.rm = TRUE),
              n_imbal = mean(n_imbal, na.rm = TRUE),
              p_imbal = mean(p_imbal, na.rm = TRUE))%>%
    mutate(nr = 1)
  
  
  
  mean_min_max_10perc <- bind_rows(data_10_percent_min, data_10_percent_max)
  
  mean_min_max_preds <- bind_rows(preds_10_percent_min, preds_10_percent_max)
  
  
  
  for (nr in nmodel){
  # filter data of corresponding model:
      tern_data <- data %>%
        filter(nrand == nr & feature == feat)%>%
        group_by(catch_ind)%>%
        mutate(dist = sqrt((c_imbal[1] - c_imbal[2])^2 + (n_imbal[1] - n_imbal[2])^2 + (p_imbal[1] - p_imbal[2])^2))%>%
        ungroup()
      
    
     tern_plot<- tern_data%>%
       # rescale the distance values according to the range of the continuous alpha value
      mutate(dist = scales::rescale(dist, to = c(0.1,1)))%>%
      ggtern(aes(x = p_imbal, y = c_imbal, z = n_imbal, color = feature_val)) +
      #geom_point() +
      #geom_path(aes(group = catch_ind, alpha = dist), color = "black", size = 0.2)+ #, alpha = 0.1) +
      geom_point(aes(x = 0.33, y = 0.33, z = 0.33), size = 3, color = "black", pch = 16) +
      labs(color=feat)+
      Tlab("") + Llab("") + Rlab("") +
      Tarrowlab("rOC (%, RFR-normalized)") + Larrowlab("rP (%, RFR-normalized)") + Rarrowlab("rN (%, RFR-normalized)") +
      theme_light(base_size = 18) +
      theme_showarrows() +
      theme_latex(TRUE) +
      geom_path(data = data.frame(x = c(0.2, 0.2),
                                y = c(0.8, 0.0),
                                z = c(0.0, 0.8)),
              aes(x, y, z), color = "black", linetype = "dashed") +
      geom_path(data = data.frame(x = c(0.0, 0.8),
                                y = c(0.8, 0.0),
                                z = c(0.2, 0.2)),
              aes(x, y, z), color = "black", linetype = "dashed") +
      geom_path(data = data.frame(x = c(0.8, 0.0),
                                y = c(0.2, 0.2),
                                z = c(0.0, 0.8)),
              aes(x, y, z), color = "black", linetype = "dashed") +
      # Add arrows indicating direction of increasing 'feature_val'
      geom_segment(data = tern_data %>%
                   group_by(catch_ind) %>%
                   arrange(feature_val) %>% # Order by feature_val within each catch_ind
                   mutate(
                     xend = lead(p_imbal),
                     yend = lead(c_imbal),
                     zend = lead(n_imbal)
                   ) %>% 
                   filter(!is.na(xend)),  # Filter out rows where lead values are NA
                 aes(x = p_imbal, y = c_imbal, z = n_imbal,
                     xend = xend, yend = yend, zend = zend,
                 alpha = dist, size = dist   ),
                 color = "black", # Set arrow color to black
                 #size = 0.2,      # Set arrow line thickness
                 #alpha = 0.1,
                 arrow = arrow(type = "open", length = unit(0.1, "inches")))+
      scale_alpha_continuous(range = c(0.1, 1), guide = 'none')+
      scale_size_continuous(range = c(0.1, 1), guide = 'none')+
       geom_segment(data = mean_min_max_10perc, aes(x = p_imbal[1], y = c_imbal[1], z = n_imbal[1], 
                                                    xend = p_imbal[2], yend = c_imbal[2], zend = n_imbal[2]),
                    arrow = arrow(length = unit(0.2, "cm")), color = "red")+
       geom_segment(data = mean_min_max_preds, aes(x = p_imbal[1], y = c_imbal[1], z = n_imbal[1], 
                                                    xend = p_imbal[2], yend = c_imbal[2], zend = n_imbal[2]),
                    arrow = arrow(length = unit(0.2, "cm")), color = "yellow")+
      #ggtitle(paste0('PDP min max ', feat, ' model ', nr))+
      #ggtitle(var_name)+
       #theme_light(TRUE)+
       theme_custom(
         base_size = 20,
         base_family = "",
         #tern.plot.background = element_rect(fill = 'transparent'),
         tern.panel.background = element_rect(fill = 'transparent'),
         col.T = '#DAA520',
         col.L = '#4169E1',
         col.R = '#006400',
         col.grid.minor = "white"
       ) +
       theme_showarrows()+
       Tlab("") + Llab("") + Rlab("") +
       Tarrowlab("rOC (%, RFR-normalized)") + Larrowlab("rP (%, RFR-normalized)") + Rarrowlab("rN (%, RFR-normalized)")
       
     
     
     
    print(tern_plot)
    tern_plots[[feat]]<-tern_plot
  
  # save plot:
 
    ggsave(tern_plot,
          device = NULL,
          filename = paste0('tern_rfr_pdp_axis_color',feat, '_model_',nr,'alpha_var_main_dir_mod.svg'),
          path = outfolder,
          width = 15,
          height = 10,
          unit = "cm",
          dpi = 400)
    
    ggsave(tern_plot,
           device = NULL,
           filename = paste0('tern_rfr_pdp_axis_color',feat, '_model_',nr,'alpha_var_main_dir_mod.png'),
           path = outfolder,
           dpi = 300,                        
           width = 8, height = 6, units = "in")
    
    # save as pdf:
    ggsave(tern_plot,
           device = NULL,
           filename = paste0('tern_rfr_pdp_axis_color',feat, '_model_',nr,'alpha_var_main_dir_mod.pdf'),
           path = outfolder,
           dpi = 300,                        
           width = 8, height = 6, units = "in")
    }
  
}



# arrange tern_plots in one large figure:

plot_combined<- ggtern::grid.arrange(tern_plots[["swc_pc_uyr"]], tern_plots[["pet_mm_uyr"]], tern_plots[["wet_pc_ug2"]], tern_plots[["hdi_ix_sav"]], tern_plots[["ppd_pk_uav"]], ncol = 2, nrow = 3)
plot_combined



ggsave(plot_combined,
       device = NULL,
       filename = paste0('tern_rfr_pdp_all_important_features',feat, '_model_',nr,'alpha_var.svg'),
       path = outfolder,
       dpi = 600,                        
       width = 6, height = 9, units = "in")



ggsave(plot_combined,
       device = NULL,
       filename = paste0('tern_rfr_pdp_all_important_features',feat, '_model_',nr,'alpha_var.png'),
       path = outfolder,
       dpi = 300,                        
       width = 6, height = 9, units = "in")


ggsave(plot_combined,
       device = NULL,
       filename = paste0('tern_rfr_pdp_all_important_features',feat, '_model_',nr,'alpha_var.pdf'),
       path = outfolder,
       #dpi = 300,                        
       width = 6, height = 9, units = "in")



