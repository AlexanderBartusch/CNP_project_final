############################################################################
# author Alexander Bartusch
# last change: 08.08.2025
# Description:
#
#   Description:
#   This script generates a series of ternary plots assuming different fractions of #   bioavailable organic carbon (OC).
#   10.08 % global average
#   18.5 % agricultural rivers
#   44.8 % sewage effluents
#   100 % bioavailability
#
############################################################################






# load required packages: 

library(tidyverse)
library(ggpubr)
library(ggtern)
library(dplyr)
library(lubridate)
library(readxl)
library(sf)
library(rnaturalearth)
library(patchwork)
library(grid)


# set working directory to source folder ----
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# create an output directory availability_CNP_basins in figures subfolder if not already exists: 

ifelse(!dir.exists("../figures/ternary_plots_CNP_basins"), dir.create("../../figures/ternary_plots_CNP_basins"), FALSE)


out_folder <- "../figures/ternary_plots_CNP_basins"


# Calculate mol ----
c_gmol <- 12.01
n_gmol <- 14.01
p_gmol <- 30.97



# Redfield ratio
c_imbal_eq_rfr <- function(cmol,nmol,pmol) (cmol / 106) / ((cmol / 106) + (nmol / 16) + (pmol))
n_imbal_eq_rfr <- function(cmol,nmol,pmol) (nmol / 16) / ((cmol / 106) + (nmol / 16) + (pmol))
p_imbal_eq_rfr <- function(cmol,nmol,pmol) pmol / ((cmol / 106) + (nmol / 16) + (pmol))







##### Read in input data:-----
exp_data_daily<-read.csv("../output_data/CNP_data_catchments/DOC_TOC_DIN_SRP_daily_median_sd_subbasins.csv")


# read in catchment attributes:
basin_atlas_lev12_available <- read.csv("../output_data/CNP_data_catchments/basin_atlas_v10_l12_data_available.csv")


# add column bioavailable OC: factor of bioavailable OC is 0.1008
exp_data_daily<-exp_data_daily%>%
  mutate(DOC_TOC_gL_bioav = DOC_TOC_gL *0.1008, DOC_TOC_gL_bioav_10_08 = DOC_TOC_gL *0.1008, DOC_TOC_gL_bioav_44_8_sewage = DOC_TOC_gL *0.448, DOC_TOC_gL_bioav_18_5_agric_riv = DOC_TOC_gL *0.185)


## AGGREGATE BY MEDIAN PER CATCHMENT:
median_HYBAS_SRP <-exp_data_daily %>%
  group_by(HYBAS_ID)%>%
  summarize(median_DOC_TOC = median(DOC_TOC_gL_bioav), median_DIN = median(DIN_gL), median_SRP = median(SRP_gL),
            centroid_lon = first(centroid_lon), centroid_lat = first(centroid_lat))





### MEDIAN VALUES: aggregate by HYDROBASIN: -----
# refield ratio normalized: 
median_data_rfr <- median_HYBAS_SRP %>% 
  mutate(docb_mol = median_DOC_TOC / c_gmol) %>% 
  mutate(nreact_mol = median_DIN / n_gmol) %>% 
  mutate(srp_mol = median_SRP / p_gmol) %>% 
  mutate(c_imbal = c_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(n_imbal = n_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(p_imbal = p_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol))


#  ternary plot:
ternvrede_n_rfr_median <- median_data_rfr %>% ggtern(aes(x = p_imbal, y = c_imbal, z = n_imbal)) +
  #geom_point(aes(colour = centroid_lat),alpha = 0.4, size = 2, position = position_jitter_tern(x = 0.04, y = 0.04, z = 0.04)) +
  geom_point(color = "steelblue",alpha = 0.4, size = 2, position = position_jitter_tern(x = 0.04, y = 0.04, z = 0.04)) +
  #scale_color_gradient(low = "red", high = "#3399FF", na.value = NA) +
  #labs(color="Latitude") +
  geom_point(aes(x = 0.33, y = 0.33, z = 0.33), size = 3, color = "black", pch = 16) +
  geom_confidence_tern(breaks = c(0.5, 0.9,0.95), color = "black", size = 4.5)+
  Tlab("") + Llab("") + Rlab("") +
  Tarrowlab("rOC (%, RFR-normalized)") + Larrowlab("SRP (%, RFR-normalized)") + Rarrowlab("DIN (%, RFR-normalized)") +
  theme_light(base_size = 14) +
  theme_showarrows() +
  theme_latex(TRUE)+
  #ggtitle(paste0("Median DOC_TOC:DIN:SRP ratios n = ",nrow(median_data_rfr)," catchments"))+
  geom_path(data = data.frame(x = c(0.2, 0.2),
                              y = c(0.8, 0.0),
                              z = c(0.0, 0.8)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  geom_path(data = data.frame(x = c(0.0, 0.8),
                              y = c(0.8, 0.0),
                              z = c(0.2, 0.2)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  geom_path(data = data.frame(x = c(0.8, 0.0),
                              y = c(0.2, 0.2),
                              z = c(0.0, 0.8)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  labs(tag = 'a')+
  theme(plot.tag.position = c(0.1, 0.91), plot.tag = element_text(face = "bold", size = 14, hjust = 0, vjust = 1))

ternvrede_n_rfr_median







## varying bioavailability of DOC: --------

# set to 18.5 percent bOC of agricultural rivers:
median_HYBAS_SRP_agric_riv_c <-exp_data_daily %>%
  group_by(HYBAS_ID)%>%
  summarize(median_DOC_TOC = median(DOC_TOC_gL_bioav_18_5_agric_riv), median_DIN = median(DIN_gL), median_SRP = median(SRP_gL),
            centroid_lon = first(centroid_lon), centroid_lat = first(centroid_lat))


# set to 44 percent bOC of sewage effluents:
median_HYBAS_SRP_sewage_c <-exp_data_daily %>%
  group_by(HYBAS_ID)%>%
  summarize(median_DOC_TOC = median(DOC_TOC_gL_bioav_44_8_sewage), median_DIN = median(DIN_gL), median_SRP = median(SRP_gL),
            centroid_lon = first(centroid_lon), centroid_lat = first(centroid_lat))


# refield ratio normalized: 
median_data_rfr_agric_riv_c <- median_HYBAS_SRP_agric_riv_c %>% 
  mutate(docb_mol = median_DOC_TOC / c_gmol) %>% 
  mutate(nreact_mol = median_DIN / n_gmol) %>% 
  mutate(srp_mol = median_SRP / p_gmol) %>% 
  mutate(c_imbal = c_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(n_imbal = n_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(p_imbal = p_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol))

# refield ratio normalized: 
median_data_rfr_sewage_c <- median_HYBAS_SRP_sewage_c %>% 
  mutate(docb_mol = median_DOC_TOC / c_gmol) %>% 
  mutate(nreact_mol = median_DIN / n_gmol) %>% 
  mutate(srp_mol = median_SRP / p_gmol) %>% 
  mutate(c_imbal = c_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(n_imbal = n_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(p_imbal = p_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol))


#  ternary plots: 


# agricultural rivers:
ternvrede_n_rfr_median_agric_riv_c <- median_data_rfr_agric_riv_c %>% ggtern(aes(x = p_imbal, y = c_imbal, z = n_imbal)) +
  #geom_point(aes(colour = centroid_lat),alpha = 0.4, size = 2, position = position_jitter_tern(x = 0.04, y = 0.04, z = 0.04)) +
  geom_point(color = "steelblue",alpha = 0.4, size = 2, position = position_jitter_tern(x = 0.04, y = 0.04, z = 0.04)) +
  #scale_color_gradient(low = "red", high = "#3399FF", na.value = NA) +
  #labs(color="Latitude") +
  geom_point(aes(x = 0.33, y = 0.33, z = 0.33), size = 3, color = "black", pch = 16) +
  geom_confidence_tern(breaks = c(0.5, 0.9,0.95), color = "black", size = 4.5)+
  Tlab("") + Llab("") + Rlab("") +
  Tarrowlab("rOC (%, RFR-normalized)") + Larrowlab("SRP (%, RFR-normalized)") + Rarrowlab("DIN (%, RFR-normalized)") +
  theme_light(base_size = 14) +
  theme_showarrows() +
  theme_latex(TRUE)+
  #ggtitle(paste0("Median DOC_TOC:DIN:SRP ratios n = ",nrow(median_data_rfr)," catchments"))+
  geom_path(data = data.frame(x = c(0.2, 0.2),
                              y = c(0.8, 0.0),
                              z = c(0.0, 0.8)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  geom_path(data = data.frame(x = c(0.0, 0.8),
                              y = c(0.8, 0.0),
                              z = c(0.2, 0.2)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  geom_path(data = data.frame(x = c(0.8, 0.0),
                              y = c(0.2, 0.2),
                              z = c(0.0, 0.8)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  labs(tag = 'b')+
  theme(plot.tag.position = c(0.1, 0.91), plot.tag = element_text(face = "bold", size = 14, hjust = 0, vjust = 1))

ternvrede_n_rfr_median_agric_riv_c





# sewage effluents:

ternvrede_n_rfr_median_sewage_c <- median_data_rfr_sewage_c %>% ggtern(aes(x = p_imbal, y = c_imbal, z = n_imbal)) +
  #geom_point(aes(colour = centroid_lat),alpha = 0.4, size = 2, position = position_jitter_tern(x = 0.04, y = 0.04, z = 0.04)) +
  geom_point(color = "steelblue",alpha = 0.4, size = 2, position = position_jitter_tern(x = 0.04, y = 0.04, z = 0.04)) +
  #scale_color_gradient(low = "red", high = "#3399FF", na.value = NA) +
  #labs(color="Latitude") +
  geom_point(aes(x = 0.33, y = 0.33, z = 0.33), size = 3, color = "black", pch = 16) +
  geom_confidence_tern(breaks = c(0.5, 0.9,0.95), color = "black", size = 4.5)+
  Tlab("") + Llab("") + Rlab("") +
  Tarrowlab("rOC (%, RFR-normalized)") + Larrowlab("SRP (%, RFR-normalized)") + Rarrowlab("DIN (%, RFR-normalized)") +
  theme_light(base_size = 14) +
  theme_showarrows() +
  theme_latex(TRUE)+
  #ggtitle(paste0("Median DOC_TOC:DIN:SRP ratios n = ",nrow(median_data_rfr)," catchments"))+
  geom_path(data = data.frame(x = c(0.2, 0.2),
                              y = c(0.8, 0.0),
                              z = c(0.0, 0.8)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  geom_path(data = data.frame(x = c(0.0, 0.8),
                              y = c(0.8, 0.0),
                              z = c(0.2, 0.2)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  geom_path(data = data.frame(x = c(0.8, 0.0),
                              y = c(0.2, 0.2),
                              z = c(0.0, 0.8)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  labs(tag = 'c')+
  theme(plot.tag.position = c(0.1, 0.91), plot.tag = element_text(face = "bold", size = 14, hjust = 0, vjust = 1))

ternvrede_n_rfr_median_sewage_c





# assuming 100 percent bioavailability: #####
median_HYBAS_SRP_100_percent_bioav <-exp_data_daily %>%
  group_by(HYBAS_ID)%>%
  summarize(median_DOC_TOC = median(DOC_TOC_gL), median_DIN = median(DIN_gL), median_SRP = median(SRP_gL),
            centroid_lon = first(centroid_lon), centroid_lat = first(centroid_lat))




# refield ratio normalized: 
median_data_rfr_100_percent_bioav <- median_HYBAS_SRP_100_percent_bioav %>% 
  mutate(docb_mol = median_DOC_TOC / c_gmol) %>% 
  mutate(nreact_mol = median_DIN / n_gmol) %>% 
  mutate(srp_mol = median_SRP / p_gmol) %>% 
  mutate(c_imbal = c_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(n_imbal = n_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(p_imbal = p_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol))


#  ternary plot:
ternvrede_n_rfr_median_100_percent_bioav <- median_data_rfr_100_percent_bioav %>% ggtern(aes(x = p_imbal, y = c_imbal, z = n_imbal)) +
  #geom_point(aes(colour = centroid_lat),alpha = 0.4, size = 2, position = position_jitter_tern(x = 0.04, y = 0.04, z = 0.04)) +
  geom_point(color = "steelblue",alpha = 0.4, size = 2, position = position_jitter_tern(x = 0.04, y = 0.04, z = 0.04)) +
  #scale_color_gradient(low = "red", high = "#3399FF", na.value = NA) +
  #labs(color="Latitude") +
  geom_point(aes(x = 0.33, y = 0.33, z = 0.33), size = 3, color = "black", pch = 16) +
  geom_confidence_tern(breaks = c(0.5, 0.9,0.95), color = "black", size = 4.5)+
  Tlab("") + Llab("") + Rlab("") +
  Tarrowlab("rOC (%, RFR-normalized)") + Larrowlab("SRP (%, RFR-normalized)") + Rarrowlab("DIN (%, RFR-normalized)") +
  theme_light(base_size = 14) +
  theme_showarrows() +
  theme_latex(TRUE)+
  #ggtitle(paste0("Median DOC_TOC:DIN:SRP ratios n = ",nrow(median_data_rfr)," catchments"))+
  geom_path(data = data.frame(x = c(0.2, 0.2),
                              y = c(0.8, 0.0),
                              z = c(0.0, 0.8)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  geom_path(data = data.frame(x = c(0.0, 0.8),
                              y = c(0.8, 0.0),
                              z = c(0.2, 0.2)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  geom_path(data = data.frame(x = c(0.8, 0.0),
                              y = c(0.2, 0.2),
                              z = c(0.0, 0.8)),
            aes(x, y, z), color = "black", linetype = "dashed")+
  theme(legend.position = 'none')+
  labs(tag = 'd')+
  theme(plot.tag.position = c(0.1, 0.91), plot.tag = element_text(face = "bold", size = 14, hjust = 0, vjust = 1))

ternvrede_n_rfr_median_100_percent_bioav






# save all plots: 

# bioavailability of agricultural rivers: 
ggsave(ternvrede_n_rfr_median_agric_riv_c,
       device = NULL,
       filename = 'tern_rfr_norm_rOC-DIN_SRP_agric_riv_median_confidence_lines.png',
       path = out_folder)

# bioavailability of sewage effluents:
ggsave(ternvrede_n_rfr_median_sewage_c,
       device = NULL,
       filename = 'tern_rfr_norm_rOC-DIN_SRP_sewage_median_confidence_lines.png',
       path = out_folder)



# 100% bioavailability:
ggsave(ternvrede_n_rfr_median_100_percent_bioav,
       device = NULL,
       filename = 'tern_rfr_norm_rOC-DIN_SRP_100_perc_bioav_median_confidence_lines.png',
       path = out_folder)








# COMBINE THE PLOTS: 

plot_combined <- ggplot() +
  annotation_custom(ggplotGrob(ternvrede_n_rfr_median), xmin=0, xmax=0.33, ymin=-Inf, ymax=Inf) +
  annotation_custom(ggplotGrob(ternvrede_n_rfr_median_agric_riv_c), xmin=0.33, xmax=0.67, ymin=-Inf, ymax=Inf) +
  annotation_custom(ggplotGrob(ternvrede_n_rfr_median_sewage_c), xmin=0.67, xmax=1, ymin=-Inf, ymax=Inf) +
  theme_void()

# show combined plot: 
plot_combined

# save combined plot with 10.08%, 18.5% and 44.8% bioavailable OC: 
ggsave(plot_combined,
       device = NULL,
       filename = 'tern_rfr_norm_OC-DIN_SRP_varying_rOC_median_confidence_lines.svg',
       path = out_folder,
       width = 12,
       height = 4)


ggsave(plot_combined,
       device = NULL,
       filename = 'tern_rfr_norm_OC-DIN_SRP_varying_rOC_median_confidence_lines.png',
       path = out_folder,
       width = 12,
       height = 4)



