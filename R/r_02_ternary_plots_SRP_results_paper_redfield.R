
###############################################################################
# This script produces ternary plots: 

# INPUT: 
#   -DOC_TOC_DIN_SRP_daily_median_sd_subbasins.csv in output_data
#   -basin_atlas_v10_l12_data_available.csv

# OUTPUT: plots showing: 
# a.) worldmap all sites
# b.) ternary plot: all available ratios (daily per catchment)
# c.) ternary plot: medians of all catchments (mean all available ratios per catchment)
# d.) plots distribution of depletion zones: ternary plots, worldmaps and barplots

###############################################################################

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
library(latex2exp)
library(grid)
library(purrr)


# set working directory to source folder ----
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# create an output directory availability_CNP_basins in figures subfolder if not already exists: 

ifelse(!dir.exists("../figures/ternary_plots_CNP_basins"), dir.create("../figures/ternary_plots_CNP_basins"), FALSE)


out_folder <- "../../figures/ternary_plots_CNP_basins"


# Calculate mol ----
c_gmol <- 12.01
n_gmol <- 14.01
p_gmol <- 30.97



# Redfield ratio:
c_imbal_eq_rfr <- function(cmol,nmol,pmol) (cmol / 106) / ((cmol / 106) + (nmol / 16) + (pmol))
n_imbal_eq_rfr <- function(cmol,nmol,pmol) (nmol / 16) / ((cmol / 106) + (nmol / 16) + (pmol))
p_imbal_eq_rfr <- function(cmol,nmol,pmol) pmol / ((cmol / 106) + (nmol / 16) + (pmol))


##### Read in input data:-----

exp_data_daily<-read.csv("../output_data/CNP_data_catchments/DOC_TOC_DIN_SRP_daily_median_sd_subbasins.csv")


# read in catchment attributes:
basin_atlas_lev12_available <- read.csv("../output_data/CNP_data_catchments/basin_atlas_v10_l12_data_available.csv")




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








# WORLD MAP all sites: ################################################
world <- ne_countries(returnclass = "sf")

# Create a ggplot object and plot the map with country borders
p <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_HYBAS_SRP, aes(x = centroid_lon, y = centroid_lat), alpha = 0.4, size = 1.5, color = "red") +
  theme_minimal() +
  ylab("latitude") +
  xlab("longitude") +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text = element_text(size = 14))

p




## TERNARY PLOTS DAILY: -----


# refield ratio normalized: 
data_rfr <- exp_data_daily %>% 
  mutate(docb_mol = DOC_TOC_gL_bioav / c_gmol) %>% 
  mutate(nreact_mol = DIN_gL / n_gmol) %>% 
  mutate(srp_mol = SRP_gL / p_gmol) %>% 
  mutate(c_imbal = c_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(n_imbal = n_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol)) %>% 
  mutate(p_imbal = p_imbal_eq_rfr(cmol = docb_mol, nmol = nreact_mol, pmol = srp_mol))






## plots:

#  ternary plot: 
ternvrede_n_rfr <- data_rfr %>% ggtern(aes(x = p_imbal, y = c_imbal, z = n_imbal)) +
  geom_point(aes(colour = centroid_lat),alpha = 0.1, size = 2.5, position = position_jitter_tern(x = 0.04, y = 0.04, z = 0.04)) +
  scale_color_gradient(low = "red", high = "#3399FF", na.value = NA) +
  labs(color="Latitude") +
  geom_point(aes(x = 0.33, y = 0.33, z = 0.33), size = 3, color = "black", pch = 16) +
  geom_confidence_tern(breaks = c(0.5, 0.9,0.95), color = "black", size = 4.5)+
  Tlab("") + Llab("") + Rlab("") +
  Tarrowlab("rOC (%, RFR-normalized)") + Larrowlab("rP (%, RFR-normalized)") + Rarrowlab("rN (%, RFR-normalized)") +
  theme_light(base_size = 14) +
  theme_showarrows() +
  theme_latex(TRUE)+
 # ggtitle(paste0("Daily DOC_TOC:DIN:SRP ratios n = ", nrow(data_rfr)))+
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
            aes(x, y, z), color = "black", linetype = "dashed")

ternvrede_n_rfr




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
  geom_point(aes(colour = centroid_lat),alpha = 0.4, size = 2, position = position_jitter_tern(x = 0.04, y = 0.04, z = 0.04)) +
  scale_color_gradient(low = "red", high = "#3399FF", na.value = NA) +
  labs(color="Latitude") +
  geom_point(aes(x = 0.33, y = 0.33, z = 0.33), size = 3, color = "black", pch = 16) +
  geom_confidence_tern(breaks = c(0.5, 0.9,0.95), color = "black", size = 4.5)+
  Tlab("") + Llab("") + Rlab("") +
  Tarrowlab("rOC (%, RFR-normalized)") + Larrowlab("rP (%, RFR-normalized)") + Rarrowlab("rN (%, RFR-normalized)") +
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
            aes(x, y, z), color = "black", linetype = "dashed")   

ternvrede_n_rfr_median

# save plot: 
ggsave(ternvrede_n_rfr_median,
       device = NULL,
       filename = 'tern_rfr_norm_rOC-DIN_SRP_median_confidence_lines.png',
       path = out_folder)










# Analysis of depletion zones ######

#1)
rP_rN_depletion <- median_data_rfr%>%
  filter(p_imbal<0.2 & c_imbal>0.6 & n_imbal<0.20)
#2)
rp_depletion <- median_data_rfr%>%
  filter(p_imbal<0.2 & c_imbal < 0.8 & c_imbal > 0.2 & n_imbal >0.2 & n_imbal <0.8)
#3)
rP_rOC_depletion <-median_data_rfr%>%
  filter(p_imbal<0.2 & c_imbal < 0.2 & n_imbal >0.6)
#4)
rOC_depletion <-median_data_rfr%>%
  filter(p_imbal<0.8 & p_imbal >0.2& c_imbal<=0.2 & n_imbal <=0.8 & n_imbal >=0.2)
  
#5)
roC_rN_depletion <-median_data_rfr%>%
  filter(p_imbal>0.6 & c_imbal < 0.2 & n_imbal <0.2)
#6)
rN_depletion <-median_data_rfr%>%
  filter(p_imbal>0.2 & p_imbal<0.8 & c_imbal > 0.2 & c_imbal<0.8 & n_imbal <=0.2)
#7)
redfield_zone<-median_data_rfr%>%
  filter(p_imbal<0.6 & p_imbal >0.2 & c_imbal <0.6 & c_imbal>0.2 & n_imbal <0.6 & n_imbal >0.2)


median_data_rfr_2<-median_data_rfr%>%
  mutate(depletion = ifelse(p_imbal<=0.2 & c_imbal>=0.6 & n_imbal<=0.20, "rP & rN",
                            ifelse(p_imbal<=0.2 & c_imbal <= 0.8 & c_imbal >= 0.2 & n_imbal >=0.2, "rP", 
                                   ifelse(p_imbal<=0.2 & c_imbal <= 0.2 & n_imbal >=0.6,"rP & rOC",
                                          ifelse(p_imbal<0.8 & c_imbal<=0.2 & n_imbal <=0.8 & n_imbal >=0.2,"rOC",
                                                 ifelse(p_imbal>=0.6 & c_imbal <= 0.2 & n_imbal <=0.2, "rOC & rN",
                                                        ifelse(p_imbal>=0.2 & p_imbal<=0.8 & c_imbal >= 0.2 & n_imbal <=0.2, "rN",
                                                               ifelse(p_imbal<=0.6 & p_imbal >=0.2 & c_imbal <=0.6 & c_imbal>=0.2 & n_imbal <=0.6 & n_imbal >=0.2, "none",NA))))))))


my_colors <- c("rP & rN" = "yellow", "rP" = "darkgreen", "rP & rOC" = "burlywood2","rOC" = "cornflowerblue", "rOC & rN" = "darkolivegreen3", "rN" = "orange", "none" = "pink")


my_colors2 <- c("rP & rN" = "#e6ab02", "rP" = "#66a61e", "rP & rOC" = "#7570b3","rOC" = "#d95f02", "rOC & rN" = "#1b9e77", "rN" = "#a6761d", "none" = "#e7298a")


## plot ternary plots with colors in depletion zones:

# add labels for confidence intervals and specify positions:
confidence_labels <- data.frame(
  x = c(0.33, 0.23, 0.25),       # Adjust positions based on your data
  y = c(0.17, 0.40, 0.5),
  z = c(0.5, 0.33, 0.25),
  label = c("50%", "90%", "95%") # Labels for confidence levels
)



ternvrede_n_rfr_dep_zones <- median_data_rfr_2 %>% ggtern(aes(x = p_imbal, y = c_imbal, z = n_imbal)) +
  geom_point(aes(colour = depletion),alpha = 0.4, size = 2, position = position_jitter_tern(x = 0.02, y = 0.02, z = 0.02)) +
  scale_color_manual(values = my_colors2) +
  #labs(color="Latitude") +
  geom_point(aes(x = 0.33, y = 0.33, z = 0.33), size = 3, color = "black", pch = 16) +
  geom_confidence_tern(aes(x = p_imbal, y = c_imbal, z = n_imbal), breaks = c(0.5, 0.9,0.95), inherit.aes =FALSE,color = "black", size = 4.5)+
  Tlab("") + Llab("") + Rlab("") +
  Tarrowlab("rOCRFR (%)") + Larrowlab("rPRFR (%)") + Rarrowlab("rNRFR (%)") +
  theme_light(base_size = 20) +
  theme_showarrows() +
  theme_latex(TRUE)+
  #ggtitle(paste0("Median DOC_TOC:DIN:SRP ratios by HYDROBASIN n = ",nrow(median_data_rfr)))+
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
  geom_text(data = confidence_labels, 
            aes(x = x, y = y, z = z, label = label),
            color = "black", size = 4, inherit.aes = FALSE) +  # Add confidence level labels
  theme(legend.position = "none") +
  labs(tag = "a") +
  theme(plot.tag.position = c(0.1, 0.91), plot.tag = element_text(face = "bold", size = 17, hjust = 0, vjust = 1))+
  theme(tern.axis.text.L = element_text(angle = 60),
        tern.axis.text.R = element_text(angle = -60))
ternvrede_n_rfr_dep_zones

ggsave(ternvrede_n_rfr_dep_zones,
       device = NULL,
       filename = '../figures/ternary_plots_CNP_basins/tern_plot_depleton_zones_conf_labels.svg',
     
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 400)

# save a table containing depletion zones and coordinates of the catchments:
unique_depletion_values <- unique(median_data_rfr_2$depletion)

catchment_export_depletion_zones_lat_lon <-median_data_rfr_2%>%
  select(HYBAS_ID, median_DOC_TOC, median_DIN, median_SRP, c_imbal, n_imbal, p_imbal, depletion, centroid_lat, centroid_lon)%>%
  bind_cols(map_dfc(unique_depletion_values, ~ as.integer(median_data_rfr_2$depletion == .x)) %>%
              set_names(unique_depletion_values))


# insert table percentages of depletion zones: 
table_percentage_depletion_zones<-median_data_rfr_2%>%
  group_by(depletion)%>%
  summarize(occurrences = n())%>%
  mutate(percent = occurrences/sum(occurrences)*100)
table_percentage_depletion_zones

# save file containing depletion zones and HYBAS_IDs:

HYBAS_depletion_zones <- median_data_rfr_2%>%
  select(c(HYBAS_ID, depletion))

write.csv(HYBAS_depletion_zones, "../output_data/CNP_data_catchments/depletion_zones_HYBAS_IDs.csv")



frequency_counts <- median_data_rfr_2 %>%
  count(depletion)

# Reorder the levels of the depletion variable based on the frequency counts
median_data_rfr_2$depletion <- factor(median_data_rfr_2$depletion, levels = frequency_counts$depletion[order(frequency_counts$n)])


percentage_depletion <- ggplot(median_data_rfr_2, aes(depletion)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), colour = depletion, fill = depletion)) + 
  scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
  scale_color_manual(values = my_colors2) +
  scale_fill_manual(values = my_colors2) +
  ylab("percent of catchments")+
  xlab("depletion")+
  theme_minimal()+
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 18), legend.position = "none") +
  #labs(tag = 'b')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))
percentage_depletion

# save figure:
ggsave(percentage_depletion,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/percentage_depleton_zones.svg",
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 400)




plot_combined <- ggplot() +
  annotation_custom(ggplotGrob(ternvrede_n_rfr_dep_zones), xmin=0, xmax=0.45, ymin=-Inf, ymax=Inf) +
  annotation_custom(ggplotGrob(percentage_depletion), xmin=0.45, xmax=Inf, ymin=0.0, ymax=0.80)+ 
  #coord_fixed() + 
  theme_void()
# showw combined plot:
plot_combined


# save plot: 
ggsave(plot_combined,
       device = NULL,
       filename = 'combi_dep_zones_bioav_10_08_perc_rOC_2.png',
       path = out_folder)




# worldmap depletion zones: 
# Create a ggplot object and plot the map with country borders
p_depl_world <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2, aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 0.4, size = 2.2) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  #xlab("longitude") +
  #ylab("latitude")+
  labs(tag = "b")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = "none", legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))
  
p_depl_world

# save plot: 
ggsave(p_depl_world,
       device = NULL,
       filename = 'worldmap_depletion_zones_bioav_10_08.png',
       path = out_folder)

# save figure as svg file:
ggsave(p_depl_world,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/depletion_zones_worldmap.svg",
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 400)



# USA: 
p_depl_us <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-140, -55), ylim = c(26, 52), expand = TRUE)+
  geom_point(data = median_data_rfr_2, aes(x = centroid_lon, y = centroid_lat, colour = depletion), size = 3) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  #xlab("longitude") +
  #ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 14),legend.position = "none", plot.tag = element_text(face = "bold", size = 14, hjust = 0, vjust = 1))

p_depl_us


# save figure as svg file:
ggsave(p_depl_us,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/depleton_zones_us_axis_labels.svg",
       width = 12,
       height = 10,
       unit = "cm",
       dpi = 400)








# Europe: 
p_depl_eu <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-10, 25), ylim = c(40, 64), expand = TRUE)+
  geom_point(data = median_data_rfr_2, aes(x = centroid_lon, y = centroid_lat, colour = depletion), size = 3) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 14), legend.position = "none", plot.tag = element_text(face = "bold", size = 14, hjust = 0, vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_depl_eu


# save figure as svg file:
ggsave(p_depl_eu,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/depletion_zones_eu_axis_labels.svg",
       width = 12,
       height = 12,
       unit = "cm",
       dpi = 400)









# Europe: 
p_depl_us2 <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-160, -50), ylim = c(20, 69), expand = TRUE)+
  geom_point(data = median_data_rfr_2, aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 0.4, size = 2) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text = element_text(size = 14), legend.position = "none", plot.tag = element_text(face = "bold", size = 14, hjust = 0, vjust = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_depl_us2




######  WORLDMAPS FOR EACH DEPLETION ZONE ON ternary plot: ####

# worldmap P and N depletion: 
# Create a ggplot object and plot the map with country borders
pand_n_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(depletion == 'rP & rN'), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 1, size = 3) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = c(0.10, 0.2), legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

pand_n_depl



# save figure as svg file:
ggsave(pand_n_depl,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/worldmap_P_and_N_depletion.svg",
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 400)






# worldmap OC and N depletion: 
# Create a ggplot object and plot the map with country borders
oc_and_n_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(depletion == 'rOC & rN'), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 1, size = 3) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = c(0.10, 0.2), legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

oc_and_n_depl



# save figure as svg file:
ggsave(oc_and_n_depl,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/worldmap_OC_and_N_depletion.svg",
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 400)



# worldmap none depletion: 
# Create a ggplot object and plot the map with country borders
none_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(depletion == 'none'), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 1, size = 3) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = c(0.10, 0.2), legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

none_depl




# save figure as svg file:
ggsave(none_depl,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/worldmap_none_depletion.svg",
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 400)





# worldmap P and OC depletion: 
# Create a ggplot object and plot the map with country borders
p_and_oc_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(depletion == 'rP & rOC'), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 1, size = 3) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = c(0.10, 0.2), legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

p_and_oc_depl




# save figure as svg file:
ggsave(p_and_oc_depl,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/worldmap_P_and_OC_depletion.svg",
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 400)



# worldmap P depletion: 
# Create a ggplot object and plot the map with country borders
p_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(depletion == 'rP'), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 1, size = 3) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = c(0.10, 0.2), legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

p_depl




# save figure as svg file:
ggsave(p_depl,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/worldmap_P_depletion.svg",
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 400)




# worldmap N depletion: 
# Create a ggplot object and plot the map with country borders
n_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(depletion == 'rN'), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 1, size = 3) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = c(0.10, 0.2), legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

n_depl




# save figure as svg file:
ggsave(n_depl,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/worldmap_N_depletion.svg",
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 400)





# worldmap OC depletion: 
# Create a ggplot object and plot the map with country borders
oc_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(depletion == 'rOC'), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 1, size = 3) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = c(0.10, 0.2), legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

oc_depl


# save figure as svg file:
ggsave(oc_depl,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/worldmap_OC_depletion.svg",
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 400)





#### Distribution of the depletion zones per continent: ##########


##### Distribution of depletion classes North America:

# Create a ggplot object and plot the map with country borders
north_america_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(centroid_lon < -60 & centroid_lat > 10), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 0.4, size = 2) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = "top", legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

north_america_depl


north_america_dist <- ggplot(median_data_rfr_2%>%filter(centroid_lon < -60 & centroid_lat > 10), aes(depletion)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), colour = depletion, fill = depletion),
           colour = "black",
           size = 0.5) +
  #geom_bar(aes(y = (..count..)/sum(..count..)), colour = "black", fill = "black") +
  scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
  scale_color_manual(values = my_colors2) +
  scale_fill_manual(values = my_colors2) +
  ylab("Percent of catchments")+
  xlab("Depletion") +
  theme_classic(base_size = 18) +
  #theme_minimal()+
  theme(axis.title.y = element_text(size = 20), axis.title.x = element_blank(), axis.text = element_text(size = 20), legend.position = "none") +
 # labs(tag = 'b')+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),plot.tag = element_text(face = "bold", size = 20, hjust = 0, vjust = 1)) +
  ggtitle(paste0('North America n = ',nrow(median_data_rfr_2%>%filter(centroid_lon < -60 & centroid_lat > 10))))
north_america_dist

# save figure:
ggsave(north_america_dist,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/barplot_depletion_zones_N_America.svg",
       width = 30,
       height = 20,
       unit = "cm",
       dpi = 400)



##### Distribution of depletion classes South America:

# Create a ggplot object and plot the map with country borders
south_america_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(centroid_lon < -20 & centroid_lat < 10), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 0.4, size = 2) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = "top", legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

south_america_depl


south_america_dist <- ggplot(median_data_rfr_2%>%filter(centroid_lon < -20 & centroid_lat < 10), aes(depletion)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), colour = depletion, fill = depletion),
           colour = "black",
           size = 0.5) + 
  #geom_bar(aes(y = (..count..)/sum(..count..)), colour = "black", fill = "black") +
  scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
  scale_color_manual(values = my_colors2) +
  scale_fill_manual(values = my_colors2) +
  ylab("Percent of catchments")+
  xlab("Depletion") +
  theme_classic(base_size = 18) +
  #theme_minimal()+
  theme(axis.title.y = element_text(size = 20), axis.title.x = element_blank(), axis.text = element_text(size = 20), legend.position = "none") +
  # labs(tag = 'b')+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),plot.tag = element_text(face = "bold", size = 20, hjust = 0, vjust = 1)) +
  ggtitle(paste0('South America n = ', nrow(median_data_rfr_2%>%filter(centroid_lon < -20 & centroid_lat < 10))))
south_america_dist

# save figure:
ggsave(south_america_dist,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/barplot_depletion_zones_S_America.svg",
       width = 30,
       height = 20,
       unit = "cm",
       dpi = 400)






##### Distribution of depletion classes Europe:

# Create a ggplot object and plot the map with country borders
europe_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(centroid_lon > -30 & centroid_lon < 50 & centroid_lat > 10), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 0.4, size = 2) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = "top", legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

europe_depl


europe_dist <- ggplot(median_data_rfr_2%>%filter(centroid_lon > -30 & centroid_lon < 50 & centroid_lat > 10), aes(depletion)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), colour = depletion, fill = depletion),
           colour = "black",
           size = 0.5) + 
  #geom_bar(aes(y = (..count..)/sum(..count..)), colour = "black", fill = "black") +
  scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
  scale_color_manual(values = my_colors2) +
  scale_fill_manual(values = my_colors2) +
  ylab("Percent of catchments")+
  xlab("Depletion") +
  theme_classic(base_size = 18) +
  #theme_minimal()+
  theme(axis.title.y = element_text(size = 20), axis.title.x = element_blank(), axis.text = element_text(size = 20), legend.position = "none") +
  # labs(tag = 'b')+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),plot.tag = element_text(face = "bold", size = 20, hjust = 0, vjust = 1)) +
  ggtitle(paste0('Europe n = ', nrow(median_data_rfr_2%>%filter(centroid_lon > -30 & centroid_lon < 50 & centroid_lat > 10))))
europe_dist


# save figure:
ggsave(europe_dist,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/barplot_depletion_zones_Europe.svg",
       width = 30,
       height = 20,
       unit = "cm",
       dpi = 400)




#### Global dataset: 

global_dist <- ggplot(median_data_rfr_2, aes(depletion)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), colour = depletion, fill = depletion),
           colour = "black",
           size = 0.5) + 
  #geom_bar(aes(y = (..count..)/sum(..count..)), colour = "black", fill = "black") +
  scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
  scale_color_manual(values = my_colors2) +
  scale_fill_manual(values = my_colors2) +
  ylab("Percent of catchments")+
  xlab("Depletion") +
  theme_classic(base_size = 18) +
  #theme_minimal()+
  theme(axis.title.y = element_text(size = 20), axis.title.x = element_blank(), axis.text = element_text(size = 20), legend.position = "none") +
  # labs(tag = 'b')+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),plot.tag = element_text(face = "bold", size = 20, hjust = 0, vjust = 1)) +
  ggtitle(paste0('Global n = ', nrow(median_data_rfr_2)))
global_dist


# save figure:
ggsave(global_dist,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/barplot_depletion_zones_Global.svg",
       width = 30,
       height = 20,
       unit = "cm",
       dpi = 400)





##### Distribution of depletion classes Africa:

# Create a ggplot object and plot the map with country borders
kenia_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(centroid_lon > -30 & centroid_lon < 60 & centroid_lat < 40), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 0.4, size = 2) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 20), legend.position = "top", legend.text = element_text(size = 20), plot.tag = element_text(face = "bold", size = 20, hjust = 0, vjust = 1))

kenia_depl





kenia_dist <- ggplot(median_data_rfr_2%>%filter(centroid_lon > -30 & centroid_lon < 60 & centroid_lat < 40), aes(depletion)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), colour = depletion, fill = depletion),
           colour = "black",
           size = 0.5) + 
  #geom_bar(aes(y = (..count..)/sum(..count..)), colour = "black", fill = "black") +
  scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
  scale_x_discrete(drop = FALSE) +
  scale_color_manual(values = my_colors2) +
  scale_fill_manual(values = my_colors2) +
  ylab("Percent of catchments")+
  xlab("Depletion") +
  theme_classic(base_size = 18) +
  #theme_minimal()+
  theme(axis.title.y = element_text(size = 20), axis.title.x = element_blank(), axis.text = element_text(size = 20), legend.position = "none") +
  # labs(tag = 'b')+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),plot.tag = element_text(face = "bold", size = 20, hjust = 0, vjust = 1)) +
  ggtitle(paste0('Kenia n = ', nrow(median_data_rfr_2%>%filter(centroid_lon > -30 & centroid_lon < 60 & centroid_lat < 40))))
kenia_dist

# save figure:
ggsave(kenia_dist,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/barplot_depletion_zones_Kenia.svg",
       width = 30,
       height = 20,
       unit = "cm",
       dpi = 400)



##### Distribution of depletion classes Arctic delta:

# Create a ggplot object and plot the map with country borders
arctic_depl <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey") +  
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)+
  geom_point(data = median_data_rfr_2%>%filter(centroid_lon > 50 & centroid_lat >50), aes(x = centroid_lon, y = centroid_lat, colour = depletion), alpha = 0.4, size = 2) +
  scale_color_manual(values = my_colors2) +
  theme_minimal() +
  xlab("longitude") +
  ylab("latitude")+
  #labs(tag = "c")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 18), legend.position = "top", legend.text = element_text(size = 18), plot.tag = element_text(face = "bold", size = 18, hjust = 0, vjust = 1))

arctic_depl


arctic_dist <- ggplot(median_data_rfr_2%>%filter(centroid_lon > 50 & centroid_lat > 40), aes(depletion)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), colour = depletion, fill = depletion),
           colour = "black",
           size = 0.5) + 
  #geom_bar(aes(y = (..count..)/sum(..count..)), colour = "black", fill = "black") + 
  scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
  scale_x_discrete(drop = FALSE) +
  scale_color_manual(values = my_colors2) +
  scale_fill_manual(values = my_colors2) +
  ylab("Percent of catchments")+
  xlab("Depletion") +
  theme_classic(base_size = 18) +
  #theme_minimal()+
  theme(axis.title.y = element_text(size = 20), axis.title.x = element_blank(), axis.text = element_text(size = 20), legend.position = "none") +
  # labs(tag = 'b')+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),plot.tag = element_text(face = "bold", size = 20, hjust = 0, vjust = 1)) +
  ggtitle(paste0('Arctic deltas n = ', nrow(median_data_rfr_2%>%filter(centroid_lon > 50 & centroid_lat > 40))))
arctic_dist


# save figure:
ggsave(arctic_dist,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/barplot_depletion_zones_Actic_Deltas.svg",
       width = 30,
       height = 20,
       unit = "cm",
       dpi = 400)



# arrange barplots: North, south, europe and global distribution:

# north without x-axis labels

a1<-north_america_dist +theme_classic(base_size = 35)+ theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 35), axis.title.y = element_text(size = 35), axis.title.x = element_blank(), legend.text = element_text(size = 35),   # Increase legend text size
                legend.title = element_blank(),
                legend.key.size = unit(1.8, "cm"),
                legend.position = "top",
                legend.direction = "horizontal") + 
  guides(fill = guide_legend(nrow = 1))
a1
a2<-europe_dist + 
  theme_classic(base_size = 35) +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        legend.text = element_text(size = 35),   # Increase legend text size
        legend.title = element_blank(),
        legend.key.size = unit(2, "cm"),
        legend.position = "top",
        legend.direction = "horizontal") + 
  guides(fill = guide_legend(nrow = 1))
a2

a3<-south_america_dist + 
  theme_classic(base_size = 35) +
  theme(axis.text = element_text(size = 35), axis.title.y = element_text(size = 35), axis.text.x =  element_blank(), axis.title.x = element_blank(),
        legend.text = element_text(size = 35),   # Increase legend text size
        legend.title = element_blank(),
        legend.key.size = unit(2, "cm"),
        legend.position = "top",
        legend.direction = "horizontal") + 
  guides(fill = guide_legend(nrow = 1))
a3
a4<-global_dist + 
  theme_classic(base_size = 35) +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        legend.text = element_text(size = 35),   # Increase legend text size
        legend.title = element_blank(),
        legend.key.size = unit(2, "cm"),
        legend.position = "top",
        legend.direction = "horizontal") + 
  guides(fill = guide_legend(nrow = 1))

a4
arranged <- ggarrange(a1, a2, a3, a4,
                      ncol = 2, nrow = 2,
                      common.legend = TRUE,
                      legend = "bottom")
arranged

# add label c to the topleft corner:
arranged_label <- annotate_figure(arranged,
                                  top = text_grob("c", size = 40, face = "bold", hjust = 0, vjust = 1, x = 0.02))
arranged_label

# save figure:
ggsave(arranged_label,
       device = NULL,
       filename = "../figures/ternary_plots_CNP_basins/barplot_depletion_zones_SA_NA_EU_GL.svg",
       width = 60,
       height = 40,
       unit = "cm",
       dpi = 500)






