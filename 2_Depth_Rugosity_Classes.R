# Title: DAR Oahu Power - Depth and Rugosity Classes / Strata - 2025
# Date: 12/04/25
# Author: Amy Carrillo
# Version: 1.2

#==========================================================================================================#
# Outline
# A. Background
# B. Packages/Libraries
# C. Data
# D. Methods
#  I.   Create Groups for Depth
#  II.  Create Groups for Rugosity
#  III. Depth and Rugosity Categories Polygon Layer
#  IV.  Remove soft bottom

#==========================================================================================================#
# A. Background   -----------------------------------------------------------------------------------------
#==========================================================================================================#
## - Classifies Oahu into strata based on depth and rugosity classes.

## Final Output Layer:
## oahu_dar_management_classified_polys_NoSoft.shp

#==========================================================================================================#
# B. Packages/Libraries ---------------------------------------------------------------------------------
#==========================================================================================================#
library(sf)
library(dplyr)
library(raster)
library(terra)
library(tidyr)
library(stringr)
library(mapview)
library(exactextractr)

#==========================================================================================================#
# C. Data   -----------------------------------------------------------------------------------------------
#==========================================================================================================#
## User path (substitute with computer user)
getwd()
user <- "C:/Users/Amy/Donovan Lab Dropbox/Donovan Lab Team Folder"
my_directory <- file.path(user, "Donovan_Lab_Projects/DAR_oahu_power")
setwd(my_directory)

## Benthic Habitat
benthic_habitat_rev <- st_read("scratch_data/geopackage/power_analysis_scratch.gpkg", layer = "1_NCCOS_Hab_AllMHI_Edits_10_MDJRUnknowns")

## Depth & Rugosity HIMARC Rasters
depth <- rast(file.path(user, "Donovan_Lab_GIS/Drivers/Finalized/2004-2014/Habitat/Depth/26_Depth100m_FocMEAN60m_Mosaic_rank_ready.tif"))
rugosity <- rast(file.path(user, "Donovan_Lab_GIS/Drivers/Finalized/2004-2014/Habitat/Rugosity/27_Slope2_100m_FocMEAN60m_combined_transform_ready.tif"))

## Masks
himarc_mask <- rast(file.path(user, "Donovan_Lab_GIS/Drivers/0_Input_Data_and_Models/Common_2024/Finalized/Masks_2026/Mask_rasters/post2018_NoStreamsNoPH_NoHKM_AnalysisMask100m.tif"))
shoreline_mask <- st_read( file.path(user, "Donovan_Lab_GIS/Base-GIS/Hawaii/Shoreline/shoreline_CorrectedInlets_2022-23/shoreline_CorrectedInlets_2022-23.shp"))

## Misc.
moku <- st_read(file.path(user, "Donovan_Lab_GIS/Base-GIS/Hawaii/Moku/Moku_MaukatoMakai_5km/HIMARC_moku_2025.gpkg"), layer = "Moku_MaukaToMakai_5km_2025")

#==========================================================================================================#
# D. Methods  ----------------------------------------------------------------------------------------------
#==========================================================================================================#
## CRS Verification
st_crs(benthic_habitat_rev)$epsg
st_crs(shoreline_mask)$epsg
st_crs(depth)$epsg
st_crs(rugosity)$epsg
st_crs(himarc_mask)$epsg

#==========================================================================================================#
## Part I: Create Groups for Depth --------------------------------------------
#==========================================================================================================#
### 1.01 Convert meters to feet (for DAR purposes)
depth_ft <- depth * 3.28084

### 1.02 Create matrix of new depth categories
### Notes: left boundary is exclusive & right boundary is inclusive
depth_reclass_matrix <- matrix(c(
  -Inf, -60, 3,    # deep
  -60, -30, 2,     # mid
#  -30, 0, 1),        # shallow
### Update: Removing areas shallower than 7ft from surveys
   -30, -7, 1),        # shallow
  ncol = 3, byrow = TRUE)

### 1.03 Reclassify depth raster
depth_reclassified_numeric <- classify(
  depth_ft,
  depth_reclass_matrix)

### [Following Code is for Visualization Purposes]
### 1.04 Convert to categorical raster (if needed)
depth_reclassified <- depth_reclassified_numeric
levels(depth_reclassified) <- data.frame(
  id = 1:3,
  category = c("shallow", "mid", "deep"))

### 1.05 Visualize
plot(depth_reclassified)

#==========================================================================================================#
## Part II: Create Groups for Rugosity --------------------------------------------
#==========================================================================================================#
### 2.01 Create matrix for rugosity
### Note: (1) - contains categories of simple, moderate, and complex. Divided into 3 categories based on rugosity.
###       (2) - contains categories of simple and complex., Divided into 2 categories based on rugosity.
rug_reclass_matrix <- matrix(c(
  20, Inf, 3,     # complex 
  0, 20, 1),        # smooth 
  ncol = 3, byrow = TRUE)

### 2.02 Reclassify rugosity raster
rug_reclassified_numeric <- classify(
  rugosity,
  rug_reclass_matrix)

### [Following Code is for Visualization Purposes]
### 2.03 Convert to categorical raster (using categorical labels)
rug_reclassified <- rug_reclassified_numeric
levels(rug_reclassified) <- data.frame(
  id = c(1, 3),
  category = c("smooth", "complex"))

### 2.04 Visualize
plot(rug_reclassified)

#==========================================================================================================#
## Part III: Depth and Rugosity Categories Polygon Layer -------------------------------------------------
#==========================================================================================================#
### 3.01 Combine depth and rugosity into a raster stack
combined_raster_numeric <- c(depth_reclassified_numeric, rug_reclassified_numeric)
names(combined_raster_numeric) <- c("depth", "rugosity")

### 3.02 Preform raster calculation. This will create a single raster with unique IDs for each combination
### Note: This creates unique values like: 11, 12, 13
combined_raster_numeric10 <-  depth_reclassified + (rug_reclassified * 10)

### 3.03 Convert to polygons
combined_numeric_polygons <- as.polygons(combined_raster_numeric10, dissolve = TRUE)
combined_numeric_polygons_sf <- st_as_sf(combined_numeric_polygons)

### 3.04 Classify polygons into categories / labels
combo_polys <- combined_numeric_polygons_sf %>%
  mutate(
    combined_class = case_when(
      category == 11 ~ "shallow_smooth",
      category == 12 ~ "mid_smooth",
      category == 13 ~ "deep_smooth",
      category == 31 ~ "shallow_complex",
      category == 32 ~ "mid_complex",
      category == 33 ~ "deep_complex")) %>%
  dplyr::select(combined_class, geometry)
mapview(combo_polys)

### 3.05 Clip to Oahu
#### Create Oahu Poly
oahu_poly <- moku %>% 
  dplyr::filter(Island == "Oahu") %>%
  dplyr::summarise(geometry = st_union(geom))   # dissolve
#### Clip
combo_poly_oahu <-  st_intersection(combo_polys, oahu_poly)
mapview(combo_poly_oahu)

#==========================================================================================================#
## Part IV: Remove "soft" bottom -------------------------------------------------------------
#==========================================================================================================#
### 4.1 Filter HIMARC Benthic Habitat layer for "sand" and "unknown soft"
### Note: "unknown soft" were "unknown" polygons classified by Mary and Jake during 12/19/25 meeting.
soft_bottom <- benthic_habitat_rev %>% 
  dplyr::filter(Target_Class %in% c("soft", "unknown_soft", "land")) %>%
  dplyr::summarise(geometry = st_union(geom))
mapview(soft_bottom)

### 4.2 Clip out whatever is soft
combo_poly_oahu_NoSand <- st_difference(combo_poly_oahu, soft_bottom)
mapview(combo_poly_oahu_NoSand, zcol = "combined_class")

### 4.3 HIMARC Mask - removes inlets and areas we don't need
#### Rasterise Mask
himarc_mask10 <- himarc_mask == 10
himarc_mask_poly <- as.polygons(himarc_mask10)
himarc_mask_poly_sf <- st_as_sf(himarc_mask_poly)
mapview(himarc_mask_poly_sf)
#### Intersection
combo_poly_oahu_NoSand_mask <- st_intersection(combo_poly_oahu_NoSand, himarc_mask_poly_sf)
combo_poly_oahu_NoSand_mask2 <- st_collection_extract(combo_poly_oahu_NoSand_mask, "POLYGON")
combo_poly_oahu_NoSand_mask2 <- combo_poly_oahu_NoSand_mask2 %>% filter(!is.na(combined_class))
mapview(combo_poly_oahu_NoSand_mask2, zcol = "combined_class", map.types = "Esri.WorldGrayCanvas")

### 4.4 Shoreline mask 
combo_poly_oahu_NoSand_mask3 <- st_difference(
  st_make_valid(combo_poly_oahu_NoSand_mask2),
  st_union(st_make_valid(shoreline_mask)))
mapview(combo_poly_oahu_NoSand_mask3, zcol = "combined_class", map.types = "Esri.WorldGrayCanvas")

### 4.5 Export
#### Geopackage
st_write(combo_poly_oahu_NoSand_mask3, 
         "scratch_data/scratch_data_1.2.gpkg", 
         layer = "2_oahu_dar_management_classified_polys_NoSoft",
         delete_layer = TRUE)

