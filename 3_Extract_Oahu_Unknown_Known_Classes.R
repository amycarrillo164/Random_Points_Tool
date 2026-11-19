# Title: DAR Oahu Power - Extract Known and Unknowns - 2026
# Date: 11/18/26
# Author: Amy Carrillo
# Version: 1.2

#==========================================================================================================#
# A. Background   -----------------------------------------------------------------------------------------
## In "2_depth_and_rogosity_classes" we only remove "soft" - sand and mud. However, this leads to the inclusion of "unknowns."
## For monitoring purposes, We need to know which areas exactly are hard bottom.

# Purpose
## To build two layers: 
## 1) Known hard-bottom with the appropriate substrata within the 30m depth contour
## 2) Unknown with the appropriate substrata within the 30m depth contour

#==========================================================================================================#
# B. Packages/Libraries
library(sf)
library(dplyr)
library(raster)
library(terra)
library(tidyr)
library(mapview)
library(stringr)

#==========================================================================================================#
# C. Data
## User path (substitute with computer user)
getwd()
user <- "C:/Users/Amy/Donovan Lab Dropbox/Donovan Lab Team Folder"
my_directory <- file.path(user, "Donovan_Lab_Projects/DAR_oahu_power")
setwd(my_directory)

oahu_dar_management_classified_polys_NoSoft <- st_read("scratch_data/scratch_data_1.2.gpkg", layer = "2_oahu_dar_management_classified_polys_NoSoft")

unknown_swath <- st_read("scratch_data/shapefiles/nccos_oahu_unknown_swath_large/nccos_oahu_unknown_swath_large.shp")
contour_30m <- st_read("scratch_data/shapefiles/MHI_bathy_HMRGv19filled_30mContour_JoeyLecky/MHI_bathy_HMRGv19filled_30mContour.shp")
benthic_habitat_rev <- st_read("scratch_data/geopackage/power_analysis_scratch.gpkg", layer = "1_NCCOS_Hab_AllMHI_Edits_10_MDJRUnknowns")

oahu_sites_v1.1 <- st_read("output_data/Oahu_Sites_v1.1_2026.gpkg", layer = "oahu_sites")

#==========================================================================================================#
# Part I: Prep --------------------------------------------------------------------------
#==========================================================================================================#
# 1.01 Verify crs
st_crs(oahu_dar_management_classified_polys_NoSoft)$epsg
st_crs(unknown_swath)$epsg
st_crs(contour_30m)$epsg
st_crs(oahu_sites_v1.1)$epsg

# 1.02 30m Depth Contour
## Convert the contour linestring into a polygon
contour_30m_oahu <- contour_30m %>%
  dplyr::filter(Id == 2007) %>%
  st_make_valid() %>%
  st_cast("POLYGON")

#==========================================================================================================#
# Part 2: Build layer: unknown -----------------------------
#==========================================================================================================#
## 2.01 Mask unknown swath to the 30m depth contour
unknown_swath_30m <- st_intersection(unknown_swath, contour_30m_oahu)

## 2.02. Unknown classes swath (before the 30m depth contour)
oahu_swath_unknown <- oahu_dar_management_classified_polys_NoSoft %>%
  sf::st_make_valid() %>%
  st_intersection(st_geometry(unknown_swath_30m)) %>%
  sf::st_make_valid() %>%
  filter(!st_is_empty(.))
mapview(oahu_swath_unknown, zcol = "combined_class")

## 2.03. JR + MD Unknowns
mdjr_unknown <- benthic_habitat_rev %>% 
  dplyr::filter(Target_Class == "unknown")
mdjr_unknown2 <- oahu_dar_management_classified_polys_NoSoft %>%
  sf::st_make_valid() %>%
  st_intersection(st_geometry(mdjr_unknown)) %>%
  sf::st_make_valid() %>%
  filter(!st_is_empty(.))
mapview(mdjr_unknown2, zcol = "combined_class")

## 2.04. Combine Unknowns
oahu_classes_unknown <- dplyr::bind_rows(oahu_swath_unknown, mdjr_unknown2)

## 2.05 Include Site Information
oahu_classes_unknown_siteInfo <- st_intersection(oahu_classes_unknown, oahu_sites_v1.1)
table(st_geometry_type(oahu_classes_unknown_siteInfo))

## 2.06 Group by site name and combined_class
oahu_classes_unknown_siteInfo_groups <- oahu_classes_unknown_siteInfo %>%
  st_make_valid() %>%
  ### Slivers: small, narrow, unwanted polygon features or tiny empty gaps that appear along the shared borders of polygons)
  filter(as.numeric(st_area(.)) > 1) %>%    
  group_by(site_name, site_code, site_label, combined_class) %>%
  summarise(geometry = st_union(geom), .groups = "drop") %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%                  # then standardize
  mutate(area_m2  = as.numeric(st_area(.)))          # square meters
table(st_geometry_type(oahu_classes_unknown_siteInfo_groups))

## 2.07
st_write(oahu_classes_unknown_siteInfo_groups,
         "scratch_data/scratch_data_1.2.gpkg",
         layer = "3_Oahu_unknown_30m",
         append = FALSE)
st_write(oahu_classes_unknown_siteInfo_groups,
         "output_data/OahuEcoregionsStrata_HIMARC_v1.2_2026.gpkg",
         layer = "Oahu_unknown_30m",
         append = FALSE)


#==========================================================================================================#
# Part 3: Build layer: Known Hard Bottom -----------------------------
#==========================================================================================================#
## 3.01 Mask benthic habitat classes layer to 30m depth contour
oahu_dar_management_classified_polys_NoSoft_30m <- st_intersection(oahu_dar_management_classified_polys_NoSoft, contour_30m_oahu)

## 3.02. Known hard bottom
### Collect unknown areas
erasers <- st_union(st_geometry(unknown_swath_30m), st_geometry(mdjr_unknown2))

### Remove unknown areas from dataset
oahu_classes_knownHardBottom <- oahu_dar_management_classified_polys_NoSoft_30m %>%
  sf::st_make_valid() %>%
  sf::st_difference(st_union(erasers)) %>% # Unioning here ensures a clean 'cut'
  sf::st_make_valid() %>%
  filter(!st_is_empty(.)) 
mapview(oahu_classes_knownHardBottom, zcol = "combined_class")
table(st_geometry_type(oahu_classes_knownHardBottom))

## Turn to multipolygons
oahu_classes_knownHardBottom_multipolys <- oahu_classes_knownHardBottom %>%
  #filter(st_is(geom, c("POLYGON", "MULTIPOLYGON"))) %>%   # drop the 3 lines
  st_make_valid() %>%
  group_by(combined_class) %>%
  summarise(geometry = st_union(geom), .groups = "drop") %>%
  st_cast("MULTIPOLYGON")
table(st_geometry_type(oahu_classes_knownHardBottom_multipolys))

## 3.03 Include Site Information
## Intersect with sites & keep site information attributes
oahu_classes_knownHardBottom_siteInfo <- st_intersection(oahu_classes_knownHardBottom_multipolys, oahu_sites_v1.1)
table(st_geometry_type(oahu_classes_knownHardBottom_siteInfo))

## After intersection, we need to turn shapes into multipolygons
oahu_classes_knownHardBottom_multipolys <- oahu_classes_knownHardBottom_siteInfo %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON") %>%
  st_cast("MULTIPOLYGON")
table(st_geometry_type(oahu_classes_knownHardBottom_multipolys))

## 3.04 Group by site name and combined_class
oahu_classes_knownHardBottom_siteInfo_groups <- oahu_classes_knownHardBottom_multipolys %>%
  st_make_valid() %>%
  ### Remove slivers 
  ### Slivers: small, narrow, unwanted polygon features or tiny empty gaps that appear along the shared borders of polygons)
  filter(as.numeric(st_area(.)) > 1) %>%                  
  group_by(site_name, site_code, site_label, combined_class) %>%
  summarise(do_union = TRUE, .groups = "drop") %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area_m2  = as.numeric(st_area(.)))          # square meters
table(st_geometry_type(oahu_classes_knownHardBottom_siteInfo_groups))

## 3.05 Export
st_write(oahu_classes_knownHardBottom_siteInfo_groups,
         "scratch_data/scratch_data_1.2.gpkg",
         layer = "3_Oahu_knownhardbottom_30m",
         append = FALSE)
st_write(oahu_classes_knownHardBottom_siteInfo_groups,
         "output_data/OahuEcoregionsStrata_HIMARC_v1.2_2026.gpkg",
         layer = "Oahu_knownhardbottom_30m",
         append = FALSE)
