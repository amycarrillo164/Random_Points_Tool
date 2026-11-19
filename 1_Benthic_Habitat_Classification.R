# Title: Benthic Habitat Classification - NCCOS & GAO - 2025 - V1.1
# Date: 02-21-2025
# Author: Amy Carrillo
# Version: 1.2

# A. Backgrounf
## Output Layer:** NCCOS_Hab_AllMHI_Edits_10

# Notes:
# Part I of the code is a copy of the benthic habitat classification code found in:
# - C_Habitat > Benthic_Habitat > V1 > 1a_classification.R
# Part II involves and internal process and layer with editing certain unknowns. 

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

## NCCOS Benthic Habitat Layer 
NCCOS_Hab_AllMHI_Merge <- st_read(file.path(user, "Donovan_Lab_GIS/Drivers/C_Habitat/Benthic_Habitat/V1/OTP_Lecky_V1.0/NCCOS_Hab_AllMHI_Merge/NCCOS_Hab_AllMHI_Merge.shp"))

## Benthic Habitat Layer with Edits (Sensitive)
### Note: Do not share this layer. This layer is for internal use in the interim and not based on "fact."
MD_JR_unknown <- st_read("raw_data/MD_JR_unknown_manual_edits_oahu_2026/MD_JR_unknown_manual_edits_oahu_2026.shp")

#==========================================================================================================#
# D. Methods

## Part I: Creating New fields in NCCOS Data
## 1.02	Added two new fields “Target Class” - text field and “Target_Class_Num” for the numeric description.
# Add new fields: "Target Class" and "Target_Class_Num"
NCCOS_Hab_AllMHI_Edits <- NCCOS_Hab_AllMHI_Merge %>%
  mutate(
    Target_Class = NA_character_,  # Placeholder for text field
    Target_Class_Num = NA_real_      # Placeholder for numeric field
  )

## 1.04	Used field calculator to change “target_class” to “coral” for these rows.
### a.	*Spur and Groove > Coralline algae should be included.
### b.	** All corals were included, those that were 10-50 percent cover and those that were higher
NCCOS_Hab_AllMHI_Edits_1 <- NCCOS_Hab_AllMHI_Edits %>%
  mutate(
    Target_Class = ifelse(M_COVER == "Coral" | 
                            (D_STRUCT == "Spur and Groove" & M_COVER == "Coralline Algae"), # 2025 Benthic Habitat Change
                          "coral", 
                          Target_Class))

## 1.05	Run select by attribute for new Target_Class > Pavement. Where M_COVER does not include Coral. And, where D_STRUCT includes the values 'Pavement' and 'Pavement with Sand Channels'
NCCOS_Hab_AllMHI_Edits_2 <- NCCOS_Hab_AllMHI_Edits_1 %>%
  mutate(
    Target_Class = ifelse( 
      !grepl("coral", Target_Class) & #grepl returns TRUE if a specified pattern is found in a string and FALSE otherwise.
        D_STRUCT %in% c("Pavement", "Pavement with Sand Channels"), 
      "pavement", Target_Class))  # Retain existing value if conditions are not met

## 1.06	Run “Select by attribute” to get rock/boulder cells. Excluded polygons already selected as coral or pavement. 
NCCOS_Hab_AllMHI_Edits_3 <- NCCOS_Hab_AllMHI_Edits_2 %>%
  mutate(
    Target_Class = ifelse( 
      !grepl("coral|pavement", Target_Class, ignore.case = TRUE) &  
        D_STRUCT %in% "Rock/Boulder", 
      "rock/boulder", Target_Class))

## 1.07 Run 4th select by attribute to turn mud/sand pixels into “Soft” classification. Excluded polygons already selected as coral, pavement, and rock/boulder.
NCCOS_Hab_AllMHI_Edits_4 <- NCCOS_Hab_AllMHI_Edits_3 %>%
  mutate(
    Target_Class = ifelse( 
      !grepl("coral|pavement|rock/boulder", Target_Class, ignore.case = TRUE) &  
        D_STRUCT %in% c("Mud", "Sand"), 
      "soft", Target_Class))

## 1.08 Run 5th select by attribute. Where D Struct includes the values “Individual patch reef, pavement with sand channels, rubble, scattered coral/rock, spur and groove, aggregated patch reef, aggregated reef.” Also, “Target_class” does not include the values “Boulder, coral, pavement”.
NCCOS_Hab_AllMHI_Edits_5 <- NCCOS_Hab_AllMHI_Edits_4 %>%
  mutate(
    Target_Class = ifelse( 
      !grepl("coral|pavement|rock/boulder|soft", Target_Class, ignore.case = TRUE) &  
        D_STRUCT %in% c("Individual Patch Reef", "Rubble", "Scattered Coral/Rock", "Spur and Groove", "Aggregated Patch Reef", "Aggregate Reef"), 
      "other_hard", Target_Class))

## 1.09 Run 6th select by attribute. Where D Struct includes the values "Artificial."  (This category isn't included in 2022_Benthic_Habitat Rachel layer. What happens to these cells needs to be further reviewed by Mary).
NCCOS_Hab_AllMHI_Edits_6 <- NCCOS_Hab_AllMHI_Edits_5 %>%
  mutate(
    Target_Class = ifelse( 
      !grepl("coral|pavement|rock/boulder|soft|other_hard", Target_Class, ignore.case = TRUE) &  
        D_STRUCT == "Artificial", 
      "artificial", Target_Class))

## 1.10 Run 7th select by attribute. Where D Struct includes the values "land." (This category isn't included in 2022_Benthic_Habitat Rachel layer. This category is included so that we can manually remove all the land cells, because the mask alone doesn't catch/mask them all.)
NCCOS_Hab_AllMHI_Edits_7 <- NCCOS_Hab_AllMHI_Edits_6 %>%
  mutate(
    Target_Class = ifelse( 
      !grepl("coral|pavement|rock/boulder|soft|other_hard|artificial", Target_Class, ignore.case = TRUE) &  
        D_STRUCT == "Land", 
      "land", Target_Class))

## 1.11 Everything else will be classified as other_hard_unknown. In the NCCOS data columns, these exist as "unknown". (This category isn't included in 2022_Benthic_Habitat Rachel layer).
NCCOS_Hab_AllMHI_Edits_8 <- NCCOS_Hab_AllMHI_Edits_7 %>%
  mutate(
    Target_Class = ifelse( 
      !grepl("coral|pavement|rock/boulder|soft|other_hard|land|artificial", Target_Class, ignore.case = TRUE),
      "other_hard_unknown", Target_Class))

# ### What's included in "other_hard_unknown"? Unknown
# other_hard_unknown_table <- NCCOS_Hab_AllMHI_Edits_8 %>%
#   filter(Target_Class == "other_hard_unknown")
# unique(other_hard_unknown_table$M_STRUCT)


## 1.13	Next, use select by attribute to calculate the “Target_Class_Num” field to set priority. Used the following numbers for each classification. 
###	Target HIMARC habitat classes: (priority)
### 	(8) coral
### 	(7) pavement
### 	(6) rock/boulder
### 	(5) other_hard
### 	(4) soft
###   (3) artificial    #Additional category. Will be revied and replaced with the other final Benthic Habitat categories.
###   (2) land    #Additional category. Not included in Rachel's layer. It will be masked and then removed in the final output.
###   (1) other_hard_unknown    #Additional *final Benthic Habitat Category. 

### Reclassify
NCCOS_Hab_AllMHI_Edits_9 <- NCCOS_Hab_AllMHI_Edits_8 %>%
  mutate(
    Target_Class_Num = case_when(
      Target_Class == "coral" ~ 8,
      Target_Class == "pavement" ~ 7,
      Target_Class == "rock/boulder" ~ 6,
      Target_Class == "other_hard" ~ 5,
      Target_Class == "soft" ~ 4,
      Target_Class == "artificial" ~ 3,
      Target_Class == "land" ~ 2,
      Target_Class == "other_hard_unknown" ~ 1))

## 1.14 Plot to verify
plot(
  st_geometry(NCCOS_Hab_AllMHI_Edits_9),       # Geometry of the polygons
  col = NCCOS_Hab_AllMHI_Edits_9$Target_Class_Num,  # Use the attribute for coloring
  main = "Target Class Map",
  axes = TRUE,
  xlim = c(592000, 642000),       # X-axis limits in UTM
  ylim = c(2350000, 2400000)      # Y-axis limits in UTM
)

## 1.16 The polygon IDs are not unique identifiers. So we need to create new unique polygon IDs
### Proof of claim
nccos_polygonIDs <- data.frame(table(NCCOS_Hab_AllMHI_Edits_9$POLYGONID))
nccos_polygonIDs[nccos_polygonIDs$Freq > 1,]
### Create unique polygon ID column
NCCOS_Hab_AllMHI_Edits_10 <- NCCOS_Hab_AllMHI_Edits_9 %>%
  rename(NCCOS_POLYGONID = POLYGONID) %>% 
  mutate(Unique_PolygonIDs = row_number())

#=============================================================================#
# Part II: Convert "unknowns" in Oahu into "unknown hard" and "unknown soft" via group review process (Mary, Jake, and Amy)

## 2.1 Fix column name of the unknown layer (unknown hard and unknown soft descriptions)
MD_JR_unknown_info <- MD_JR_unknown %>%
  rename(Unique_PolygonIDs = Unique_Pol,
         NCCOS_POLYGONID = NCCOS_POLY,
         Target_Class = Target_Cla,
         Target_Class_Num = Target_C_1) %>%
  dplyr::select(-c(CumImpact_, CumImpact1, HIMARC_Hab, Shape_Leng, Shape_Area))

## 2.2 Create a list of the unknown polygon IDs that have been edited in Oahu (from unknown layer)
MD_JR_unknown_polyID <- MD_JR_unknown_info$Unique_PolygonIDs

## 2.3 Filter out that list of polygon IDs from our benthic habitat layer
NCCOS_Hab_AllMHI_Edits_10_unknownremoval <- NCCOS_Hab_AllMHI_Edits_10 %>%
  filter(!Unique_PolygonIDs %in% MD_JR_unknown_polyID)

## 2.4 Add back the soft unknowns and hard unknowns (for only Oahu) from the "unknown layer"
NCCOS_Hab_AllMHI_Edits_10_MDJRUnknowns <- NCCOS_Hab_AllMHI_Edits_10_unknownremoval %>%
  bind_rows(MD_JR_unknown_info) %>%
  st_make_valid() %>%
  rename(
    NCCOS_ID = NCCOS_POLYGONID,
    UPOLY_ID = Unique_PolygonIDs,
    ID = UNIQUEID)

## 2.5 Export
### Export Geopackage
st_write(
  NCCOS_Hab_AllMHI_Edits_10_MDJRUnknowns, 
  "scratch_data/scratch_data_1.2.gpkg",
  layer = "1_NCCOS_Hab_AllMHI_Edits_10_MDJRUnknowns",
  delete_layer = TRUE)

