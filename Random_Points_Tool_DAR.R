# Title: Random Point Tool
# Date: 11/10/26
# Author: Amy Carrillo

#==========================================================================================================#
# A. Background   -----------------------------------------------------------------------------------------
## Generates random points within each polygon using a minimum distance constraint and exports each as a layer in a GeoPackage

## Notes:
## - Known: Spatstat not accepting these sites due to irregular shape and small area: 
##     -> Makua deep_smooth
##     -> KBay  mid_smooth
## - Unknown: Spatstat not accepting these sites due to irregular shape and small area: 
##     -> Waimanalo deep_complex
##     -> Waimanalo deep_smooth
##     -> Kualoa deep_smooth
#==========================================================================================================#
# B. Packages/Libraries
library(sf)
library(dplyr)
# if you don't have spatstat installed already, you may need to un-comment the following code:
#chooseCRANmirror
#Choose “0-Cloud [https]”
#install.packages("spatstat", dependencies = TRUE)
library(spatstat)

#==========================================================================================================#
# C. Data
## User path (substitute with computer user)
my_directory <- "C:/Users/Amy/Random_Points_Tool_Tutorial"
setwd(my_directory)

## Strata
oahu_strata_knownHardBottom <- st_read(
  "raw_data/OahuEcoRegionsStrata_HIMARC_v1.1_2026.gpkg", 
  layer = "Oahu_knownhardbottom_30m")

oahu_strata_unknown <- st_read(
  "raw_data/OahuEcoRegionsStrata_HIMARC_v1.1_2026.gpkg", 
  layer = "Oahu_unknown_30m")

#==========================================================================================================#
# D. Methods
# Prep -----------------------------------------------------------------------------------------
#==========================================================================================================#
## Verify crs
st_crs(oahu_strata_knownHardBottom)$epsg
st_crs(oahu_strata_unknown)$epsg

#==========================================================================================================#
# Part I: Generate Random Points Tool - Function ----------------------------------------------------------
#==========================================================================================================#
## 1.01 The function
generate_random_points <- function(layer, site_field, code_field, strata_field, num_points_field, max_num_points,
                                   min_distance, crs,type_known, out_gpkg) {
  
  out_dir <- dirname(out_gpkg)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  for (i in 1:nrow(layer)) {
    
    ## For layer naming, messaging purposes, and join back to dataframe
    poly      <- layer[i, ]
    site_name <- poly[[site_field]]
    site_code <- poly[[code_field]]
    strata_name <- poly[[strata_field]]
    
    ## Option A: Un-comment if your polygon layer has filled out the number of points wanted for each strata
    #num_pts  <- poly[[num_points_field]]
    
    ## Option B: Un-comment if you want to generate 100 random points (see parameters) for each strata
    num_pts   <- max_num_points
    
    # this if one strata doesn't hit max number of points, it keeps running
    tryCatch({
      
      ## Convert to spatstat window
      poly_owin <- as.owin(poly)
      
      ## Generate random points with minimum distance
      ### Note: rSSI - Simple Sequential Inhibition — points are placed one at a time at random locations, 
      ###       and any candidate closer than r to an existing point is rejected. The result is a random 
      ###       pattern with a guaranteed minimum spacing
      random_pts <- rSSI(
        r = min_distance,   # inhibition distance, the minimum allowed distance between any two points.
        ## Option C: Max number possible points. If you comment out the following, then it will just max out when it maxes out
        n = num_pts,        # number of points you want
        win = poly_owin)    # the window the points must fall inside
      message("Placed ", random_pts$n, " : ", site_name, " ", strata_name)
      
      ## Convert to sf
      random_pts_sf <- st_as_sf(
        data.frame(x = random_pts$x, y = random_pts$y),
        coords = c("x", "y"),
        crs = crs)
      
      ## Tag with site and strata name
      random_pts_sf[[site_field]]   <- site_name
      random_pts_sf[[code_field]]   <- site_code
      random_pts_sf[[strata_field]] <- strata_name
      
      # Write as individual layer into GeoPackage
      st_write(random_pts_sf,
               dsn   = out_gpkg,
               layer = paste0(site_name,"_",strata_name,"_", type_known, "_randompoints"),
               # Be careful un-commenting the following piece of code. It will re-write your already randomized points.
               delete_layer = TRUE,
               quiet = TRUE)
      message(paste0("Complete:", site_name, " ", strata_name))
      #}
      ## WARNING MESSAGE
    }, error = function(e) {
      print(paste0("Failed:", site_name, " ", strata_name, " - ", conditionMessage(e)))})
  }
}

## 1.02 Run the function
generate_random_points(
  layer            = oahu_strata_knownHardBottom,
  site_field       = "site_name",
  code_field       = "site_code",
  strata_field     = "combined_class",
  num_points_field = "num_point",       # if applicable...your point count field. This info is attached to your layer attributes
  max_num_points   = 100,               # if applicable... the max number of points you want to generate for each strata 
  min_distance     = 100,               # minimum distance in meters
  crs              = 26904,
  type_known       = "known",
  out_gpkg         = "scratch_data/randompoints_known_v1.1_2026.gpkg")  # change to your output gpkg

generate_random_points(
  layer            = oahu_strata_unknown,
  site_field       = "site_name",
  code_field       = "site_code",
  strata_field     = "combined_class",
  num_points_field = "num_point",      # your point count field...this info is attached to your layer attributes
  max_num_points   = 100,
  min_distance     = 100,              # minimum distance in meters
  crs              = 26904,
  type_known       = "unknown",
  out_gpkg         = "scratch_data/randompoints_unknown_v1.1_2026.gpkg")  # change to your output gpkg

#==========================================================================================================#
# Part II: Priority Filtering Tool - Function
#==========================================================================================================#
## Keeps points stratum by stratum, in priority order. A point is kept only if it is at least
## min_distance away from every point already kept (from higher-priority strata AND earlier
## points in the same stratum).

filter_points_by_priority <- function(in_gpkg, priority_order, class_field  = "combined_class",
                                      min_distance = 100, out_gpkg = NULL, out_layer =  "priority_points",
                                      overwrite    = FALSE) {
  
  ### Read and combine all layers from the input gpkg
  layers  <- st_layers(in_gpkg)$name
  pts_all <- layers %>%
    lapply(function(x) st_read(in_gpkg, layer = x, quiet = TRUE)) %>%
    bind_rows()
  
  ### Warn about classes that aren't in priority_order
  missing_cls <- setdiff(unique(pts_all[[class_field]]), priority_order)
  
  ### Start with an empty sf object (same columns as the input)
  accepted_pts <- pts_all[0, ]
  
  ### Loop through strata in priority order
  for (cls in priority_order) {
    message("Processing: ", cls)
    pts_cls <- dplyr::filter(pts_all, .data[[class_field]] == cls)
    ### if strata is empty, skip it
    if(nrow(pts_cls) == 0) next
    ## keep the vector (true/false tracker)
    keep <- rep(FALSE, nrow(pts_cls))
    
    ### Evaluate each candidate point
    for (i in seq_len(nrow(pts_cls))) {
      ### Extract single candidate
      candidate <- pts_cls[i, ]
      ### Code for if it's the first time running... aka if no points exist in the total accepted pts list
      if(is.null(accepted_pts) || nrow(accepted_pts) == 0){
        # then automatically accept this point and move to the next point
        keep[i] <- TRUE
        next}
      
      ### if not the first time...compute the candidate point's distance to all accepted points
      d <- st_distance(candidate, accepted_pts)
      ### keep point if it is at least 100 m away from EVERY previously accepted point
      if(all(as.numeric(d) >= min_distance)){
        keep[i] <- TRUE}}
    
    ### collect the kept points for the stratum (take only TRUE rows and save)
    new_pts <- pts_cls[keep, ]
    
    ### Add these to the total accepted set
    accepted_pts <- if(is.null(accepted_pts)){
      new_pts
    } else {
      rbind(accepted_pts, new_pts)}
    
    message(cls, ": kept ", nrow(new_pts))
  }
  
  message("Total points kept: ", nrow(accepted_pts))
  
  ### Export (optional)
  if (!is.null(out_gpkg)) {
    out_dir <- dirname(out_gpkg)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    st_write(accepted_pts,
             dsn          = out_gpkg,
             layer        = out_layer,
             delete_layer = FALSE,   # TRUE will overwrite an existing layer with this name
             quiet        = TRUE)
    message("Written to: ", out_gpkg, " (layer: ", out_layer, ")")
  }
  return(accepted_pts)
}

## Run Function
accepted_pts_known <- filter_points_by_priority(
  in_gpkg        = "scratch_data/Oahu_v1.1_Points_Known_2026.gpkg",
  priority_order = c("mid_complex", "shallow_complex", "mid_smooth", "shallow_smooth", "deep_complex", "deep_smooth"),
  class_field    = "combined_class",
  min_distance   = 100,
  out_gpkg       = "output_data/Known/Oahu_v1.1_Points_Priority_Known_2026.gpkg",  # change to your output gpkg
  out_layer      = "Oahu_v1.1_Known_Points_Priority"
)

accepted_pts_unknown <- filter_points_by_priority(
  in_gpkg        = "scratch_data/Oahu_v1.1_Points_Unknown_2026.gpkg",
  priority_order = c("mid_complex", "shallow_complex", "mid_smooth", "shallow_smooth", "deep_complex", "deep_smooth"),
  class_field    = "combined_class",
  min_distance   = 100,
  out_gpkg       = "output_data/Unknown/Oahu_v1.1_Points_Priority_Unknown_2026.gpkg",  # change to your output gpkg
  out_layer      = "Oahu_v1.1_Unknown_Points_Priority"
)

#==========================================================================================================#
# Part III: Collect points by site & export as csv ----------------------------------------------------------
#==========================================================================================================#
## 3.01 Function
export_points_by_site <- function(pts, file_prefix, out_dir) {
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  ## Convert to lon/lat (4326)
  pts_4326 <- st_transform(pts, 4326)
  
  ## Compile sites for for loop
  sites <- unique(pts_4326[["site_name"]])
  
  ## Loop through to combine by site and export as csv
  for(site in sites){
    message("Processing site: ", site)
    site_pts <- pts_4326 %>% filter(site_name == site)
    
    coords <- st_coordinates(site_pts)
    df <- site_pts %>%
      st_drop_geometry() %>%
      mutate(
        lon = coords[,1],
        lat = coords[,2]
      ) %>%
      rename(strata = combined_class)
    
    ## File name: prefix_[type_]site.csv, with site name made file-safe
    safe_site <- gsub("[^A-Za-z0-9_]", "_", trimws(site))
    file_name <- paste0(file_prefix, "_", safe_site, ".csv")
    
    ## Export csv
    write.csv(df, file = file.path(out_dir, file_name), row.names = FALSE)
    
    message("Exported: ", site)
    message("Complete: ", site, " - ", nrow(df), " total points")
  }
}


## 3.02 Run Function
### Known Strata 
export_points_by_site(
  out_dir     = "output_data/Known",
  pts         = accepted_pts_known, 
  file_prefix = "Oahu_v1.1_Points_Priority_Known")

### Unknown Strata
export_points_by_site(
  out_dir     = "output_data/Unknown",
  pts         = accepted_pts_unknown, 
  file_prefix = "Oahu_v1.1_Points_Priority_Unknown")
