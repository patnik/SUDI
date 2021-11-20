# these are the libraries needed to run these functions
# library(sf)
# library(dplyr)
# library(data.table)

########
# FUNCTION 1

# This function calculates area in sqm cover by a particular class in each grid

# It returns a data frame with two columns (grid ID and area built - in sqkm)

# makes sure that the data are projected in the same coordinate system 
# NOTE: I have updated the code so everything is on WGS84

# Theme can be either 'Buildings', 'Land', 'Roads Tracks And Paths'
# Make is used when Theme = 'Land' and can be either 'Multiple' or 'Natural'
# DescGroup is used when Theme = 'Roads Tracks And Paths' 
# and can be either 'Road Or Track' or 'Roadside' or 'Path'
# blt_feat can be either 'building' or 'land' or 'roads_paths'
# in the unique_id_code should specify the unique ID of the higher_geo_lay

fnct1 <- function(blt_env_lay, higher_geo_lay, Theme, Make, DescGroup, 
                  blt_feat, unique_id_code) {
  # this is used to calculate the total time the function takes to run
  start_time <- Sys.time()
  
  # this is the desired CRS
  # I am using BNG in this case as it uses meters (so I can calculate buffer)
  # WGS84 uses degress which is tricky
  BNG = "+init=epsg:27700"
  # make sure that all layers have consistent CRS- in this case is WGS84
  blt_env_lay <- st_transform(blt_env_lay,BNG)
  higher_geo_lay <- st_transform(higher_geo_lay,BNG)
  
  #### 1st step
  # calculate total area of grids
  higher_geo_lay$tot_area_sqkm <- st_area(higher_geo_lay$geometry) / 1000000
  # convert area of grids to numeric too
  higher_geo_lay$tot_area_sqkm <- as.numeric(higher_geo_lay$tot_area_sqkm)
  
  #### 2nd step
  if (blt_feat == 'building') {
    # subset the buildings
    built <- subset(blt_env_lay, Theme == theme)
  } 
  else if (blt_feat == 'land') {
    # or subset the land
    built <- subset(blt_env_lay, Theme == theme & Make == make)
  }
  else if (blt_feat == 'roads_paths') {
    # or subset the Roads and Paths
    built <- subset(blt_env_lay, Theme == theme & DescGroup == descriptiveGroup)
  }
  else if (blt_feat == 'other') {
    built <- blt_env_lay
  }
  
  
  #run the intersect function, converting the output to a tibble in the process
  int <- as_tibble(st_intersection(built, higher_geo_lay))
  
  int$area_sqkm<- st_area(int$geometry) /1000000
  
  # convert area to numeric
  int$area_sqkm <- as.numeric(int$area_sqkm)
  
  # remove polygons that are outside the grid boundaries to avoid getting errors
  int <- int %>%
    drop_na(!!as.name(unique_id_code))
  
  BuiltByGrid <- int %>%
    group_by(!!as.name(unique_id_code)) %>% # '!!' this evaluates if it is true, when it is '!' evaluates if it is false
    summarise(areaBuilt = sum(area_sqkm))
  
  # this is used to calculate the total time the function takes to run
  end_time <- Sys.time()
  total_time <- end_time - start_time
  
  
  results <- list(fnct1_output = BuiltByGrid, running_time = total_time)
  return(results)
  
}



########
# FUNCTION 2

# This function calculates what area (in sqkm) in a buffer zones from a particular class is covered by other class

# It returns a data frame with two columns (blt_env_lay_1 ID and area built within
# the buffer zones - in sqkm)

# NOTE: # I am using BNG in this case as it uses meters (so I can calculate buffer)
# WGS84 uses degress which is tricky

# blt_env_lay_1 is the feature that we want buffer zones
# blt_env_lay_2 is the feature that will be within each buffer

# the following inputs should be specified for blt_env_lay_1 and blt_env_lay_2 separately
# Theme can be either 'Buildings', 'Land', 'Roads Tracks And Paths'
# Make is used when Theme = 'Land' and can be either 'Multiple' or 'Natural'
# DescGroup is used when Theme = 'Roads Tracks And Paths' 
# and can be either 'Road Or Track' or 'Roadside' or 'Path'
# blt_feat can be either 'building' or 'land' or 'roads_paths'
# in the unique_id_code should specify the unique ID of the higher_geo_lay
# buffer_m is the buffer zones that we want to draw around the polygons in metres

fnct2 <- function(blt_env_lay_1, Theme_1, Make_1, DescGroup_1, blt_feat_1,
                  blt_env_lay_2, Theme_2, Make_2, DescGroup_2, blt_feat_2,
                  unique_id_code, buffer_m) {
  # this is used to calculate the total time the function takes to run
  start_time <- Sys.time()
  
  # this is the desired CRS
  # I am using BNG in this case as it uses meters (so I can calculate buffer)
  # WGS84 uses degress which is tricky
  BNG = "+init=epsg:27700"
  # make sure that all layers have consistent CRS- in this case is WGS84
  blt_env_lay_1 <- st_transform(blt_env_lay_1,BNG)
  blt_env_lay_2 <- st_transform(blt_env_lay_2,BNG)
  
  #### subset the built env feature that we want to draw buffer areas
  if (blt_feat_1 == 'building') {
    # subset the buildings
    built_1 <- subset(blt_env_lay_1, Theme_1 == theme)
  } 
  else if (blt_feat_1 == 'land') {
    # or subset the land
    built_1 <- subset(blt_env_lay_1, Theme_1 == theme & Make_1 == make)
  }
  else if (blt_feat_1 == 'roads_paths') {
    # or subset the Roads and Paths
    built_1 <- subset(blt_env_lay_1, Theme_1 == theme & DescGroup_1 == descriptiveGroup)
  }
  else if (blt_feat_1 == 'other') {
    built_1 <- blt_env_lay_1
  }
  
  # 1km buffer
  blt_env_lay_buffer <- st_buffer(built_1, buffer_m)
  
  # total area of grids in Liverpool
  blt_env_lay_buffer$area_sqkm <- st_area(blt_env_lay_buffer$geometry) / 1000000
  # convert area of grids to numeric too
  blt_env_lay_buffer$area_sqkm <- as.numeric(blt_env_lay_buffer$area_sqkm)
  
  #### subset the built env feature that is within the buffer areas
  if (blt_feat_2 == 'building') {
    # subset the buildings
    built_2 <- subset(blt_env_lay_2, Theme_2 == theme)
  } 
  else if (blt_feat_2 == 'land') {
    # or subset the land
    built_2 <- subset(blt_env_lay_2, Theme_2 == theme & Make_2 == make)
  }
  else if (blt_feat_2 == 'roads_paths') {
    # or subset the Roads and Paths
    built_2 <- subset(blt_env_lay_2, Theme_2 == theme & DescGroup_2 == descriptiveGroup)
  }
  else if (blt_feat_2 == 'other') {
    built_2 <- blt_env_lay_2
  }
 
  #run the intersect function, converting the output to a tibble in the process
  # running time for part of Manchester city centr i abour 15-20 mins
  int <- as_tibble(st_intersection(built_2, blt_env_lay_buffer))
  
  int$area_sqkm<- st_area(int$geometry) /1000000
  
  # convert area to numeric
  int$area_sqkm <- as.numeric(int$area_sqkm)
  
  # remove polygons that are outside the grid boundaries to avoid getting errors
  int <- int %>%
    drop_na(!!as.name(unique_id_code))
  
  BuiltByBuffer <- int %>%
    group_by(!!as.name(unique_id_code)) %>% # '!!' this evaluates if it is true, when it is '!' evaluates if it is false
    summarise(BufareaBuilt = sum(area_sqkm))
  
  # this is used to calculate the total time the function takes to run
  end_time <- Sys.time()
  total_time <- end_time - start_time
  
  
  results <- list(fnct1_output = BuiltByBuffer, running_time = total_time)
  return(results)
  
}


########
# FUNCTION 3

# This function calculates the number of points within each grid

# It returns a data frame with two columns (grid ID and NoPoints)
# NOTE: this function can be updated to relfect the categories present in the OS data
# otherwise the data need to be filtered before calling this function
# in the unique_id_code should specify the unique ID of the higher_geo_lay


fnct3 <- function(point_data, higher_geo_lay, unique_id_code) {
  
  # this is the desired CRS
  # I am using BNG in this case as it uses meters (so I can calculate buffer)
  # WGS84 uses degress which is tricky
  BNG = "+init=epsg:27700"
  # make sure that all layers have consistent CRS- in this case is WGS84
  point_data <- st_transform(point_data,BNG)
  higher_geo_lay <- st_transform(higher_geo_lay,BNG)
  
  # find points within polygons
  points_in_grids <- st_join(point_data, higher_geo_lay, join = st_within)
  
  # remove points that are outside the grid boundaries to avoid getting errors
  points_in_grids <- points_in_grids %>%
    drop_na(!!as.name(unique_id_code))

  
  # to count the number of points by grid
  points_count <- count(as_tibble(points_in_grids), !!as.name(unique_id_code))
  names(points_count)[2] <- "NoPoints"
  
  # this is an alternative way of doing the same as above
  # points_count <- as.data.frame(table(points_in_grids$ID))
  # colnames(points_count) <- c("ID","NoPoints")
  
  return(points_count)
  
}

########
# FUNCTION for unzipping gz files

# file_dir = is the file directory where the files are located

unzipping_gz <- function(file_dir) {
  zipF <- list.files(path = file_dir, pattern = "*.gz", full.names = TRUE)
  library(R.utils)
  lapply(zipF, function(x) gunzip(x, remove=FALSE))
  
}


########
# FUNCTION for reading gml files

# file_dir -> is the file directory where the files are located
# env_layer -> should be either 'TopographicArea' or 'TopographicLine' or 'TopographicPoint'
# '$' is the regular expression denoting the end of the string.
# if capital leter are used i.e. '.gml' and '.GML' are different (ignore.case = TRUE solves that issue)

reading_gml <- function(file_dir, env_layer) {
  # list all the gml
  gmlF <- list.files(path = file_dir, pattern = '*.gml$', full.names = TRUE, ignore.case = TRUE)
  
  
  GML_read_list <- list() # creates a list
  for (k in 1:length(gmlF)){
    GML_read_list[[k]] <- st_read(gmlF[k], layer = env_layer)
  }
  
  MergedData <- do.call(rbind, GML_read_list)
  return(MergedData)
  
}

########
# FUNCTION for reading layers for Mastermap Topo

# Note that the files have to be unzipped first
# also they do not have a specific file type?
# we have to account for some gfs files that are being created after the unzipping process
# tryCatch is used to catch any errors but to conitnue running the function
# there are errors sometime when the file does not have the specific layers we want i.e. some files do not have 'TopographicArea'
# # for some reason there are some NULL elements in the files supplied
# this code 'layers_read_list[lengths(layers_read_list) != 0]' removes this NULL elements

# file_dir -> is the file directory where the files are located
# env_layer -> should be either 'TopographicArea' or 'TopographicLine' or 'TopographicPoint'
# '$' is the regular expression denoting the end of the string.
# if capital leter are used i.e. '.gml' and '.GML' are different (ignore.case = TRUE solves that issue)

reading_mmap_topo <- function(file_dir, env_layer) {
  # list all the gml
  gzF <- list.files(path = file_dir, pattern = '*.gz|*.gfs', full.names = TRUE, ignore.case = TRUE)
  allF <- list.files(path = file_dir, full.names = TRUE, ignore.case = TRUE)
  
  layersF <- setdiff(allF, gzF)
  
  layers_read_list <- list() # creates a list
  for (k in 1:length(layersF)){
    tryCatch({
      layers_read_list[[k]] <- st_read(layersF[k], layer = env_layer)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    # make sure that we keep the columns we need
    # this avoids inconsistencies in the column number

    
    col_needed = c("fid","featureCode",
                   "version","versionDate",
                   "calculatedAreaValue","make",
                   "physicalLevel","theme",
                   "changeDate","reasonForChange",
                   "descriptiveGroup","descriptiveTerm")

    
    layers_read_list <- lapply(layers_read_list, "[", col_needed) 
    
  }
  
  # remove NULL elements
  layers_read_list <- layers_read_list[lengths(layers_read_list) != 0]
  
  MergedData <- do.call(rbind, layers_read_list)
  return(MergedData)
  
}


########
# FUNCTION for creating simple polygons around each detailed file of master map topo

# this function can be used to find correspondence between our FUA grids and the files that need to be read 
# for each FUA. In this way we can save computational power and time


# file_dir -> is the file directory where the files are located
# env_layer -> should be either 'TopographicArea' or 'TopographicLine' or 'TopographicPoint'
# '$' is the regular expression denoting the end of the string.
# if capital leter are used i.e. '.gml' and '.GML' are different (ignore.case = TRUE solves that issue)

create_bbox_pol <- function(file_dir, env_layer) {
  # list all the gml
  gzF <- list.files(path = file_dir, pattern = '*.gz|*.gfs', full.names = TRUE, ignore.case = TRUE)
  allF <- list.files(path = file_dir, full.names = TRUE, ignore.case = TRUE)
  
  layersF <- setdiff(allF, gzF)
  
  layers_read_list <- list() # creates a list
  bb <- list()
  bbox_pol <- list()

  for (k in 1:length(layersF)){
    tryCatch({
      layers_read_list[[k]] <- st_read(layersF[k], layer = 'TopographicArea')
      
      # extract the coordinates of the boundix box for each fileand create a polygon
      bb[[k]] <- st_as_sfc(st_bbox(layers_read_list[[k]]))
      
      # attach the name of the file to this polygon
      bbox_pol[[k]] <- st_sf(data.frame(un_ID = basename(layersF[k]), geom = bb[[k]]))
      
      
    }, error = function(e) { cat("failing to read :",basename(layersF[k]), file = paste0(file_dir,"/log.txt"), append = TRUE) })
    
    #warning = function(e){writeLines(paste0("at index/step ", i, " occurred following error ", as.character(e)))})
    #error=function(e){write(toString(e), log.path, append=TRUE) })
    # error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

    
    
  }
   
  # remove NULL elements
  bbox_pol <- bbox_pol[lengths(bbox_pol) != 0]
  
  MergedData <- do.call(rbind, bbox_pol)
  return(MergedData)
  
}






########
# FUNCTION for reading csv files

# file_dir -> is the file directory where the files are located
# file_type -> is the file type of the files for example there should be a distinction
# if capital leter are used i.e. '.csv' and '.CSV' are different (ignore.case = TRUE solves that issue)
# zipped_files -> can be "yes" or "no". Yes if the files provided are zipped and no if the files provided are not zipped

reading_csv <- function(file_dir, file_type, zipped_files) {
  # list all the csv
  csvF <- list.files(path = file_dir, pattern = paste0('*', file_type, ('$')), full.names = TRUE, ignore.case = TRUE)
  CSV_read_list <- list() # creates a list
  
  if (zipped_files == 'yes') {
    unz_item <- 'unzip -p ' # this code is neede to unzip the files
    for (k in 1:length(csvF)){
      CSV_read_list[[k]] <- fread(paste0(unz_item, csvF[k]), na.strings=c(""," ","NA"))
    }
  } 
  else if (zipped_files == 'no') {
    for (k in 1:length(csvF)){
      CSV_read_list[[k]] <- fread(csvF[k], na.strings=c(""," ","NA"))
    }
  }
  

  MergedData <- do.call(rbind, CSV_read_list)
  return(MergedData)
  
}

########
# FUNCTION for reading csv files with the gridded data

# library(data.table) is needed for this function

# file_dir -> is the file directory where the files are located
# lst_of_area_names -> is a list with the national grid names that we want to read 
# (i.e. if we want NS and NT we pass c("NS", "NT"))
# att_name -> the name of the built environment attribute (i.e. build, land_mult, land_nat, etc.)
# file_ext -> the file extension i.e. .csv or .CSV (it sould work with .txt too)
# clmn_names -> list of the column names


read_grid_data <- function(file_dir, lst_of_area_names, att_name, file_ext, clmn_names) {
  
  CSV_grid_list <- list() # creates a list
  for (k in 1:length(lst_of_area_names)){
    
    dir <- paste0(file_dir, "/", lst_of_area_names[k], "_", att_name, file_ext)
    #print(dir)
    # drop the first column which is just a sequential number
    CSV_grid_list[[k]] <- fread(dir, col.names = clmn_names)
    
  }
  
  MergeAll <- do.call(rbind, CSV_grid_list)
  return(MergeAll)
  
}



########
# FUNCTION for geolocating point data based on coordinates

# x_coord and y_coord specifies the names of the columns that inlcude the coordinates
# can also be easting and northing or x and y
# if the projection is British National Grid
# the coord_sys can be "+init=epsg:4326" for latlong (i.e. WGS84), 
# "+init=epsg:27700" for British National Grid or any other

geolocate_points <- function(point_data, x_coord, y_coord, coord_sys) {
  st_crs(coord_sys)
  point_data_sf <- st_as_sf(point_data, coords = c(x_coord, y_coord), crs = coord_sys)
  return(point_data_sf)
  
}

########
# FUNCTION for returning the number of point by grid and FUA - this is just for Address Point data from OS

# arguments to pass in the function
# 1. FUA geospatial file (i.e. grids) - fua geographies/boundaries
# 2. FUA name
# 3. csv file list corresponding to the region that belongs
# 4. the names of the columns contain Easting and Northing


# the output will be: 
# 1. the points that fall within the FUA boundaries (name: FUA_points) 
# - geospatial file with crs = BNG;
# 2. the total number of points in each polygon (name: total_point_count) - no geography attached
# 3. the  number of residential points in each polygon (name: residential_point_count) - no geography attached
# 4. the  number of commercial points in each polygon (name: commercial_point_count) - no geography attached
# 5. gridded dataset for the FUA with the total number of points 
# as well as residential and commercial distinction. Also the commercial to residential ratio
#  higher number means more commercial units (name: gridded_output)
# - geospatial file with crs = BNG

# function to run for each FUA
ad_points_FUA <- function(fua_geo, fua_name, regional_list, eastings, northings, unique_id_code) {
 
  # this is used to calculate the total time the function takes to run
  start_time <- Sys.time()
  
  # Subset the area we want
  fua_grids <- subset(fua_geo, fuaname == fua_name)
  
  # we geolocate the points - note that the coordinates are projected on British National Grid
  BNG = "+init=epsg:27700"
  regional_list_geol <- geolocate_points(regional_list, eastings, northings, BNG)
  
  # Evaluate if both columns Department Name and Organisation Name
  # are NA and assign it as residential, otherwise it's commercial
  regional_list_geol$class <-
    ifelse(
      is.na(regional_list_geol$DP) &
        is.na(regional_list_geol$ON),
      "residential",
      "commercial"
    )
  
  # crop points that are within FUA boundaries
  # first make sure both files have the same coordinate system
  BNG = "+init=epsg:27700"
  # make sure that all layers have consistent CRS- in this case is WGS84
  regional_list_geol <- st_transform(regional_list_geol,BNG)
  fua_grids <- st_transform(fua_grids,BNG)
  # then intersect the points with the grids
  # so then we will have just the points that fall within the FUA boundaries
  # we use that as an output
  points_in_grids <- st_intersection(regional_list_geol, fua_grids) 
  
  # 10.Count points per grid but also separated by class
  # this uses dplyr library
  tot_pnts <- count(as_tibble(points_in_grids), !!as.name(unique_id_code))
  
  # we rename the columns
  # we rename the columns
  names(tot_pnts)[2] <- "tot_pnts_count"
  
  # do the same including the categories
  class_pnts <- count(as_tibble(points_in_grids), !!as.name(unique_id_code), class)
  
  # subset for each category
  res_pnts <- subset(class_pnts, class == "residential")
  names(res_pnts)[3] <- "res_pnts_count"
  
  com_pnts <- subset(class_pnts, class == "commercial")
  names(com_pnts)[3] <- "com_pnts_count"
  
  # merge all tables 
  mrg1 <- merge(x = fua_grids , y = tot_pnts, by = unique_id_code, all.x = TRUE)
  
  mrg2 <- merge(x = mrg1 , y = res_pnts[, c(unique_id_code, 'res_pnts_count')], by = unique_id_code, all.x = TRUE)
  
  gridded_data <- merge(x = mrg2 , y = com_pnts[, c(unique_id_code, 'com_pnts_count')], by = unique_id_code, all.x = TRUE)
  
  # calculate the mix of commercial to residential
  gridded_data$land_use_mix <- gridded_data$com_pnts_count / gridded_data$res_pnts_count
  
  # this is used to calculate the total time the function takes to run
  end_time <- Sys.time()
  total_time <- end_time - start_time
  
  # this is the resulting list 
  results <- list(FUA_points = points_in_grids,
                  total_point_count = tot_pnts, 
                  residential_point_count = res_pnts,
                  commercial_point_count = com_pnts, 
                  gridded_output = gridded_data,
                  running_time = total_time)
  return(results)
  
}


########
# FUNCTION for returning the number of point by grid and FUA - this is just for Address Base data from OS

# arguments to pass in the function
# 1. FUA geospatial file (i.e. grids) - fua geographies/boundaries
# 2. FUA name
# 3. csv file list corresponding to the region that belongs
# 4. the names of the columns contain Easting and Northing
# 5. subset area can be yes or no depending whether we want to subset the area referring to reference geography-in this case is FUAs


# the output will be: 
# 1. the points that fall within the FUA boundaries (name: FUA_points) 
# - geospatial file with crs = BNG;
# 2. the total number of points in each polygon (name: total_point_count) - no geography attached
# 3. the  number of residential points in each polygon (name: residential_point_count) - no geography attached
# 4. the  number of commercial points in each polygon (name: commercial_point_count) - no geography attached
# 5. gridded dataset for the FUA with the total number of points 
# as well as residential and commercial distinction. Also the commercial to residential ratio
#  higher number means more commercial units (name: gridded_output)
# - geospatial file with crs = BNG

ad_base <- function(fua_geo, fua_name, regional_list, eastings, northings, unique_id_code, subset_area,
                        classification, land_use_1, land_use_2) {
  
  # this is used to calculate the total time the function takes to run
  start_time <- Sys.time()
  
  if (subset_area == 'yes') {
    # Subset the area we want
    fua_grids <- subset(fua_geo, fuaname == fua_name)
  } 
  else if (subset_area == 'no') {
    fua_grids <- fua_geo
  }
  
  
  
  # we geolocate the points - note that the coordinates are projected on British National Grid
  BNG = "+init=epsg:27700"
  regional_list_geol <- geolocate_points(regional_list, eastings, northings, BNG)
  
  # crop points that are within FUA boundaries
  # first make sure both files have the same coordinate system
  BNG = "+init=epsg:27700"
  # make sure that all layers have consistent CRS- in this case is WGS84
  regional_list_geol <- st_transform(regional_list_geol,BNG)
  fua_grids <- st_transform(fua_grids,BNG)
  # then intersect the points with the grids
  # so then we will have just the points that fall within the FUA boundaries
  # we use that as an output
  points_in_grids <- st_intersection(regional_list_geol, fua_grids) 
  
  # 10.Count points per grid but also separated by class
  # this uses dplyr library
  tot_pnts <- count(as_tibble(points_in_grids), !!as.name(unique_id_code))
  
  # we rename the columns
  names(tot_pnts)[2] <- "tot_pnts_count"
  
  # do the same including the categories
  class_pnts <- count(as_tibble(points_in_grids), !!as.name(unique_id_code), !!as.name(classification))
  names(class_pnts)[3] <- "pnts_count"
  
  class_points <- class_pnts %>% spread(!!as.name(classification), pnts_count)
  
  # merge all tables 
  mrg1 <- merge(x = fua_grids , y = tot_pnts, by = unique_id_code, all.x = TRUE)
  
  gridded_data <- merge(x = mrg1 , y = class_points, by = unique_id_code, all.x = TRUE)
  
  # calculate the mix between two land uses
  #gridded_data[,'land_use_mix'] <- gridded_data[, land_use_1] / gridded_data[, land_use_2]
  # specify a dynamic variable name
  land_use_mix <- paste0(land_use_1, "_to_", land_use_2)
  gridded_data <- gridded_data %>% mutate(!!land_use_mix := !!as.name(land_use_1) / !!as.name(land_use_2))
  
  # this is used to calculate the total time the function takes to run
  end_time <- Sys.time()
  total_time <- end_time - start_time
  

  # this is the resulting list 
  results <- list(point_data = points_in_grids,
                  total_point_count = tot_pnts, 
                  class_point_count = class_points,
                  gridded_output = gridded_data,
                  running_time = total_time)
  return(results)
  
}


