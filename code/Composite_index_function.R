# function for creating the index -------------------------------------
Comp_ind <- function(yearly_data, grids, year) {
  
  normalised_compactness <- yearly_data %>%
    select(ID, Total_points, Residential_points, Commercial_points, area_build) %>%
    drop_na(Total_points, Residential_points, Commercial_points, area_build) %>% 
    mutate(Total_points = normalize(Total_points)) %>%
    mutate(Residential_points = normalize(Residential_points)) %>%
    mutate(Commercial_points = normalize(Commercial_points)) %>%
    mutate(area_build = normalize(area_build)) %>%
    mutate(Total_points = Total_points * 1/4) %>%
    mutate(Residential_points = Residential_points * 1/4) %>%
    mutate(Commercial_points = Commercial_points * 1/4) %>%
    mutate(area_build = area_build * 1/4) %>%
    mutate_all(funs(replaceMissing))
  
  
  
  compactness_score <- normalised_compactness %>% 
    mutate(comp_ind = rowSums(.[-1])) # row sums except first column which is the ID
  
  compactness_rank <- rank(-compactness_score$comp_ind, ties.method = 'random')
  
  
  compact_rank <- data.frame(
    ID = normalised_compactness$ID,
    Cmp_sc = compactness_score$comp_ind, 
    Cmp_rnk = compactness_rank
    
  )
  
  ###
  ### 2. Open/green space
  ###
  
  
  
  normalised_GreenSp <- yearly_data %>%
    select(ID, area_land_mult, area_land_nat, area_land_nat_lag) %>%
    drop_na(area_land_mult, area_land_nat, area_land_nat_lag) %>% 
    mutate(area_land_mult = normalize(area_land_mult)) %>%
    mutate(area_land_nat = normalize(area_land_nat)) %>%
    mutate(area_land_nat_lag = normalize(area_land_nat_lag)) %>%
    mutate(area_land_mult = area_land_mult * 1/3) %>%
    mutate(area_land_nat = area_land_nat * 1/3) %>%
    mutate(area_land_nat_lag = area_land_nat_lag * 1/3) %>%
    mutate_all(funs(replaceMissing))
  
  
  
  GreenSp_score <- normalised_GreenSp %>% 
    mutate(GreenSp_ind = rowSums(.[-1])) # row sums except first column which is the ID
  
  GreenSp_rank <- rank(-GreenSp_score$GreenSp_ind, ties.method = 'random')
  
  GreenSp_rank <- data.frame(
    ID = normalised_GreenSp$ID,
    GreenSp_sc = GreenSp_score$GreenSp_ind, 
    GreenSp_rnk = GreenSp_rank
    
  )
  
  
  ###
  ### 3. Walkability
  ###
  
  
  
  normalised_walkability <- yearly_data %>%
    select(ID, area_path, area_roadside, area_path_lag, area_roadside_lag, area_road) %>%
    drop_na(area_path, area_roadside, area_path_lag, area_roadside_lag, area_road) %>% 
    mutate(area_path = normalize(area_path)) %>%
    mutate(area_roadside = normalize(area_roadside)) %>%
    mutate(area_path_lag = normalize(area_path_lag)) %>%
    mutate(area_roadside_lag = normalize(area_roadside_lag)) %>%
    mutate(area_road = normalize(area_road)) %>%
    mutate(area_road = area_road * -1) %>% # to capture the negative sign of the high road proportion
    mutate(area_path = area_path * 1/5) %>%
    mutate(area_roadside = area_roadside * 1/5) %>%
    mutate(area_path_lag = area_path_lag * 1/5) %>%
    mutate(area_roadside_lag = area_roadside_lag * 1/5) %>%
    mutate(area_road = area_road * 1/5) %>%
    mutate_all(funs(replaceMissing))
  
  
  
  
  Walkability_score <- normalised_walkability %>% 
    mutate(Walkability_ind = rowSums(.[-1])) # row sums except first column which is the ID
  
  Walkability_rank <- rank(-Walkability_score$Walkability_ind, ties.method = 'random')
  
  
  wlkb_rank <- data.frame(
    ID = normalised_walkability$ID,
    Wlkb_sc = Walkability_score$Walkability_ind, 
    Wlkb_rnk = Walkability_rank
    
  )
  
  Join1 <- left_join(grids, compact_rank, by = "ID")
  Join2 <- left_join(Join1, GreenSp_rank, by = "ID")
  domains <- left_join(Join2, wlkb_rank, by = "ID")
  

  ########
  # Calculate Composite index
  
  # drop the rows that do not contain values in all three domains
  # domains_complete <- domains %>%
  #   drop_na(Cmp_sc, GreenSp_sc, Wlkb_sc)
  
  # subset the rows that contain a value in at least one of the domains
  domains_complete <- domains %>% filter(!is.na(Cmp_sc) | !is.na(GreenSp_sc) | !is.na(Wlkb_sc))
  
  # then places the grids with missing values in the last rank of each domain
  domains_complete <- domains_complete %>%
    mutate(Cmp_rnk = if_else(is.na(Cmp_rnk), max(Cmp_rnk, na.rm = TRUE), Cmp_rnk)) %>% 
    mutate(GreenSp_rnk = if_else(is.na(GreenSp_rnk), max(GreenSp_rnk, na.rm = TRUE), GreenSp_rnk)) %>% 
    mutate(Wlkb_rnk = if_else(is.na(Wlkb_rnk), max(Wlkb_rnk, na.rm = TRUE), Wlkb_rnk))
  
  # subset the cols needed
  domains_subs <- domains_complete[,c("ID", "Cmp_rnk",
                             "GreenSp_rnk", "Wlkb_rnk")]
  
  domains_subs <- domains_subs %>%
    filter(!is.na(Cmp_rnk) | !is.na(GreenSp_rnk) | !is.na(Wlkb_rnk))
  
  # treat the dataset as dataframe
  st_geometry(domains_subs) <- NULL
  
  InvRank <- function(v) rank(-v)
  
  exponential_domains <- domains_subs %>%
    mutate_at(vars(-ID), funs(InvRank)) %>%
    mutate_at(vars(-ID), funs(expoTransform))
  
  with(exponential_domains, {
    built_index_score <<-
      1/3 * Cmp_rnk +
      1/3 * GreenSp_rnk +
      1/3 * Wlkb_rnk
  })
  
  built_index_rank <- rank(-built_index_score)
  
  built_index_results <- data.frame(
    ID = domains_subs$ID,
    built_index_score = built_index_score, 
    built_index_rank = built_index_rank
  )
  
  
  Comp_ind_ranks <- left_join(domains, built_index_results, by = "ID")
  
  
  # Subset the dataset to the variables we want to map
  Comp_ind_ranks_subs <- Comp_ind_ranks[, c(
    'built_index_rank',
    'Cmp_rnk',
    'GreenSp_rnk',
    'Wlkb_rnk'

  )]
  
  # specify the CRS required
  WGS84 = "+init=epsg:4326"
  
  # Make sure that the layer is projected on WGS84 so can be plot with leaflet
  Comp_ind_ranks_subs <- st_transform(Comp_ind_ranks_subs,WGS84)
  
  # Convert the geospatial data from sf to sp object (I found it easier to be plotted)
  Comp_ind_ranks_subs_sp <- as(Comp_ind_ranks_subs, 'Spatial')
  
  # Change the the column names to remove the dot between lines
  names(Comp_ind_ranks_subs_sp@data)[names(Comp_ind_ranks_subs_sp@data)=="Cmp_rnk"] <- "Compactness domain rank"
  names(Comp_ind_ranks_subs_sp@data)[names(Comp_ind_ranks_subs_sp@data)=="GreenSp_rnk"] <- "Green space domain rank"
  names(Comp_ind_ranks_subs_sp@data)[names(Comp_ind_ranks_subs_sp@data)=="Wlkb_rnk"] <- "Walkability domain rank"
  names(Comp_ind_ranks_subs_sp@data)[names(Comp_ind_ranks_subs_sp@data)=="built_index_rank"] <- "Sustainable Urban Development Index rank"
  
  # Specify the colour pallete
  #my_colours <- rev(brewer.pal(9,"YlOrRd")) # rev indicates reverse order
  my_colours <- brewer.pal(9,"YlOrRd") # rev indicates reverse order
  my_colours <- colorRampPalette(my_colours)(10) # there is a limit to 9 colours from the palette so I have to extent it
  
  # Initiate the map
  map <- leaflet() %>% addTiles() %>%
    addProviderTiles(providers$CartoDB.DarkMatter)
  
  # Create a layer for each of the variables through looping across the columns
  for (i in seq_along(1:4)) {
    # create the colour palette for each layer
    pal <- colorQuantile(my_colours, Comp_ind_ranks_subs_sp[[i]], n = 10)
    
    map <- map %>% 
      addPolygons(data = Comp_ind_ranks_subs_sp,
                  fillColor = ~pal(Comp_ind_ranks_subs_sp[[i]]),
                  weight = 0.4,
                  opacity = 0.8,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.5,
                  popup = paste(names(Comp_ind_ranks_subs_sp@data[i]),":" , round(Comp_ind_ranks_subs_sp[[i]],2), "<br>",
                                "FUA: ", Comp_ind_ranks$fuaname, "<br>"),
                  group = names(Comp_ind_ranks_subs_sp@data[i]),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    bringToFront = TRUE))   %>%
      addLayersControl(baseGroups = c(names(Comp_ind_ranks_subs_sp@data)),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(
        c(
          "Compactness domain rank",
          'Green space domain rank',
          'Walkability domain rank'
        )
      )
  }
  
  
  # Add a legend and title to the map
  Ind_map <- map %>%
    addLegend(colors = c("#FFFFCC","#FFEFA4","#FEDD7F","#FEBE59","#FD9D43","#FC7134","#F33C25","#DA141E","#B60026","#800026"),
              labels= c("Best performing decile", "","","", "","","","","","Worst performing decile"),
              position = "bottomleft",
              title = paste("Built environment index in", year, "<br>", "by 1km grid (Deciles)"))  %>% 
    addFullscreenControl()
  
  # Plot the map
  Ind_map
  
  # create a list with the results
  results <- list("Domains_rank" = domains, 
                  "Comp_ind" = Comp_ind_ranks, 
                  "Interactive_map" = Ind_map,
                  "compactness_score" = compactness_score,
                  "GreenSp_score" = GreenSp_score,
                  "Walkability_score" = Walkability_score)
  
  return(results)
  
  
}