FUA_decile_plot <- function(Comp_ind_file, year) {
  
  Comp_ind_file_dec <- as.data.frame(Comp_ind_file) %>% 
    mutate(Cmp_rnk_deciles = ntile(Cmp_rnk, 10) ) %>% 
    mutate(GreenSp_deciles = ntile(GreenSp_rnk, 10) ) %>% 
    mutate(Wlkb_rnk_deciles = ntile(Wlkb_rnk, 10) ) %>% 
    mutate(built_index_rank_deciles = ntile(built_index_rank, 10) )
  
  # calculate summary for comp index
  Comp_ind_summary <- Comp_ind_file_dec %>% 
    group_by(fuaname, built_index_rank_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  # calculate summary for compactness
  Compactness_summary <- Comp_ind_file_dec %>% 
    group_by(fuaname, Cmp_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  # calculate summary for green space
  GreenSp_summary <- Comp_ind_file_dec %>% 
    group_by(fuaname, GreenSp_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  # calculate summary for walkability
  Wlkb_summary <- Comp_ind_file_dec %>% 
    group_by(fuaname, Wlkb_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  
  Comp_ind_summary$built_index_rank_deciles <- as.character(Comp_ind_summary$built_index_rank_deciles)
  Compactness_summary$Cmp_rnk_deciles <- as.character(Compactness_summary$Cmp_rnk_deciles)
  GreenSp_summary$GreenSp_deciles <- as.character(GreenSp_summary$GreenSp_deciles)
  Wlkb_summary$Wlkb_rnk_deciles <- as.character(Wlkb_summary$Wlkb_rnk_deciles)
  
  
  # Specify the colours for each neighbourhood trajectory type
  cpal = c(
    "10" = "#800026",
    "9" = "#B60026",
    "8" = "#DA141E",
    "7" = "#F33C25",
    "6" = "#FC7134",
    "5" = "#FD9D43",
    "4" = "#FEBE59",
    "3" = "#FEDD7F",
    "2" = "#FFEFA4",
    "1" = "#FFFFCC"
  )
  
  
  
  # for a specific order
  Comp_ind_summary$built_index_rank_deciles <- factor(Comp_ind_summary$built_index_rank_deciles,
                                                      levels=c("1", "2", "3", "4","5",
                                                               "6", "7", "8", "9", "10"))
  
  Compactness_summary$Cmp_rnk_deciles <- factor(Compactness_summary$Cmp_rnk_deciles,
                                                levels=c("1", "2", "3", "4","5",
                                                         "6", "7", "8", "9", "10"))
  
  GreenSp_summary$GreenSp_deciles <- factor(GreenSp_summary$GreenSp_deciles,
                                            levels=c("1", "2", "3", "4","5",
                                                     "6", "7", "8", "9", "10"))
  
  Wlkb_summary$Wlkb_rnk_deciles <- factor(Wlkb_summary$Wlkb_rnk_deciles,
                                          levels=c("1", "2", "3", "4","5",
                                                   "6", "7", "8", "9", "10"))
  
  # Create the plot showing the FUA distribution for composite index
  p1 <- ggplot() + 
    geom_bar(aes(y =  freq, x = fuaname, fill = built_index_rank_deciles), data = Comp_ind_summary,  stat="identity")+
    labs(title = paste0("Distribution of grids in each decile for the composite index-", year, "across FUAs"), 
         x = "", y = "", fill = "") +
    scale_fill_manual(values = cpal)+
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 90, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_line('white', size = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  # Create the plot showing the FUA distribution for compactness domain
  p2 <- ggplot() + 
    geom_bar(aes(y =  freq, x = fuaname, fill = Cmp_rnk_deciles), data = Compactness_summary,  stat="identity")+
    labs(title = paste0("Distribution of grids in each decile for the compactness domain-", year, "across FUAs"), 
         x = "", y = "", fill = "") +
    scale_fill_manual(values = cpal)+
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 90, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_line('white', size = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  # Create the plot showing the FUA distribution for green space domain
  p3 <- ggplot() + 
    geom_bar(aes(y =  freq, x = fuaname, fill = GreenSp_deciles), data = GreenSp_summary,  stat="identity")+
    labs(title = paste0("Distribution of grids in each decile for the green space domain-", year, "across FUAs"), 
         x = "", y = "", fill = "") +
    scale_fill_manual(values = cpal)+
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 90, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_line('white', size = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  # Create the plot showing the FUA distribution for walkability domain
  p4 <- ggplot() + 
    geom_bar(aes(y =  freq, x = fuaname, fill = Wlkb_rnk_deciles), data = Wlkb_summary,  stat="identity")+
    labs(title = paste0("Distribution of grids in each decile for the walkability domain-", year, "across FUAs"), 
         x = "", y = "", fill = "") +
    scale_fill_manual(values = cpal)+
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 90, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_line('white', size = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  
  # create a list with the results
  results <- list("Composite_index_plot" = p1, 
                  "Compactness_plot" = p2, 
                  "GreenSpace_plot" = p3,
                  "Walkability_plot" = p4,
                  "Grids_deciles" = Comp_ind_file_dec)
  
  return(results)
  
}

# Function to calculate indicator means by composite index decile -------------------------
decile_summary <- function(indicator_data, decile_data) {
  cols_needed <- c("ID", "built_index_rank_deciles")
  full_data <- left_join(indicator_data, decile_data[,cols_needed], by = "ID")
  full_data$built_index_rank_deciles <- as.character(full_data$built_index_rank_deciles)
  
  # drop the rows that do not contain values in the ranking
  full_data <- full_data %>%
    drop_na(built_index_rank_deciles)
  
  # calculate summary for indicators by decile
  decile_summary <- full_data %>% 
    select(built_index_rank_deciles, Total_points, Residential_points, Commercial_points, area_build,
           area_land_mult, area_land_nat, area_path, area_road,
           area_roadside, area_land_nat_lag, area_path_lag, area_roadside_lag) %>%
    group_by(built_index_rank_deciles) %>% 
    summarise_each(funs(mean(., na.rm = TRUE)))
  
  
}

# function to plot decile distribution by year and FUA -----------------------------------------------------------------

FUA_decile_plot_FUA_CompInd <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  # calculate summary for comp index
  CI_deciles_Y1_summary <- CI_deciles_Y1 %>% 
    drop_na(built_index_rank_deciles) %>% 
    group_by(fuaname, built_index_rank_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y1_summary$year <- "2001"
  
  CI_deciles_Y2_summary <- CI_deciles_Y2 %>% 
    drop_na(built_index_rank_deciles) %>% 
    group_by(fuaname, built_index_rank_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y2_summary$year <- "2006"
  
  CI_deciles_Y3_summary <- CI_deciles_Y3 %>% 
    drop_na(built_index_rank_deciles) %>% 
    group_by(fuaname, built_index_rank_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y3_summary$year <- "2011"
  
  CI_deciles_Y4_summary <- CI_deciles_Y4 %>% 
    drop_na(built_index_rank_deciles) %>% 
    group_by(fuaname, built_index_rank_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y4_summary$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1_summary, CI_deciles_Y2_summary,
                               CI_deciles_Y3_summary, CI_deciles_Y4_summary)
  
  # this will be pritned in the result
  CI_deciles_FUA <- CI_deciles_combined
  
  CI_deciles_combined$built_index_rank_deciles <- as.character(CI_deciles_combined$built_index_rank_deciles)
  
  # Specify the colours for each decile
  cpal = c(
    "10" = "#800026",
    "9" = "#B60026",
    "8" = "#DA141E",
    "7" = "#F33C25",
    "6" = "#FC7134",
    "5" = "#FD9D43",
    "4" = "#FEBE59",
    "3" = "#FEDD7F",
    "2" = "#FFEFA4",
    "1" = "#FFFFCC"
  )
  
  
  # for a specific order
  CI_deciles_combined$built_index_rank_deciles <- factor(CI_deciles_combined$built_index_rank_deciles,
                                                         levels=c("1", "2", "3", "4","5",
                                                                  "6", "7", "8", "9", "10"))
  
  CI_deciles_combined_16 <- subset(CI_deciles_combined, year== '2016')
  
  
  # reformat the data to order them by 10th decile frequency
  data_wide <- dcast(CI_deciles_combined_16, fuaname ~ built_index_rank_deciles, value.var="freq")
  data_wide<- data_wide[order(-data_wide$`1`,
                              -data_wide$`2`
                              -data_wide$`3`
                              -data_wide$`4`
                              -data_wide$`5`
                              -data_wide$`6`
                              -data_wide$`7`
                              -data_wide$`8`
                              -data_wide$`9`,
                              -data_wide$`10`),]
  # create a new column with the new order
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(data_wide$fuaname))
  
  # Create the plot showing the FUA distribution for composite index
  p1 <- ggplot() + 
    geom_bar(aes(y =  freq, x = year, fill = built_index_rank_deciles), data = CI_deciles_combined,  stat="identity")+
    labs(title = paste0("Sustainable Urban Development Index distribution by FUA"), 
         x = "", y = "", fill = "") +
    scale_fill_manual(values = cpal, na.translate = FALSE) +
    facet_wrap(~fuaname_re) +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 90, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_line('white', size = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  # create a list with the results
  results <- list("Index_plot" = p1, 
                  "Deciles_FUA_summary" = CI_deciles_FUA)
  
  return(results)
  
  
}



# plot for compactness domain --------------------------------------------------
FUA_decile_plot_FUA_Compactness <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  # calculate summary for comp index
  CI_deciles_Y1_summary <- CI_deciles_Y1 %>% 
    drop_na(Cmp_rnk_deciles) %>% 
    group_by(fuaname, Cmp_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y1_summary$year <- "2001"
  
  CI_deciles_Y2_summary <- CI_deciles_Y2 %>% 
    drop_na(Cmp_rnk_deciles) %>% 
    group_by(fuaname, Cmp_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y2_summary$year <- "2006"
  
  CI_deciles_Y3_summary <- CI_deciles_Y3 %>% 
    drop_na(Cmp_rnk_deciles) %>% 
    group_by(fuaname, Cmp_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y3_summary$year <- "2011"
  
  CI_deciles_Y4_summary <- CI_deciles_Y4 %>% 
    drop_na(Cmp_rnk_deciles) %>% 
    group_by(fuaname, Cmp_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y4_summary$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1_summary, CI_deciles_Y2_summary,
                               CI_deciles_Y3_summary, CI_deciles_Y4_summary)
  
  # this will be pritned in the result
  CI_deciles_FUA <- CI_deciles_combined
  
  CI_deciles_combined$Cmp_rnk_deciles <- as.character(CI_deciles_combined$Cmp_rnk_deciles)
  
  # Specify the colours for each decile
  cpal = c(
    "10" = "#800026",
    "9" = "#B60026",
    "8" = "#DA141E",
    "7" = "#F33C25",
    "6" = "#FC7134",
    "5" = "#FD9D43",
    "4" = "#FEBE59",
    "3" = "#FEDD7F",
    "2" = "#FFEFA4",
    "1" = "#FFFFCC"
  )
  
  
  # for a specific order
  CI_deciles_combined$Cmp_rnk_deciles <- factor(CI_deciles_combined$Cmp_rnk_deciles,
                                                levels=c("1", "2", "3", "4","5",
                                                         "6", "7", "8", "9", "10"))
  
  CI_deciles_combined_16 <- subset(CI_deciles_combined, year== '2016')
  
  
  # reformat the data to order them by 10th decile frequency
  data_wide <- dcast(CI_deciles_combined_16, fuaname ~ Cmp_rnk_deciles, value.var="freq")
  data_wide<- data_wide[order(-data_wide$`1`,
                              -data_wide$`2`
                              -data_wide$`3`
                              -data_wide$`4`
                              -data_wide$`5`
                              -data_wide$`6`
                              -data_wide$`7`
                              -data_wide$`8`
                              -data_wide$`9`,
                              -data_wide$`10`),]
  # create a new column with the new order
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(data_wide$fuaname))
  
  # Create the plot showing the FUA distribution for composite index
  p1 <- ggplot() + 
    geom_bar(aes(y =  freq, x = year, fill = Cmp_rnk_deciles), data = CI_deciles_combined,  stat="identity")+
    labs(title = paste0("Compactness domain distribution by FUA"), 
         x = "", y = "", fill = "") +
    scale_fill_manual(values = cpal, na.translate = FALSE) +
    facet_wrap(~fuaname_re) +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 90, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_line('white', size = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  
  
  # create a list with the results
  results <- list("Index_plot" = p1, 
                  "Deciles_FUA_summary" = CI_deciles_FUA)
  
  return(results)
  
}


# plot for green space domain --------------------------------------------------
FUA_decile_plot_FUA_GreenSp <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  # calculate summary for comp index
  CI_deciles_Y1_summary <- CI_deciles_Y1 %>% 
    drop_na(GreenSp_deciles) %>% 
    group_by(fuaname, GreenSp_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y1_summary$year <- "2001"
  
  CI_deciles_Y2_summary <- CI_deciles_Y2 %>% 
    drop_na(GreenSp_deciles) %>% 
    group_by(fuaname, GreenSp_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y2_summary$year <- "2006"
  
  CI_deciles_Y3_summary <- CI_deciles_Y3 %>% 
    drop_na(GreenSp_deciles) %>% 
    group_by(fuaname, GreenSp_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y3_summary$year <- "2011"
  
  CI_deciles_Y4_summary <- CI_deciles_Y4 %>% 
    drop_na(GreenSp_deciles) %>% 
    group_by(fuaname, GreenSp_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y4_summary$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1_summary, CI_deciles_Y2_summary,
                               CI_deciles_Y3_summary, CI_deciles_Y4_summary)
  
  # this will be pritned in the result
  CI_deciles_FUA <- CI_deciles_combined
  
  CI_deciles_combined$GreenSp_deciles <- as.character(CI_deciles_combined$GreenSp_deciles)
  
  # Specify the colours for each decile
  cpal = c(
    "10" = "#800026",
    "9" = "#B60026",
    "8" = "#DA141E",
    "7" = "#F33C25",
    "6" = "#FC7134",
    "5" = "#FD9D43",
    "4" = "#FEBE59",
    "3" = "#FEDD7F",
    "2" = "#FFEFA4",
    "1" = "#FFFFCC"
  )
  
  
  # for a specific order
  CI_deciles_combined$GreenSp_deciles <- factor(CI_deciles_combined$GreenSp_deciles,
                                                levels=c("1", "2", "3", "4","5",
                                                         "6", "7", "8", "9", "10"))
  
  CI_deciles_combined_16 <- subset(CI_deciles_combined, year== '2016')
  
  
  # reformat the data to order them by 10th decile frequency
  data_wide <- dcast(CI_deciles_combined_16, fuaname ~ GreenSp_deciles, value.var="freq")
  data_wide<- data_wide[order(-data_wide$`1`,
                              -data_wide$`2`
                              -data_wide$`3`
                              -data_wide$`4`
                              -data_wide$`5`
                              -data_wide$`6`
                              -data_wide$`7`
                              -data_wide$`8`
                              -data_wide$`9`,
                              -data_wide$`10`),]
  # create a new column with the new order
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(data_wide$fuaname))
  
  # Create the plot showing the FUA distribution for composite index
  p1 <- ggplot() + 
    geom_bar(aes(y =  freq, x = year, fill = GreenSp_deciles), data = CI_deciles_combined,  stat="identity")+
    labs(title = paste0("Green space domain distribution by FUA"), 
         x = "", y = "", fill = "") +
    scale_fill_manual(values = cpal, na.translate = FALSE) +
    facet_wrap(~fuaname_re) +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 90, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_line('white', size = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  
  
  # create a list with the results
  results <- list("Index_plot" = p1, 
                  "Deciles_FUA_summary" = CI_deciles_FUA)
  
  return(results)
  
}


# plot for walkability domain --------------------------------------------------
FUA_decile_plot_FUA_Walkability <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  # calculate summary for comp index
  CI_deciles_Y1_summary <- CI_deciles_Y1 %>% 
    drop_na(Wlkb_rnk_deciles) %>% 
    group_by(fuaname, Wlkb_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y1_summary$year <- "2001"
  
  CI_deciles_Y2_summary <- CI_deciles_Y2 %>% 
    drop_na(Wlkb_rnk_deciles) %>% 
    group_by(fuaname, Wlkb_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y2_summary$year <- "2006"
  
  CI_deciles_Y3_summary <- CI_deciles_Y3 %>% 
    drop_na(Wlkb_rnk_deciles) %>% 
    group_by(fuaname, Wlkb_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y3_summary$year <- "2011"
  
  CI_deciles_Y4_summary <- CI_deciles_Y4 %>% 
    drop_na(Wlkb_rnk_deciles) %>% 
    group_by(fuaname, Wlkb_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y4_summary$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1_summary, CI_deciles_Y2_summary,
                               CI_deciles_Y3_summary, CI_deciles_Y4_summary)
  
  # this will be pritned in the result
  CI_deciles_FUA <- CI_deciles_combined
  
  CI_deciles_combined$Wlkb_rnk_deciles <- as.character(CI_deciles_combined$Wlkb_rnk_deciles)
  
  # Specify the colours for each decile
  cpal = c(
    "10" = "#800026",
    "9" = "#B60026",
    "8" = "#DA141E",
    "7" = "#F33C25",
    "6" = "#FC7134",
    "5" = "#FD9D43",
    "4" = "#FEBE59",
    "3" = "#FEDD7F",
    "2" = "#FFEFA4",
    "1" = "#FFFFCC"
  )
  
  
  # for a specific order
  CI_deciles_combined$Wlkb_rnk_deciles <- factor(CI_deciles_combined$Wlkb_rnk_deciles,
                                                 levels=c("1", "2", "3", "4","5",
                                                          "6", "7", "8", "9", "10"))
  
  CI_deciles_combined_16 <- subset(CI_deciles_combined, year== '2016')
  
  
  # reformat the data to order them by 10th decile frequency
  data_wide <- dcast(CI_deciles_combined_16, fuaname ~ Wlkb_rnk_deciles, value.var="freq")
  data_wide<- data_wide[order(-data_wide$`1`,
                              -data_wide$`2`
                              -data_wide$`3`
                              -data_wide$`4`
                              -data_wide$`5`
                              -data_wide$`6`
                              -data_wide$`7`
                              -data_wide$`8`
                              -data_wide$`9`,
                              -data_wide$`10`),]
  # create a new column with the new order
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(data_wide$fuaname))
  
  # Create the plot showing the FUA distribution for composite index
  p1 <- ggplot() + 
    geom_bar(aes(y =  freq, x = year, fill = Wlkb_rnk_deciles), data = CI_deciles_combined,  stat="identity")+
    labs(title = paste0("Walkability domain distribution by FUA"), 
         x = "", y = "", fill = "") +
    scale_fill_manual(values = cpal, na.translate = FALSE) +
    facet_wrap(~fuaname_re) +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 90, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_line('white', size = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  
  
  # create a list with the results
  results <- list("Index_plot" = p1, 
                  "Deciles_FUA_summary" = CI_deciles_FUA)
  
  return(results)
  
}


line_FUA_domain_plot <- function(input_summary, plot_title, decile_num){
  
  # here we subset based on the decile number which is always in the second column
  input_summary <- input_summary[input_summary[ ,2] == decile_num, ]
  
  # this will help with plotting the data
  input_summary$year <- as.numeric(input_summary$year)
  
  ggplot(input_summary, aes(x = year, y = freq, group = fuaname)) +
    geom_line(size = 1, colour = "grey40") + 
    geom_point() +
    xlab("") +
    ylab("") +
    coord_trans(y = "log10") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks=c(2001,2006,2011, 2016),
                       labels=c('2001','2006','2011', '2016'), expand = c(0.6, 0)) +
    #scale_colour_manual(values = cpal) +
    #scale_colour_brewer(palette="Accent") +
    labs(title = plot_title,
         colour = "") +
    theme_minimal() +
    geom_dl(aes(label = fuaname), method = list('last.bumpup', cex = 1.3, fontfamily="Avenir Next Condensed")) + # help to avoid overlap otherwise we can also use "first.points", "last.points"
    theme(text = element_text(size = 16, family = 'Avenir Next Condensed'), 
          legend.position = "none")
  
}




# Box plots ---------------------------------------------------------------

# box plot for composite index --------------------------------------------------
FUA_decile_plot_FUA_CompInd_boxplot <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  CI_deciles_Y1$year <- "2001"
  CI_deciles_Y2$year <- "2006"
  CI_deciles_Y3$year <- "2011"
  CI_deciles_Y4$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1, CI_deciles_Y2,
                               CI_deciles_Y3, CI_deciles_Y4)
  
  
  
  # for a specific order
  
  median_sorted <- CI_deciles_Y4 %>%
    group_by(fuaname) %>%
    dplyr::summarize(Median = median(built_index_rank_deciles, na.rm=TRUE)) %>% 
    arrange(Median)
  
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(median_sorted$fuaname))
  
  
  p1 <-ggplot(CI_deciles_combined, aes(x=year, y=built_index_rank_deciles)) + 
    geom_boxplot()+
    labs(title = paste0("Sustainable Urban Development Index distribution by FUA"), 
         x = "", y = "", fill = "") +
    facet_wrap(~fuaname_re) +
    scale_y_continuous(breaks = round(seq(1, 10, by = 1),1)) +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 45, hjust = 1, size = 16),
      axis.text.y=element_text(size = 12),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(colour = "black", size=1, fill=NA)
    )
  
  
  
  
  # create a list with the results
  results <- list("Index_plot" = p1)
  
  return(results)
  
}



# box plot for compactness domain --------------------------------------------------
FUA_decile_plot_FUA_Compactness_boxplot <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  CI_deciles_Y1$year <- "2001"
  CI_deciles_Y2$year <- "2006"
  CI_deciles_Y3$year <- "2011"
  CI_deciles_Y4$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1, CI_deciles_Y2,
                               CI_deciles_Y3, CI_deciles_Y4)
  
  
  
  # for a specific order
  
  median_sorted <- CI_deciles_Y4 %>%
    group_by(fuaname) %>%
    dplyr::summarize(Median = median(Cmp_rnk_deciles, na.rm=TRUE)) %>% 
    arrange(Median)
  
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(median_sorted$fuaname))
  
  
  p1 <- ggplot(CI_deciles_combined, aes(x=year, y=Cmp_rnk_deciles)) + 
    geom_boxplot()+
    labs(title = paste0("Compactness domain distribution by FUA"), 
         x = "", y = "", fill = "") +
    facet_wrap(~fuaname_re) +
    scale_y_continuous(breaks = round(seq(1, 10, by = 1),1)) +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 45, hjust = 1, size = 16),
      axis.text.y=element_text(size = 12),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(colour = "black", size=1, fill=NA)
    )
  
  
  
  
  # create a list with the results
  results <- list("Index_plot" = p1)
  
  return(results)
  
}

# box plot for green space domain --------------------------------------------------
FUA_decile_plot_FUA_GreenSp_boxplot <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  CI_deciles_Y1$year <- "2001"
  CI_deciles_Y2$year <- "2006"
  CI_deciles_Y3$year <- "2011"
  CI_deciles_Y4$year <- "2016"

  CI_deciles_combined <- rbind(CI_deciles_Y1, CI_deciles_Y2,
                               CI_deciles_Y3, CI_deciles_Y4)
  
  
  
  # for a specific order
  
  median_sorted <- CI_deciles_Y4 %>%
    group_by(fuaname) %>%
    dplyr::summarize(Median = median(GreenSp_deciles, na.rm=TRUE)) %>% 
    arrange(Median)
  
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(median_sorted$fuaname))
  
  
  p1 <- ggplot(CI_deciles_combined, aes(x=year, y=GreenSp_deciles)) + 
    geom_boxplot()+
    labs(title = paste0("Green Space domain distribution by FUA"), 
         x = "", y = "", fill = "") +
    facet_wrap(~fuaname_re) +
    scale_y_continuous(breaks = round(seq(1, 10, by = 1),1)) +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 45, hjust = 1, size = 16),
      axis.text.y=element_text(size = 12),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(colour = "black", size=1, fill=NA)
    )
  
  
  
  
  # create a list with the results
  results <- list("Index_plot" = p1)
  
  return(results)
  
}


# box plot for walkability domain --------------------------------------------------
FUA_decile_plot_FUA_Walkability_boxplot <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  CI_deciles_Y1$year <- "2001"
  CI_deciles_Y2$year <- "2006"
  CI_deciles_Y3$year <- "2011"
  CI_deciles_Y4$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1, CI_deciles_Y2,
                               CI_deciles_Y3, CI_deciles_Y4)
  
  
  
  # for a specific order
  
  median_sorted <- CI_deciles_Y4 %>%
    group_by(fuaname) %>%
    dplyr::summarize(Median = median(Wlkb_rnk_deciles, na.rm=TRUE)) %>% 
    arrange(Median)
  
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(median_sorted$fuaname))
  
  
  p1 <- ggplot(CI_deciles_combined, aes(x=year, y=Wlkb_rnk_deciles)) + 
    geom_boxplot()+
    labs(title = paste0("Walkability domain distribution by FUA"), 
         x = "", y = "", fill = "") +
    facet_wrap(~fuaname_re) +
    scale_y_continuous(breaks = round(seq(1, 10, by = 1),1)) +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 45, hjust = 1, size = 16),
      axis.text.y=element_text(size = 12),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(colour = "black", size=1, fill=NA)
    )
  
  
  
  
  # create a list with the results
  results <- list("Index_plot" = p1)
  
  return(results)
  
}


# Distribution line plots ---------------------------------------------------------------

# line plot for composite index --------------------------------------------------
FUA_decile_plot_FUA_CompInd_line_plot <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  # calculate summary for comp index
  CI_deciles_Y1_summary <- CI_deciles_Y1 %>% 
    drop_na(built_index_rank_deciles) %>% 
    group_by(fuaname, built_index_rank_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y1_summary$year <- "2001"
  
  CI_deciles_Y2_summary <- CI_deciles_Y2 %>% 
    drop_na(built_index_rank_deciles) %>% 
    group_by(fuaname, built_index_rank_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y2_summary$year <- "2006"
  
  CI_deciles_Y3_summary <- CI_deciles_Y3 %>% 
    drop_na(built_index_rank_deciles) %>% 
    group_by(fuaname, built_index_rank_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y3_summary$year <- "2011"
  
  CI_deciles_Y4_summary <- CI_deciles_Y4 %>% 
    drop_na(built_index_rank_deciles) %>% 
    group_by(fuaname, built_index_rank_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y4_summary$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1_summary, CI_deciles_Y2_summary,
                               CI_deciles_Y3_summary, CI_deciles_Y4_summary)
  
  # this will be pritned in the result
  CI_deciles_FUA <- CI_deciles_combined
  
  
  CI_deciles_combined_16 <- subset(CI_deciles_combined, year== '2016')
  
  
  # reformat the data to order them by 10th decile frequency
  data_wide <- dcast(CI_deciles_combined_16, fuaname ~ built_index_rank_deciles, value.var="freq")
  data_wide<- data_wide[order(-data_wide$`1`,
                              -data_wide$`2`
                              -data_wide$`3`
                              -data_wide$`4`
                              -data_wide$`5`
                              -data_wide$`6`
                              -data_wide$`7`
                              -data_wide$`8`
                              -data_wide$`9`,
                              -data_wide$`10`),]
  # create a new column with the new order
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(data_wide$fuaname))
  
  # Create the plot showing the FUA distribution for composite index
  p1 <- ggplot(data=CI_deciles_combined, aes(x=built_index_rank_deciles, y= freq, colour=year)) +
    geom_smooth(method = "loess", se = FALSE, size=1, span = 0.7) +
    labs(title = paste0("Sustainable Urban Development Index distribution by FUA"), 
         x = "", y = "", colour = "Year") +
    scale_x_discrete(limits=c("1","2","3", "4","5","6", "7","8","9", "10"))+
    facet_wrap(~ fuaname_re) +
    scale_color_brewer(palette="Set1") +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  
  # create a list with the results
  results <- list("Index_plot" = p1, 
                  "Deciles_FUA_summary" = CI_deciles_FUA)
  
  return(results)
  
  
}

# line plot for compactness --------------------------------------------------
FUA_decile_plot_FUA_Compactness_line_plot <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  # calculate summary for comp index
  CI_deciles_Y1_summary <- CI_deciles_Y1 %>% 
    drop_na(Cmp_rnk_deciles) %>% 
    group_by(fuaname, Cmp_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y1_summary$year <- "2001"
  
  CI_deciles_Y2_summary <- CI_deciles_Y2 %>% 
    drop_na(Cmp_rnk_deciles) %>% 
    group_by(fuaname, Cmp_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y2_summary$year <- "2006"
  
  CI_deciles_Y3_summary <- CI_deciles_Y3 %>% 
    drop_na(Cmp_rnk_deciles) %>% 
    group_by(fuaname, Cmp_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y3_summary$year <- "2011"
  
  CI_deciles_Y4_summary <- CI_deciles_Y4 %>% 
    drop_na(Cmp_rnk_deciles) %>% 
    group_by(fuaname, Cmp_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y4_summary$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1_summary, CI_deciles_Y2_summary,
                               CI_deciles_Y3_summary, CI_deciles_Y4_summary)
  
  # this will be pritned in the result
  CI_deciles_FUA <- CI_deciles_combined
  
  
  CI_deciles_combined_16 <- subset(CI_deciles_combined, year== '2016')
  
  
  # reformat the data to order them by 10th decile frequency
  data_wide <- dcast(CI_deciles_combined_16, fuaname ~ Cmp_rnk_deciles, value.var="freq")
  data_wide<- data_wide[order(-data_wide$`1`,
                              -data_wide$`2`
                              -data_wide$`3`
                              -data_wide$`4`
                              -data_wide$`5`
                              -data_wide$`6`
                              -data_wide$`7`
                              -data_wide$`8`
                              -data_wide$`9`,
                              -data_wide$`10`),]
  # create a new column with the new order
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(data_wide$fuaname))
  
  # Create the plot showing the FUA distribution for composite index
  p1 <- ggplot(data=CI_deciles_combined, aes(x=Cmp_rnk_deciles, y= freq, colour=year)) +
    geom_smooth(method = "loess", se = FALSE, size=1, span = 0.7) +
    labs(title = paste0("Compactness domain distribution by FUA"), 
         x = "", y = "", colour = "Year") +
    scale_x_discrete(limits=c("1","2","3", "4","5","6", "7","8","9", "10"))+
    facet_wrap(~ fuaname_re) +
    scale_color_brewer(palette="Set1") +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  
  # create a list with the results
  results <- list("Index_plot" = p1, 
                  "Deciles_FUA_summary" = CI_deciles_FUA)
  
  return(results)
  
  
}


# line plot for green space --------------------------------------------------
FUA_decile_plot_FUA_GreenSp_line_plot <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  # calculate summary for comp index
  CI_deciles_Y1_summary <- CI_deciles_Y1 %>% 
    drop_na(GreenSp_deciles) %>% 
    group_by(fuaname, GreenSp_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y1_summary$year <- "2001"
  
  CI_deciles_Y2_summary <- CI_deciles_Y2 %>% 
    drop_na(GreenSp_deciles) %>% 
    group_by(fuaname, GreenSp_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y2_summary$year <- "2006"
  
  CI_deciles_Y3_summary <- CI_deciles_Y3 %>% 
    drop_na(GreenSp_deciles) %>% 
    group_by(fuaname, GreenSp_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y3_summary$year <- "2011"
  
  CI_deciles_Y4_summary <- CI_deciles_Y4 %>% 
    drop_na(GreenSp_deciles) %>% 
    group_by(fuaname, GreenSp_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y4_summary$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1_summary, CI_deciles_Y2_summary,
                               CI_deciles_Y3_summary, CI_deciles_Y4_summary)
  
  # this will be pritned in the result
  CI_deciles_FUA <- CI_deciles_combined
  
  
  CI_deciles_combined_16 <- subset(CI_deciles_combined, year== '2016')
  
  
  # reformat the data to order them by 10th decile frequency
  data_wide <- dcast(CI_deciles_combined_16, fuaname ~ GreenSp_deciles, value.var="freq")
  data_wide<- data_wide[order(-data_wide$`1`,
                              -data_wide$`2`
                              -data_wide$`3`
                              -data_wide$`4`
                              -data_wide$`5`
                              -data_wide$`6`
                              -data_wide$`7`
                              -data_wide$`8`
                              -data_wide$`9`,
                              -data_wide$`10`),]
  # create a new column with the new order
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(data_wide$fuaname))
  
  # Create the plot showing the FUA distribution for composite index
  p1 <- ggplot(data=CI_deciles_combined, aes(x=GreenSp_deciles, y= freq, colour=year)) +
    geom_smooth(method = "loess", se = FALSE, size=1, span = 0.7) +
    labs(title = paste0("Green space domain distribution by FUA"), 
         x = "", y = "", colour = "Year") +
    scale_x_discrete(limits=c("1","2","3", "4","5","6", "7","8","9", "10"))+
    facet_wrap(~ fuaname_re) +
    scale_color_brewer(palette="Set1") +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  
  # create a list with the results
  results <- list("Index_plot" = p1, 
                  "Deciles_FUA_summary" = CI_deciles_FUA)
  
  return(results)
  
  
}


# line plot for walkability --------------------------------------------------
FUA_decile_plot_FUA_Walkability_line_plot <- function(CI_deciles_Y1, CI_deciles_Y2, CI_deciles_Y3, CI_deciles_Y4) {
  
  # calculate summary for comp index
  CI_deciles_Y1_summary <- CI_deciles_Y1 %>% 
    drop_na(Wlkb_rnk_deciles) %>% 
    group_by(fuaname, Wlkb_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y1_summary$year <- "2001"
  
  CI_deciles_Y2_summary <- CI_deciles_Y2 %>% 
    drop_na(Wlkb_rnk_deciles) %>% 
    group_by(fuaname, Wlkb_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y2_summary$year <- "2006"
  
  CI_deciles_Y3_summary <- CI_deciles_Y3 %>% 
    drop_na(Wlkb_rnk_deciles) %>% 
    group_by(fuaname, Wlkb_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y3_summary$year <- "2011"
  
  CI_deciles_Y4_summary <- CI_deciles_Y4 %>% 
    drop_na(Wlkb_rnk_deciles) %>% 
    group_by(fuaname, Wlkb_rnk_deciles) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) 
  
  CI_deciles_Y4_summary$year <- "2016"
  
  CI_deciles_combined <- rbind(CI_deciles_Y1_summary, CI_deciles_Y2_summary,
                               CI_deciles_Y3_summary, CI_deciles_Y4_summary)
  
  # this will be pritned in the result
  CI_deciles_FUA <- CI_deciles_combined
  
  
  CI_deciles_combined_16 <- subset(CI_deciles_combined, year== '2016')
  
  
  # reformat the data to order them by 10th decile frequency
  data_wide <- dcast(CI_deciles_combined_16, fuaname ~ Wlkb_rnk_deciles, value.var="freq")
  data_wide<- data_wide[order(-data_wide$`1`,
                              -data_wide$`2`
                              -data_wide$`3`
                              -data_wide$`4`
                              -data_wide$`5`
                              -data_wide$`6`
                              -data_wide$`7`
                              -data_wide$`8`
                              -data_wide$`9`,
                              -data_wide$`10`),]
  # create a new column with the new order
  CI_deciles_combined$fuaname_re = factor(CI_deciles_combined$fuaname, levels=c(data_wide$fuaname))
  
  # Create the plot showing the FUA distribution for composite index
  p1 <- ggplot(data=CI_deciles_combined, aes(x=Wlkb_rnk_deciles, y= freq, colour=year)) +
    geom_smooth(method = "loess", se = FALSE, size=1, span = 0.7) +
    labs(title = paste0("Walkability domain distribution by FUA"), 
         x = "", y = "", colour = "Year") +
    scale_x_discrete(limits=c("1","2","3", "4","5","6", "7","8","9", "10"))+
    facet_wrap(~ fuaname_re) +
    scale_color_brewer(palette="Set1") +
    theme_minimal() +
    theme(
      text = element_text('Avenir Next Condensed'),
      axis.text.x=element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = 'bold', hjust = 0),
      plot.caption = element_text(face = 'italic'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.ontop = TRUE
    )
  
  
  # create a list with the results
  results <- list("Index_plot" = p1, 
                  "Deciles_FUA_summary" = CI_deciles_FUA)
  
  return(results)
  
  
}

