library(sf)

# 1. Read in the FUA grids
FUAs <- st_read("../data/layers/grids.gpkg")

# if we want we can subset the dataset only to the FUA that we are interested
FUAs <- subset(FUAs, fuaname %in% c("London","Manchester",
                                    "West Midlands urban area","Leeds",
                                    "Glasgow","Liverpool",
                                    "Southampton","Newcastle upon Tyne",
                                    "Nottingham","Sheffield",
                                    "Bristol","Edinburgh"))

# use Birmingham as it is easier to report
FUAs$fuaname[FUAs$fuaname == "West Midlands urban area"] <- "Birmingham"





# we need these libraries for the composite index function
library(dplyr)
library(psych)
library(tidyr) # this is used for drop_na()
library(RColorBrewer)
library(leaflet)
library(leaflet.extras)


# source the functions for composite index function
source('Composite_index_function.R')

# source the functions for creating plots
source('plot_functions.R')

# source for helper functions
source('helpers.R')




# function for creating decile plots by FUA --------------------------------
# this function needs
library(ggplot2)
library(reshape2)


# Read all the RDS files with gridded data ------------------------

data_2001 <- readRDS(file = "../data/gridded_data/full_data_2001.Rds")
data_2006 <- readRDS(file = "../data/gridded_data/full_data_2006.Rds")
data_2011 <- readRDS(file = "../data/gridded_data/full_data_2011.Rds")
data_2016 <- readRDS(file = "../data/gridded_data/full_data_2016.Rds")

# treat the dataset as dataframe
st_geometry(data_2001) <- NULL
st_geometry(data_2006) <- NULL
st_geometry(data_2011) <- NULL
st_geometry(data_2016) <- NULL


CI_2001 <- Comp_ind(data_2001, FUAs, "2001")
CI_2006 <- Comp_ind(data_2006, FUAs, "2006")
CI_2011 <- Comp_ind(data_2011, FUAs, "2011")
CI_2016 <- Comp_ind(data_2016, FUAs, "2016")




#run the function to produce plots
plots_2001 <- FUA_decile_plot(CI_2001$Comp_ind, "2001")
plots_2006 <- FUA_decile_plot(CI_2006$Comp_ind, "2006") 
plots_2011 <- FUA_decile_plot(CI_2011$Comp_ind, "2011") 
plots_2016 <- FUA_decile_plot(CI_2016$Comp_ind, "2016") 



# Calculate average of indicators by decile -------------------------------
# create datasets with composite index deciles and indicators
CI_deciles01 <- plots_2001$Grids_deciles
CI_deciles06 <- plots_2006$Grids_deciles
CI_deciles11 <- plots_2011$Grids_deciles
CI_deciles16 <- plots_2016$Grids_deciles


# change the name to distinguish years -------------------------------------------
names(CI_deciles01)[names(CI_deciles01)=="built_index_rank_deciles"] <- "built_index_rank_deciles_01"
names(CI_deciles06)[names(CI_deciles06)=="built_index_rank_deciles"] <- "built_index_rank_deciles_06"
names(CI_deciles11)[names(CI_deciles11)=="built_index_rank_deciles"] <- "built_index_rank_deciles_11"
names(CI_deciles16)[names(CI_deciles16)=="built_index_rank_deciles"] <- "built_index_rank_deciles_16"


names(CI_deciles01)[names(CI_deciles01)=="Cmp_rnk_deciles"] <- "Cmp_rnk_deciles_01"
names(CI_deciles06)[names(CI_deciles06)=="Cmp_rnk_deciles"] <- "Cmp_rnk_deciles_06"
names(CI_deciles11)[names(CI_deciles11)=="Cmp_rnk_deciles"] <- "Cmp_rnk_deciles_11"
names(CI_deciles16)[names(CI_deciles16)=="Cmp_rnk_deciles"] <- "Cmp_rnk_deciles_16"

names(CI_deciles01)[names(CI_deciles01)=="GreenSp_deciles"] <- "GreenSp_deciles_01"
names(CI_deciles06)[names(CI_deciles06)=="GreenSp_deciles"] <- "GreenSp_deciles_06"
names(CI_deciles11)[names(CI_deciles11)=="GreenSp_deciles"] <- "GreenSp_deciles_11"
names(CI_deciles16)[names(CI_deciles16)=="GreenSp_deciles"] <- "GreenSp_deciles_16"

names(CI_deciles01)[names(CI_deciles01)=="Wlkb_rnk_deciles"] <- "Wlkb_rnk_deciles_01"
names(CI_deciles06)[names(CI_deciles06)=="Wlkb_rnk_deciles"] <- "Wlkb_rnk_deciles_06"
names(CI_deciles11)[names(CI_deciles11)=="Wlkb_rnk_deciles"] <- "Wlkb_rnk_deciles_11"
names(CI_deciles16)[names(CI_deciles16)=="Wlkb_rnk_deciles"] <- "Wlkb_rnk_deciles_16"


# this function returns a dataset with ID and index
get_index_only <- function(deciles_sum, col_needed) {
  results <- deciles_sum[, c(
    'ID',
    col_needed
  )]
  
  return(results)
}

Comp_ind_2001 <- get_index_only(CI_deciles01, 'built_index_rank_deciles_01')
Comp_ind_2006 <- get_index_only(CI_deciles06, 'built_index_rank_deciles_06')
Comp_ind_2011 <- get_index_only(CI_deciles11, 'built_index_rank_deciles_11')
Comp_ind_2016 <- get_index_only(CI_deciles16, 'built_index_rank_deciles_16')

cmp_domain_2001 <- get_index_only(CI_deciles01, 'Cmp_rnk_deciles_01')
cmp_domain_2006 <- get_index_only(CI_deciles06, 'Cmp_rnk_deciles_06')
cmp_domain_2011 <- get_index_only(CI_deciles11, 'Cmp_rnk_deciles_11')
cmp_domain_2016 <- get_index_only(CI_deciles16, 'Cmp_rnk_deciles_16')

GreenSp_domain_2001 <- get_index_only(CI_deciles01, 'GreenSp_deciles_01')
GreenSp_domain_2006 <- get_index_only(CI_deciles06, 'GreenSp_deciles_06')
GreenSp_domain_2011 <- get_index_only(CI_deciles11, 'GreenSp_deciles_11')
GreenSp_domain_2016 <- get_index_only(CI_deciles16, 'GreenSp_deciles_16')

Wlkb_domain_2001 <- get_index_only(CI_deciles01, 'Wlkb_rnk_deciles_01')
Wlkb_domain_2006 <- get_index_only(CI_deciles06, 'Wlkb_rnk_deciles_06')
Wlkb_domain_2011 <- get_index_only(CI_deciles11, 'Wlkb_rnk_deciles_11')
Wlkb_domain_2016 <- get_index_only(CI_deciles16, 'Wlkb_rnk_deciles_16')



# this functions give a dataset with all the deciles by year
get_all_years_deciles <- function(dec_year1, dec_year2, dec_year3, dec_year4) {
  one_year_data <- left_join(FUAs, dec_year1, by = "ID")
  two_year_data <- left_join(one_year_data, dec_year2, by = "ID")
  three_year_data <- left_join(two_year_data, dec_year3, by = "ID")
  four_year_data <- left_join(three_year_data, dec_year4, by = "ID")
  
  return(four_year_data)
  
}


Comp_ind_all_years <- get_all_years_deciles(Comp_ind_2001, Comp_ind_2006, Comp_ind_2011, Comp_ind_2016)
cmp_domain_all_years <- get_all_years_deciles(cmp_domain_2001, cmp_domain_2006, cmp_domain_2011, cmp_domain_2016)
GreenSp_domain_all_years <- get_all_years_deciles(GreenSp_domain_2001, GreenSp_domain_2006, GreenSp_domain_2011, GreenSp_domain_2016)
Wlkb_domain_all_years <- get_all_years_deciles(Wlkb_domain_2001, Wlkb_domain_2006, Wlkb_domain_2011, Wlkb_domain_2016)



# drop the rows that do not contain values in all four years
# I don't think there are grids that miising one year. They either have all 4 or none
Comp_ind_all_years <- Comp_ind_all_years %>%
  drop_na(built_index_rank_deciles_01, built_index_rank_deciles_06, 
          built_index_rank_deciles_11, built_index_rank_deciles_16)

cmp_domain_all_years <- cmp_domain_all_years %>%
  drop_na(Cmp_rnk_deciles_01, Cmp_rnk_deciles_06, 
          Cmp_rnk_deciles_11, Cmp_rnk_deciles_16)

GreenSp_domain_all_years <- GreenSp_domain_all_years %>%
  drop_na(GreenSp_deciles_01, GreenSp_deciles_06, 
          GreenSp_deciles_11, GreenSp_deciles_16)

Wlkb_domain_all_years <- Wlkb_domain_all_years %>%
  drop_na(Wlkb_rnk_deciles_01, Wlkb_rnk_deciles_06, 
          Wlkb_rnk_deciles_11, Wlkb_rnk_deciles_16)





# calculate difference between 2001 and 2016 rankings
Comp_ind_all_years$rank_dif_01_16 <- Comp_ind_all_years$built_index_rank_deciles_16 - Comp_ind_all_years$built_index_rank_deciles_01
# calculate difference between 2001 and 2006 rankings
Comp_ind_all_years$rank_dif_01_06 <- Comp_ind_all_years$built_index_rank_deciles_06 - Comp_ind_all_years$built_index_rank_deciles_01
# calculate difference between 2006 and 2011 rankings
Comp_ind_all_years$rank_dif_06_11 <- Comp_ind_all_years$built_index_rank_deciles_11 - Comp_ind_all_years$built_index_rank_deciles_06
# calculate difference between 2011 and 2016 rankings
Comp_ind_all_years$rank_dif_11_16 <- Comp_ind_all_years$built_index_rank_deciles_16 - Comp_ind_all_years$built_index_rank_deciles_11


# Subset the dataset to the variables we want to map
Comp_ind_all_years_subs <- Comp_ind_all_years[, c(
  'rank_dif_01_16',
  'rank_dif_01_06',
  'rank_dif_06_11',
  'rank_dif_11_16'
  
)]


# specify the CRS required
WGS84 = "+init=epsg:4326"

# Make sure that the layer is projected on WGS84 so can be plot with leaflet
Comp_ind_all_years_subs <- st_transform(Comp_ind_all_years_subs,WGS84)

# Convert the geospatial data from sf to sp object (I found it easier to be plotted)
Comp_ind_all_years_subs_sp <- as(Comp_ind_all_years_subs, 'Spatial')

# Change the the column names to remove the dot between lines
names(Comp_ind_all_years_subs_sp@data)[names(Comp_ind_all_years_subs_sp@data)=="rank_dif_01_16"] <- "Decile change between 2001 and 2016"
names(Comp_ind_all_years_subs_sp@data)[names(Comp_ind_all_years_subs_sp@data)=="rank_dif_01_06"] <- "Decile change between 2001 and 2006"
names(Comp_ind_all_years_subs_sp@data)[names(Comp_ind_all_years_subs_sp@data)=="rank_dif_06_11"] <- "Decile change between 2006 and 2011"
names(Comp_ind_all_years_subs_sp@data)[names(Comp_ind_all_years_subs_sp@data)=="rank_dif_11_16"] <- "Decile change between 2011 and 2016"




# Initiate the map
map <- leaflet() %>% addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter)


# create the colour palette for each layer
# specify the range so to be consistent across years
scale_range <- c(-10, 10)

pal <- colorNumeric(
  palette = "RdYlGn",
  domain = scale_range, reverse = TRUE)

# Create a layer for each of the variables through looping across the columns
for (i in seq_along(1:4)) {

  
  
  map <- map %>% 
    addPolygons(data = Comp_ind_all_years_subs_sp,
                fillColor = ~pal(Comp_ind_all_years_subs_sp[[i]]),
                weight = 0.4,
                opacity = 0.8,
                color = "black",
                dashArray = "3",
                fillOpacity = 0.5,
                popup = paste(names(Comp_ind_all_years_subs_sp@data[i]),":" , round(Comp_ind_all_years_subs_sp[[i]],2), "<br>",
                              "Decile 2001: ", Comp_ind_all_years$built_index_rank_deciles_01, "<br>",
                              "Decile 2006: ", Comp_ind_all_years$built_index_rank_deciles_06, "<br>",
                              "Decile 2011: ", Comp_ind_all_years$built_index_rank_deciles_11, "<br>",
                              "Decile 2016: ", Comp_ind_all_years$built_index_rank_deciles_16, "<br>"),
                group = names(Comp_ind_all_years_subs_sp@data[i]),
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  bringToFront = TRUE))   %>%
    addLayersControl(baseGroups = c(names(Comp_ind_all_years_subs_sp@data)),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(
      c(
        "Decile change between 2001 and 2006",
        'Decile change between 2006 and 2011',
        'Decile change between 2011 and 2016'
      )
    ) 
}


# pal2 <- colorNumeric(
#   palette = "RdYlGn",
#   domain = Comp_ind_all_years_subs_sp[[1]], reverse = TRUE)

# Add a legend and title to the map
dif_map <- map %>%
  addLegend("bottomleft", pal = pal, values = scale_range,
            title = "Change in SUDI deciles",
            opacity = 1
  ) %>% 
  addFullscreenControl()

# Plot the map
#dif_map

library(htmlwidgets)
saveWidget(dif_map, file="../outputs/Decile_difference.html", title = "Decile_difference", selfcontained=TRUE)
