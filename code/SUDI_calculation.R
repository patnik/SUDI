library(sf)
library(htmlwidgets)
library(tmap)
library(gridExtra)

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

# run the composite index function
CI_2001 <- Comp_ind(data_2001, FUAs, "2001")
CI_2006 <- Comp_ind(data_2006, FUAs, "2006")
CI_2011 <- Comp_ind(data_2011, FUAs, "2011")
CI_2016 <- Comp_ind(data_2016, FUAs, "2016")


# save the interactive maps
saveWidget(CI_2001$Interactive_map, file="../outputs/2001_map.html", title = "2001_map", selfcontained=TRUE)
saveWidget(CI_2006$Interactive_map, file="../outputs/2006_map.html", title = "2006_map", selfcontained=TRUE)
saveWidget(CI_2011$Interactive_map, file="../outputs/2011_map.html", title = "2011_map", selfcontained=TRUE)
saveWidget(CI_2016$Interactive_map, file="../outputs/2016_map.html", title = "2016_map", selfcontained=TRUE)


# Create decile maps for the SUDI ----------------------------------------
LA_boundaries <- st_read("../data/layers/LAs.gpkg")


tmap_mode("plot")

mapping16 <- CI_2016$Comp_ind
mapping16 <- mapping16 %>% 
  drop_na(built_index_rank)


# create map for 2016
facet_map2016 <- tm_shape(mapping16) +
  tm_fill("built_index_rank", style = "quantile", n=10,
          palette = "OrRd",
          alpha = 1, title = '',
          showNA = FALSE,legend.show = FALSE,
          labels = c("Best performing decile", "2", "3",
                     "4", "5", "6",
                     "7", "8", "9",
                     "Worst performing decile")) +
  tm_layout(bg.color = "black",
            fontfamily = 'Avenir Next Condensed',
            legend.text.color = "black",
            legend.text.size = 1,
            main.title = "2016",
            title.size = 1,
            title.bg.color = TRUE,
            panel.label.bg.color = "black",
            panel.label.color = "white",
            panel.label.size = 1.3,
            panel.label.fontface = "bold",
            panel.label.fontfamily = 'Avenir Next Condensed') +
  tm_facets(by = "fuaname", nrow = 2)



# create the legend as a separaet object
legend.map <- tm_shape(mapping16) +
  tm_fill("built_index_rank", style = "quantile", n=10,
          palette = "OrRd",
          alpha = 1, title = '',
          showNA = FALSE,legend.show = TRUE,
          labels = c("Best performing decile", "2", "3",
                     "4", "5", "6",
                     "7", "8", "9",
                     "Worst performing decile")) +
  tm_layout(fontfamily = 'Avenir Next Condensed',
            legend.text.color = "black",
            legend.text.size = 2,
            legend.only = TRUE)




# print only 2016 maps
png("../outputs/SUDI_16.png",units="in", width=15, height=10, res=300)
tmap_arrange(facet_map2016, legend.map, ncol = 1, widths = c(1, 1), heights = c(0.75, 0.25))
dev.off() # Close the file




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

dec_summary_01 <- decile_summary(data_2001, CI_deciles01)
dec_summary_06 <- decile_summary(data_2006, CI_deciles06)
dec_summary_11 <- decile_summary(data_2011, CI_deciles11)
dec_summary_16 <- decile_summary(data_2016, CI_deciles16)





# create distribution line plots for each index
plots_new <- FUA_decile_plot_FUA_CompInd_line_plot(CI_deciles01, CI_deciles06, CI_deciles11, CI_deciles16)

plots_new2 <- FUA_decile_plot_FUA_Compactness_line_plot(CI_deciles01, CI_deciles06, CI_deciles11, CI_deciles16)

plots_new3 <- FUA_decile_plot_FUA_GreenSp_line_plot(CI_deciles01, CI_deciles06, CI_deciles11, CI_deciles16)

plots_new4 <- FUA_decile_plot_FUA_Walkability_line_plot(CI_deciles01, CI_deciles06, CI_deciles11, CI_deciles16)


# save the plots
png("../outputs/Composite_index_ranks_lineplots.png",units="in", width=8, height=7, res=300)
plots_new$Index_plot
dev.off() # Close the file

png("../outputs/Compactness_ranks_lineplots.png",units="in", width=8, height=7, res=300)
plots_new2$Index_plot
dev.off() # Close the file

png("../outputs/GreenSpace_ranks_lineplots.png",units="in", width=8, height=7, res=300)
plots_new3$Index_plot
dev.off() # Close the file

png("../outputs/Walkability_ranks_lineplots.png",units="in", width=8, height=7, res=300)
plots_new4$Index_plot
dev.off() # Close the file





