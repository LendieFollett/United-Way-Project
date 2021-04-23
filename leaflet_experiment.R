library(leaflet)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(tigris)
library(sp)
library(tidycensus)
library(rgeos)#spatial
library(DT)#datatable
library(rmapshaper)


acs <- read.csv("acs_predictions.csv")#read acs data with predictions


#for merging
acs$GEOID <- as.character(paste0(acs$GEOID, substr(acs$X, 13, 13)))
#this is block groups w/in tracts
ia_shp <- readRDS("ia_shp.RDS")#block_groups(state = "IA")  #TIME INTENSIVE 1st run (so each dashboard run)

county_list <- readRDS("county_list.RDS")#unique(counties("Iowa")$NAME)#TIME INTENSIVE 1st run (so each dashboard run)
county_list <- county_list[order(county_list)]
all_counties <-   block_groups(state = 'IA', county = county_list) #also time intensive...

ia_shp_join <- left_join(ia_shp, acs, by="GEOID" ) %>%
  rmapshaper::ms_simplify( keep = 0.01, keep_shapes = TRUE)

pal <- colorBin(
  palette = "viridis", domain = ia_shp_join$lasso_bin_pred,
  bins = seq(0, max(ia_shp_join$lasso_bin_pred, na.rm = TRUE) + .01, by = .1)
)
leaflet(ia_shp_join) %>%
  addTiles() %>%
  #setView(lng = 0, lat = 30, zoom = 2) %>%
  addPolygons(
    fillColor = ~ pal(lasso_bin_pred),
    color = "white",
    fillOpacity = 0.6,
    #label = ~labels,
    highlight = highlightOptions(
      color = "black",
      bringToFront = TRUE
    )
  )

# %>%
  leaflet::addLegend(
    pal = pal, values = ~PM2.5,
    opacity = 0.7, title = "PM2.5"
  )
