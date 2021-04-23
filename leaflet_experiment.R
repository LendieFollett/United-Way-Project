library(leaflet)
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
