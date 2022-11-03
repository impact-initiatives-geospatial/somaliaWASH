#' map_leaflet_polygon_and_point
#' Create a leaflet map dynamically by supplying different variables
#' @param input_polygon_layer
#' @param polygon_lyr_column
#' @param input_point_layer
#' @param point_lyr_column
#'
#' @return
#' @export
#'
#' @examples
map_leaflet_polygon_and_point <- function(input_polygon_layer, polygon_lyr_column, input_point_layer, point_lyr_column) {
  # colouring the shapes
  pal <- colorBin(palette = "YlOrBr", domain = input_polygon_layer[[polygon_lyr_column]], bins = 5, na.color = "#b6b6b7")
  # colouring the points
  dark_pal <-RColorBrewer::brewer.pal(n=6, name = "Dark2")
  points_facpal <- colorFactor(palette = dark_pal, domain = input_point_layer[[point_lyr_column]])
  # start leaflet
  leaflet() |>
    addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(minZoom = 3.75, maxZoom = 10.5), group="Esri Gray Canvas") |>
    addProviderTiles(providers$Esri.WorldImagery,  options = providerTileOptions(minZoom = 3.75, maxZoom = 10.5), group="Esri World Imagery") |>
    setView(lng = 43.825611, lat= 2.766313, zoom = 8) |>
    addPolygons( data = input_polygon_layer,
                 color = "white", weight = 2, opacity = 1, dashArray = "3", fillOpacity = 0.8,
                 fillColor = ~pal(input_polygon_layer[[polygon_lyr_column]]),
                 label = input_polygon_layer[[polygon_lyr_column]],
                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                             textsize = "15px",
                                             direction = "auto", opacity =0.75)

    ) |>
    addCircleMarkers(data = input_point_layer,
                     fillColor = ~points_facpal(input_point_layer[[point_lyr_column]]), radius = 6.5,
                     color= NA, fillOpacity = 1, group = point_lyr_column,
                     label = ~input_point_layer[[point_lyr_column]])  |>
    addLegend(position ="bottomleft",
              pal = pal,
              values = input_polygon_layer[[polygon_lyr_column]],
              title = polygon_lyr_column,
              opacity  = 1
    ) |>
    addLegend(position ="bottomright", pal = points_facpal,

              values =input_point_layer[[point_lyr_column]],
              title = point_lyr_column,
              opacity  = 1,
              na.label = ""
    ) |>
    addLayersControl(
      baseGroups = c("Esri Gray Canvas", "Esri World Imagery"),
      overlayGroups = c(polygon_lyr_column, point_lyr_column),
      options = layersControlOptions(collapsed = FALSE)
    )
}
