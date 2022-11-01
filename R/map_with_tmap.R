#' map_with_tmap
#' Function to iterate over given columns and produce maps based on those columns
#'
#' @param input_shepefile The shapefile/layer to visulise
#' @param input_col Column for styling the map. This column should be present in the layer provided
#' @param input_ctry_code Provide country code
#'
#' @return
#' @export
#'
#' @examples
map_with_tmap <- function(input_shepefile, input_col, input_ctry_code) {
  if (input_col %in% colnames(input_shepefile)){
    tmap::tm_shape(input_shepefile) +
      tmap::tm_polygons(col = input_col, border.col = "white") +
      tmap::tm_layout(paste("Map of",input_ctry_code,  "showing", input_col), title.size=.8)+
      tmap::tm_borders("white", lwd = .1) +
      tmap::tm_compass(type = "4star", size = 2, position = c("left", "top"))+
      tmap::tm_scale_bar(position = c("right", "bottom"))
  }else{
    warning("The given column does not exist in the given layer")
  }

}
