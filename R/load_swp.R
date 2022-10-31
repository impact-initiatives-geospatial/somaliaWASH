


#' load strategic water point data
#'
#' @return data.frame containing strategic water point data
#' @export
#'
#' @examples \dontrun{
#' swp <-  load_swp()
#' }

load_swp <-  function(){
  readxl:::read_xlsx( path = ds_find(proj = "somWASH", ds = "strategic_wp"),
                      sheet = "Water_sources") |>
    janitor::clean_names()
}

swp <-  load_swp()

hex_aggregate_pt <-  function(pt_sf= swp,hex=som_hex, var=NULL, var_val=NULL){


  hex_utm <- hex |>
    reach_reproject_utm("som")



}
