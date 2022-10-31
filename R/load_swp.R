


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
                      sheet = "Water_sources")
}

