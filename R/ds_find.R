

#' data set find
#'
#' @param proj project (default = "somWASH") - might be useful for expanding to related projects later
#' @param ds dataset name. This is an easy to remember short name that's been given (default = 2205_h2r_settlement)
#'
#' @return file path to data set
#' @export
#'
#' @examples \dontrun{
#' library(somaliaWASH)
#' ds_find(ds= "202205_h2r_settlement")
#' }
#'

ds_find <-  function(proj="somWASH",ds="2205_h2r_settlement"){
  if(proj=="somWASH"){
  root_dir <-  Sys.getenv("SOM_2203_WASH_DATA")

  ds_path <-switch(
    ds,
    "2205_h2r_settlement" = "data\\202209\\h2r_aggregated\\SOM1901_H2R_settlement_aggregation_May_2022.csv" ,
    "strategic_wp" = "data\\202209\\raw\\SWALIM water point\\SOM_Strategic_water_sources_Sept2020_v3.xlsx"  )
  }
  return(file.path(root_dir,ds_path))
}


#' ds_ls
#'
#' @param proj
#' @description  helper function to return list of all datasets used  in project. Includes short/abbreviated name and file path
#' @return named list
#' @export
#'
#' @examples\dontrun{
#' library(somaliaWASH)
#' ds_ls())
#' }
#'

ds_ls <-  function(proj="somWASH"){
  if(proj=="somWASH"){
  list("2205_h2r_settlement" = "data\\202209\\h2r_aggregated\\SOM1901_H2R_settlement_aggregation_May_2022.csv" ,
  "strategic_wp" = "data\\202209\\raw\\SWALIM water point\\SOM_Strategic_water_sources_Sept2020_v3.csv"
  )
  }
}

