#' load_h2r_indictors
#' Calling the loading and selecting functions to prepare the data.
#'
#' @param main_folder_path
#' @param data_path
#' @param ctry_code
#'
#' @return
#' @export
#' @description combination of `load_h2r_data` and `select_h2r_indicators` to quicky load only data with only relelvant indicators
#' @examples
load_h2r_indicators <-  function(main_folder_path, data_path, ctry_code="som"){
  df <-load_h2r_data(main_folder_path, data_path)

  df |>
    select_h2r_indicators(country_code=ctry_code)

}
