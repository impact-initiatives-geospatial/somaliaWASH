#' load_h2r_indictors
#' Calling the loading and selecting functions to prepare the data.
#' @return
#' @export
#' @description combination of `load_h2r_data` and `select_h2r_indicators` to quicky load only data with only relelvant indicators
#' @examples
load_h2r_indicators <-  function(ctry_code="som"){
  df <-load_h2r_data()

  df |>
    select_h2r_indicators(country_code=ctry_code)

}
