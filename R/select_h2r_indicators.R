#' select_h2r_indicators
#' This function is for selecting appropriate indicators for a given country. You need to supply data frame and the country code like "som" for Somalia
#' @return
#' @export
#' @description function to select relevant h2r indicators from data set
#' @examples
select_h2r_indicators <- function(input_df, country_code="som"){
  if(country_code=="som"){
    h2r_cols <- list(
      `Flood` = "climatic_shock.flood",
      `Failed rains` = "climatic_shock.no_rain",
      `Drought` = "climatic_shock.drought",
      `Locust invation` = "climatic_shock.locusts",
      `Wild fires` = "climatic_shock.fires",
      `Latitude` = "latitude",
      `Longitude` = "longitude"
    )
  }

  df_h2r_cols <- input_df |>
    select(any_of(purrr::map_chr(h2r_cols, ~.x)))
  return(df_h2r_cols)
}
