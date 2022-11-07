#' select_h2r_indicators
#' This function is for selecting appropriate indicators for a given country. You need to supply data frame and the country code like "som" for Somalia
#'
#' @param input_df
#' @param country_code
#'
#' @return
#' @export
#' @description function to select relevant h2r indicators from data set
#' @examples
select_h2r_indicators <- function(input_df, country_code="som"){
  if(country_code =="som"){
    h2r_cols <- c("hex_4000km",
                  "climatic_shock",
                  "crop_loss_reasons",
                  "primary_reason_moved",
                  "idp_new_arrivals",
                  "idp_arrived_reason",
                  "idp_pull_factors",
                  "hc_push_main",
                  "mainsource_water",
                  "responsible_water_fetching",
                  "surfacewater_drinking",
                  "gettingwater_time",
                  "water_sufficient_lastmonth",
                  "stagnant_water_near",
                  "people_using_latrines",
                  "barriers_usetoilets",
                  "waste_disposal",
                  "handwashing_access"
    )
  }

  df_h2r_cols <- input_df |>
    # select(any_of(purrr::map_chr(h2r_cols, ~.x))) |>
    select(contains(h2r_cols)) |>
    mutate(country_code = country_code)
  return(df_h2r_cols)
}
