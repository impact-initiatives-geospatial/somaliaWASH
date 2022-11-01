#' select_h2r_indicators
#' This function is for selecting appropriate indicators for a given country. You need to supply data frame and the country code like "som" for Somalia
#' @return
#' @export
#' @description function to select relevant h2r indicators from data set
#' @examples
select_h2r_indicators <- function(input_df, country_code="som"){
  if(country_code =="som"){
    h2r_cols <- list(
      `uuid` = "uuid",
      `District` = "district_info",
      `Hex` = "hex_4000km",
      `Base` = "base",
      `info settlement` = "info_settlement",
      `Latitude` = "latitude",
      `Longitude` = "longitude",
      `Flood` = "climatic_shock.flood",
      `Failed rains` = "climatic_shock.no_rain",
      `Drought` = "climatic_shock.drought",
      `Locust invation` = "climatic_shock.locusts",
      `Wild fires` = "climatic_shock.fires",
      `Crop loss reasons` = "crop_loss_reasons",
      `Crop loss reasons locusts` = "crop_loss_reasons.locusts",
      `Crop loss reasons no rain` = "crop_loss_reasons.no_rain",
      `Crop loss reasons flooding` = "crop_loss_reasons.flooding",
      `Crop loss reasons no irrigation` = "crop_loss_reasons.no_irrigation",
      `Crop loss reasons high temperature` = "crop_loss_reasons.high_temperature",
      `Reason moved` = "primary_reason_moved",
      `Reason moved to access water` = "primary_reason_moved.access_water",
      `Reason moved to access food` = "primary_reason_moved.access_food",
      `Reason moved to better security` = "primary_reason_moved.better_security",
      `Reason moved to jobsa vailable` = "primary_reason_moved.jobsa_vailable",
      `Reason moved to shelters available` = "primary_reason_moved.shelters_available",
      `Reason moved to better services` = "primary_reason_moved.better_services",
      `New IDP arrivals` = "idp_new_arrivals",
      `New IDP arrivals reason climatic shock` = "idp_arrived_reason.climatic_shock",
      `New IDP arrivals reason lack food` = "idp_arrived_reason.lack_food",
      `New IDP arrivals reason lack water` = "idp_arrived_reason.lack_water",
      `New IDP arrivals reason lack income` = "idp_arrived_reason.lack_income",
      `New IDP arrivals reason no services` = "idp_arrived_reason.no_services",
      `New IDP arrivals reason eviction` = "idp_arrived_reason.eviction",
      `New IDP arrivals reason insecurity` = "idp_arrived_reason.insecurity",
      # `IDP pull factors` = "idp_pull_factors", # not in the data
      `IDP pull factors water` = "idp_pull_factors.access_water",
      `IDP pull factors food` = "idp_pull_factors.access_food",
      `IDP pull factors security` = "idp_pull_factors.better_security",
      `IDP pull factors jobs` = "idp_pull_factors.presence_jobs",
      `IDP pull factors shelters` = "idp_pull_factors.availability_shelters",
      `IDP pull factors services` = "idp_pull_factors.better_services",
      `HC push main` = "hc_push_main",
      `Main source water` = "mainsource_water",
      `Responsible water fetching` = "responsible_water_fetching",
      `Surfacewater drinking` = "surfacewater_drinking",
      `Gettingwater time` = "gettingwater_time",
      `Water sufficient lastmonth` = "water_sufficient_lastmonth",
      `Stagnant water near` = "stagnant_water_near",
      `People using latrines` = "people_using_latrines",
      `Barriers usetoilets no barrier` = "barriers_usetoilets.no_barrier",
      `Barriers usetoilets not available` = "barriers_usetoilets.not_available",
      `Barriers usetoilets insufficient` = "barriers_usetoilets.insufficient",
      `Barriers usetoilets not funtional` = "barriers_usetoilets.not_funtional",
      `Barriers usetoilets overcrowded` = "barriers_usetoilets.overcrowded",
      `Barriers usetoilets too dirty` = "barriers_usetoilets.too_dirty",
      `Barriers usetoilets too far` = "barriers_usetoilets.too_far",
      `Barriers usetoilets not common` = "barriers_usetoilets.not_common",
      `Barriers usetoilets women notsafe` = "barriers_usetoilets.women_notsafe",
      `Barriers usetoilets pwd notsafe` = "barriers_usetoilets.pwd_notsafe",
      `Barriers usetoilets child notsafe` = "barriers_usetoilets.child_notsafe",
      `Barriers usetoilets night notsafe` = "barriers_usetoilets.night_notsafe",
      `Waste disposal` = "waste_disposal",
      `Hand washing access` = "handwashing_access"
    )
  }

  df_h2r_cols <- input_df |>
    select(any_of(purrr::map_chr(h2r_cols, ~.x))) |>
    mutate(country_code = country_code)
  return(df_h2r_cols)
}
