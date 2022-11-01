

#' h2r_to_sf_utm
#'
#' @param settlement level H2R data.frame with coordinates stored as character "geometry" column
#' @param country_code country_code (default= "som")
#'
#' @return sf object projected to UTM zone based on country code
#' @description helper function used inside h2r_aggregate funcs
#' @export
#'
#' @examples \dontrun{
#' settlement_level |>
#'   h2r_to_sf_utm
#' }

h2r_to_sf_utm <- function(df, country_code="som"){
  coord_names <- c("lon","lat")
  df |>
    tidyr::separate(geometry, into =coord_names,sep = ",") |>
    dplyr::mutate(
      dplyr::across(.cols= dplyr::all_of(coord_names),\(coord){readr::parse_number(coord)})
    ) |>
    sf::st_as_sf(coords = coord_names,crs=4326) |>
    reach_reproject_utm(country_code = country_code)
}





#' h2r_hex_aggregate_pct
#'
#' @param df settlement level h2r data set
#' @param hex hex sf object
#' @param summarise_cols cols to summarise
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' h2r_hex_aggregate_pct(settlement_level,
#'                       som_hex,
#'                       summarise_cols = "lack_food_reasons.noland",
#'                      country_code = "som")
#'
#' }
h2r_hex_aggregate_pct <-  function(df,
                                   hex,
                                   summarise_cols,
                                   country_code="som"
){

  assertthat::assert_that("geometry" %in% colnames(df),"df must have column named geometry")
  missing_col <-  setdiff(summarise_cols,colnames(df))
  assertthat::assert_that(length(missing_col)==0,msg = glue::glue("{crayon::red(missing_col)} missing from dataset"))

  df_prep <- settlement_level |>
    dplyr::select(dplyr::any_of(c(summarise_cols,"geometry"))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(summarise_cols),\(coi){ifelse(coi %in% c("SL","NC"),NA,as.character(coi))
    })
    ) |>
    h2r_to_sf_utm(country_code = country_code)

  hex <- hex |>
    reach_reproject_utm(country_code = country_code)

  df_sf_grid<-df_prep |>
    sf::st_join(hex) |>
    st_drop_geometry()

  # unique grid ids
  grid_ids <-hex |>
    st_drop_geometry()

  summary_tables <- summarise_cols |>
    purrr::map(
      ~{
        count_name <-  paste0(.x,"_n")
        pct_name <-  paste0(.x,"_pct")
        unique_lvls_approx <- df_sf_grid |>
          dplyr::filter(!is.na(!!sym(.x))) |>
          pull(!!sym(.x))

        var_summarised<- df_sf_grid |>
          dplyr::select(dplyr::all_of(c("GRID_ID",.x))) |>
          dplyr::filter(!is.na(!!sym(.x)))  |>
          dplyr::mutate(!!.x:= forcats::fct_expand(!!sym(.x),unique_lvls_approx)) |>
          dplyr::group_by(GRID_ID,!!sym(.x)) |>
          dplyr::summarise(!!count_name:=n(),.groups="drop_last") |>
          dplyr::group_by(GRID_ID) |>
          mutate(
            !!pct_name:= !!sym(count_name)/sum(!!sym(count_name)),
          )
        vars_summarised_wide <- var_summarised |>
          tidyr::pivot_wider(id_cols=GRID_ID,
                             values_from = dplyr::all_of(c(count_name,pct_name)),
                             names_from = .x,
          )
        grid_ids |>
          left_join(vars_summarised_wide,by="GRID_ID")
      }
    )
  summary_stats_joined <-purrr::reduce(summary_tables,left_join,by="GRID_ID")
  hex |>
    dplyr::left_join(summary_stats_joined, by="GRID_ID")



}









#' Title
#'
#' @param pt_sf sf class point object
#' @param hex hex/grid/polygon
#' @param var variable of interest (default=NULL) . If NULL, just point counts of entire dataset are aggregated
#' @param var_val variable value of interest (default = NULL). If NULL, just point counts of entire dataset are aggregated
#' @param country_code three chr country code (default = "som")
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' library(somaliaWASH)
#' swp <-  load_swp()
#' swp_sf <- swp |>
#'     sf::st_as_sf(coords=c("longitude","latitude"), crs=4326)
#'
#' # just aggregate point counts
#' water_point_count_per_grid <- swp_sf |>
#'    hex_aggregate_pt_count(hex = som_hex)
#'
#' # aggregate point counts based on `var`- `val` combination
#' strategic_water_point_count_per_grid <- swp_sf |>
#'    hex_aggregate_pt_count(hex = som_hex,var = "is_strategic",var_val = "yes")
#'}


hex_aggregate_pt_count <-  function(pt_sf= swp_sf,hex=som_hex, var=NULL, var_val=NULL, country_code="som"){

  var_and_val_cond<- ((is.null(var) & is.null(var_val))| (!is.null(var)&!is.null(var_val)))
  assertthat::assert_that(var_and_val_cond,msg = "if either var or var_value is NULL, they both most be NULL & visa versa")

  if(!is.null(var)){
    pt_sf <- pt_sf |>
      dplyr::filter(!!sym(var)==var_val)
  }

  pt_sf_utm <- pt_sf |>
    reach_reproject_utm(country_code = country_code)

  hex_utm <- hex |>
    reach_reproject_utm(country_code = country_code)

  grid_counts <- pt_sf_utm |>
    sf::st_join(hex_utm) |>
    sf::st_drop_geometry() |>
    group_by(GRID_ID) |>
    summarise(
      count=n()
    )
  res<- hex_utm |>
    left_join(grid_counts, by ="GRID_ID")
  return(res)



}
