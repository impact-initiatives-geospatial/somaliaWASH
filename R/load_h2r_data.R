#' load_h2r_data
#' Provide the location of the main folder in our example we were reading from OneDrive, the the path to the csv file.
#' @return
#' @export
#' @description function to load H2R data set - will use environment key to specify base data directory
#' @examples
load_h2r_data <-  function(main_folder_path, data_path){

  # main_folder_path <-  Sys.getenv("SOM_2203_WASH_DATA")
  # data_path <- "data/202209/raw/REACH_H2R_0522-2607_data2.csv"

  h2r_data <- readr::read_csv(file.path(main_folder_path, data_path))

  return(h2r_data)
}
