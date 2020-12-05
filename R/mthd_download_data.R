#' @title download_data.ready4_sp_import_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param destination_directory PARAM_DESCRIPTION
#' @param data_lookup_ref PARAM_DESCRIPTION
#' @param lookup_variable PARAM_DESCRIPTION, Default: 'name'
#' @param directory_sub_divs PARAM_DESCRIPTION, Default: NULL
#' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname download_data.ready4_sp_import_lup
#' @export
#' @importMethodsFrom ready4use download_data
download_data.ready4_sp_import_lup <- function(x, # NOTE, WHEN DOCUMENTING: IMPORTS GENERIC
                                               destination_directory,
                                               data_lookup_ref,
                                               lookup_variable = "name",
                                               directory_sub_divs = NULL,
                                               overwrite_lgl = F){

  if(is.null(directory_sub_divs))
    directory_sub_divs <- names(ready4_sp_import_lup())[names(ready4_sp_import_lup()) %in% c("country","area_type","region","main_feature","year")] ## Could add boundary year as extra directory
  directory_paths <- data_import_get_dir_paths(x = x,
                                               destination_directory = destination_directory,
                                               data_lookup_ref = data_lookup_ref,
                                               lookup_variable = lookup_variable,
                                               directory_sub_divs = directory_sub_divs)
  data_import_make_directories(directory_paths = directory_paths)
  data_import_save_files(x = x,
                         data_lookup_ref = data_lookup_ref,
                         lookup_variable = lookup_variable,
                         directory_path = directory_paths[length(directory_paths)],
                         overwrite_lgl = overwrite_lgl)
}
