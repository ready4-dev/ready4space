download_data.vicinity_raw <- function(x, # NOTE, WHEN DOCUMENTING: IMPORTS GENERIC
                                               destination_directory,
                                               data_lookup_ref,
                                               lookup_variable = "name",
                                               directory_sub_divs = NULL,
                                               overwrite_1L_lgl = F){

  if(is.null(directory_sub_divs))
    directory_sub_divs <- names(vicinity_raw())[names(vicinity_raw()) %in% c("country_chr","area_type_chr","region_chr","main_feature_chr","year_chr")] ## Could add boundary year as extra directory
  directory_paths <- get_dir_paths_for_data_imp(x = x,
                                               destination_directory = destination_directory,
                                               data_lookup_ref = data_lookup_ref,
                                               lookup_variable = lookup_variable,
                                               directory_sub_divs = directory_sub_divs)
  write_dirs_for_imp(directory_paths = directory_paths)
  write_fls_for_imp(x = x,
                         data_lookup_ref = data_lookup_ref,
                         lookup_variable = lookup_variable,
                         directory_path = directory_paths[length(directory_paths)],
                         overwrite_1L_lgl = overwrite_1L_lgl)
}
