download_data.ready4_sp_import_lup <- function(x, # NOTE, WHEN DOCUMENTING: IMPORTS GENERIC
                                               destination_directory,
                                               data_lookup_ref,
                                               lookup_variable = "name",
                                               directory_sub_divs = NULL,
                                               overwrite_1L_lgl = F){

  if(is.null(directory_sub_divs))
    directory_sub_divs <- names(ready4_sp_import_lup())[names(ready4_sp_import_lup()) %in% c("country","area_type","region","main_feature","year")] ## Could add boundary year as extra directory
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
