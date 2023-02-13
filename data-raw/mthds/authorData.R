authorData.vicinity_raw <- function(x,
                                    dir_1L_chr,
                                    data_match_value_xx,
                                    match_var_nm_1L_chr = "name",
                                    sub_dirs_chr = NULL,
                                    overwrite_1L_lgl = F){

  if(is.null(sub_dirs_chr))
    sub_dirs_chr <- names(vicinity_raw())[names(vicinity_raw()) %in% c("country_chr","area_type_chr","region_chr","main_feature_chr","year_chr")] ## Could add boundary year as extra directory
  paths_chr <- make_write_paths_chr(x = x,
                                    dir_1L_chr = dir_1L_chr,
                                    data_match_value_xx = data_match_value_xx,
                                    match_var_nm_1L_chr = match_var_nm_1L_chr,
                                    sub_dirs_chr = sub_dirs_chr)
  write_dirs_for_imp(paths_chr = paths_chr)
  files_written_1L_lgl <- write_fls_for_imp(x = x,
                                            data_match_value_xx = data_match_value_xx,
                                            match_var_nm_1L_chr = match_var_nm_1L_chr,
                                            path_1L_chr = paths_chr[length(paths_chr)],
                                            overwrite_1L_lgl = overwrite_1L_lgl)
  return(files_written_1L_lgl)
}
