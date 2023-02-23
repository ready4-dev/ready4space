authorData.vicinity_raw <- function(x,
                                    path_1L_chr,
                                    data_match_value_xx,
                                    match_var_nm_1L_chr = "name",
                                    sub_dirs_chr = NULL,
                                    overwrite_1L_lgl = F,
                                    what_1L_chr = "outer"){
  if(what_1L_chr == "outer"){
    if(is.null(sub_dirs_chr))
      sub_dirs_chr <- names(vicinity_raw())[names(vicinity_raw()) %in% c("country_chr","area_type_chr","region_chr","main_feature_chr","year_chr")] ## Could add boundary year as extra directory
    paths_chr <- manufacture(x = x,#make_paths_chr
                             processed_fls_dir_1L_chr = path_1L_chr,
                             match_value_xx = data_match_value_xx,
                             match_var_nm_1L_chr = match_var_nm_1L_chr,
                             sub_dirs_chr = sub_dirs_chr,
                             what_1L_chr = "paths_chr")
    write_dirs_for_imp(paths_chr = paths_chr)
    files_written_1L_lgl <- authorData.vicinity_raw(x,#write_fls_for_imp
                                                    data_match_value_xx = data_match_value_xx,
                                                    match_var_nm_1L_chr = match_var_nm_1L_chr,
                                                    path_1L_chr = paths_chr[length(paths_chr)],
                                                    overwrite_1L_lgl = overwrite_1L_lgl,
                                                    what_1L_chr = "inner")
  }
  if(what_1L_chr == "inner"){ # write_fls_for_imp
    # write_fls_for_imp <- function(x,
    #                               data_match_value_xx,
    #                               match_var_nm_1L_chr,
    #                               path_1L_chr,
    #                               overwrite_1L_lgl = F){
      files_written_1L_lgl <- F
      download_components_chr <- purrr::map_chr(c("file_name_chr",
                                                  "file_type_chr",
                                                  "download_url_chr",
                                                  "inc_file_main_chr",
                                                  "local_file_src_chr",
                                                  "data_repo_db_ui_chr"),
                                                ~ ready4::get_from_lup_obj(data_lookup_tb = x,
                                                                           match_value_xx = data_match_value_xx,
                                                                           match_var_nm_1L_chr = match_var_nm_1L_chr,
                                                                           target_var_nm_1L_chr = .x,
                                                                           evaluate_1L_lgl = FALSE))
      dest_file_1L_chr <- paste0(path_1L_chr,
                                 "/",
                                 download_components_chr[1],
                                 download_components_chr[2])
      if(!is.na(download_components_chr[5])){
        if(overwrite_1L_lgl | file.exists(dest_file_1L_chr))
          file.copy(from = download_components_chr[5],to = dest_file_1L_chr)
      }else{
        if(!is.na(paste0(path_1L_chr,
                         "/",
                         download_components_chr[4]))){
          if(overwrite_1L_lgl | !file.exists(paste0(path_1L_chr, ## NEEDS UPDATING TO REFERENCE RENAMED PRE-EXISTING FILES
                                                    "/",
                                                    download_components_chr[4]))){
            if(!is.na(download_components_chr[6])){
              procure(x %>%
                        dplyr::select(intersect(names(ready4use_dataverses()),names(vicinity_raw()))) %>%
                        ready4use::ready4use_dataverses(), # Does this need to be ingest / author # NEEDS TO BE ready4use_dataverses ?
                      save_dir_path_chr = path_1L_chr,
                      unlink_lgl = F)

            }else{
              utils::download.file(download_components_chr[3],
                                   destfile = dest_file_1L_chr,
                                   mode = 'wb')
            }
            if(download_components_chr[2] == ".zip"){
              utils::unzip(dest_file_1L_chr,
                           exdir = path_1L_chr)
            }
            capture_xx <- authorData.vicinity_raw(x,#write_to_rnm_fls_for_imp
                                                  data_match_value_xx = data_match_value_xx,
                                                  match_var_nm_1L_chr = match_var_nm_1L_chr,
                                                  path_1L_chr = path_1L_chr,
                                                  overwrite_1L_lgl = overwrite_1L_lgl,
                                                  what_1L_chr == "rename")
            files_written_1L_lgl <- T
          }
        }
      }
    #   return(write_1L_lgl)
    # }
  }
  if(what_1L_chr == "rename"){ # write_to_rnm_fls_for_imp
    files_written_1L_lgl <- NULL
    # write_to_rnm_fls_for_imp <- function(x,
    #                                      data_match_value_xx,
    #                                      match_var_nm_1L_chr,
    #                                      path_1L_chr,
    #                                      overwrite_1L_lgl = F){
      old_names_ls <- ready4::get_from_lup_obj(data_lookup_tb = x,
                                                 match_value_xx = data_match_value_xx,
                                                 match_var_nm_1L_chr = match_var_nm_1L_chr,
                                                 target_var_nm_1L_chr = "inc_fls_to_rename_ls",
                                                 evaluate_1L_lgl = FALSE)
      new_names_ls <- ready4::get_from_lup_obj(data_lookup_tb = x,
                                                 match_value_xx = data_match_value_xx,
                                                 match_var_nm_1L_chr = match_var_nm_1L_chr,
                                                 target_var_nm_1L_chr = "new_nms_for_inc_fls_ls",
                                                 evaluate_1L_lgl = FALSE)
      if(!is.na(old_names_ls)){
        purrr::walk2(old_names_ls,
                     new_names_ls,
                     ~ if(overwrite_1L_lgl | !file.exists(paste0(path_1L_chr,
                                                                 "/",
                                                                 .y))){

                       file.rename(paste0(path_1L_chr,
                                          "/",
                                          .x),
                                   paste0(path_1L_chr,
                                          "/",
                                          .y))

                     })
      }
  }
  return(files_written_1L_lgl)
}
