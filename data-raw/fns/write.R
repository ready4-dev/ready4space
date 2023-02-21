write_dirs_for_imp <- function(paths_chr){
  purrr::walk(paths_chr,
              ~ dir.create(.x))
}
write_fls_for_imp <- function(x,
                              data_match_value_xx,
                              match_var_nm_1L_chr,
                              path_1L_chr,
                              overwrite_1L_lgl = F){
  write_1L_lgl <- F
  download_components_vec <- purrr::map_chr(c("file_name_chr",
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
  dest_file <- paste0(path_1L_chr,
                      "/",
                      download_components_vec[1],
                      download_components_vec[2])
  if(!is.na(download_components_vec[5])){
    if(overwrite_1L_lgl | file.exists(dest_file))
      file.copy(from = download_components_vec[5],to = dest_file)
  }else{
    if(!is.na(paste0(path_1L_chr,
                     "/",
                     download_components_vec[4]))){
      if(overwrite_1L_lgl | !file.exists(paste0(path_1L_chr, ## NEEDS UPDATING TO REFERENCE RENAMED PRE-EXISTING FILES
                                                "/",
                                                download_components_vec[4]))){
        if(!is.na(download_components_vec[6])){
          procure(x, # Does this need to be ingest / author
                  save_dir_path_chr = path_1L_chr,
                  unlink_lgl = F)

        }else{
          utils::download.file(download_components_vec[3],
                               destfile = dest_file,
                               mode = 'wb')
        }
        if(download_components_vec[2] == ".zip"){
          utils::unzip(dest_file,
                       exdir = path_1L_chr)
        }
        write_to_rnm_fls_for_imp(x = x,
                                 data_match_value_xx = data_match_value_xx,
                                 match_var_nm_1L_chr = match_var_nm_1L_chr,
                                 path_1L_chr = path_1L_chr,
                                 overwrite_1L_lgl = overwrite_1L_lgl)
        write_1L_lgl <- T
      }
    }
  }
  return(write_1L_lgl)
}
write_to_rnm_fls_for_imp <- function(x,
                                     data_match_value_xx,
                                     match_var_nm_1L_chr,
                                     path_1L_chr,
                                     overwrite_1L_lgl = F){
  old_names_list <- ready4::get_from_lup_obj(data_lookup_tb = x,
                                             match_value_xx = data_match_value_xx,
                                             match_var_nm_1L_chr = match_var_nm_1L_chr,
                                             target_var_nm_1L_chr = "inc_fls_to_rename_ls",
                                             evaluate_1L_lgl = FALSE)
  new_names_list <- ready4::get_from_lup_obj(data_lookup_tb = x,
                                             match_value_xx = data_match_value_xx,
                                             match_var_nm_1L_chr = match_var_nm_1L_chr,
                                             target_var_nm_1L_chr = "new_nms_for_inc_fls_ls",
                                             evaluate_1L_lgl = FALSE)
  if(!is.na(old_names_list)){
    purrr::walk2(old_names_list,
                 new_names_list,
                 ~ if(overwrite_1L_lgl | !file.exists(paste0(path_1L_chr,
                                                             "/",
                                                             .y)))
                   file.rename(paste0(path_1L_chr,
                                      "/",
                                      .x),
                               paste0(path_1L_chr,
                                      "/",
                                      .y)))

  }
}
##
write_attr_tb <- function(attr_tb,
                          obj_name,
                          processed_fls_dir_1L_chr,
                          overwrite_1L_lgl){
  path_to_attr_tb_chr <- make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                                               name_chr = obj_name,
                                               data_type_chr = "Attribute")
  if(overwrite_1L_lgl | !file.exists(path_to_attr_tb_chr))
    saveRDS(attr_tb, file = path_to_attr_tb_chr)
}
write_procsd_geom_imp <- function(x,
                                  imports_ls,
                                  path_to_seed_sf_1L_chr,
                                  merge_itms_chr,
                                  crs_nbr_dbl,
                                  overwrite_1L_lgl){
  ready4use::assert_single_row_tb(x)
  if(overwrite_1L_lgl | !file.exists(path_to_seed_sf_1L_chr)){
    if(is.na(merge_itms_chr) %>% all()){
      starter_sf <- imports_ls[[1]]
    }else{
      starter_sf <- purrr::reduce(merge_itms_chr,
                                  .init = imports_ls[[1]],
                                  ~ make_intersecting_geometries(.x,
                                                          eval(parse(text=.y)),
                                                          crs_nbr_dbl = crs_nbr_dbl,
                                                          validate_1L_lgl = T
                                  ))
      if((sf::st_geometry_type(starter_sf) %>% as.character()!="POINT") %>% any()){
        starter_sf <- starter_sf %>%
          dplyr::mutate(area = sf::st_area(.)) %>%
          dplyr::filter(area > units::set_units(0,m^2)) ## Note: Will discard points
      }
    }
    if(x %>% dplyr::pull(main_feature_chr) == "Boundary")
      starter_sf <- starter_sf %>%
        transform_to_simpler_sf(crs_dbl = crs_nbr_dbl[1])
    saveRDS(starter_sf, file = path_to_seed_sf_1L_chr)
  }
}
write_procsd_imp_xx <- function(x,
                                imports_ls,
                                path_to_seed_sf_1L_chr,
                                merge_itms_chr,
                                package_1L_chr,
                                raw_fls_dir_1L_chr,
                                processed_fls_dir_1L_chr,
                                crs_nbr_dbl = NA_real_,
                                overwrite_1L_lgl = F){
  if(x %>% dplyr::pull(data_type_chr) == "Geometry"){
    write_procsd_geom_imp(x,
                          imports_ls = imports_ls,
                          path_to_seed_sf_1L_chr = path_to_seed_sf_1L_chr,
                          #processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                          merge_itms_chr = merge_itms_chr,
                          crs_nbr_dbl= crs_nbr_dbl,
                          overwrite_1L_lgl = overwrite_1L_lgl)
  }
  if(x %>% dplyr::pull(data_type_chr) == "Attribute"){
    purrr::walk2(imports_ls,
                 names(imports_ls),
                 ~ write_attr_tb(attr_tb = .x,
                                 obj_name = .y,
                                 processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                                 overwrite_1L_lgl = overwrite_1L_lgl))

  }
}
write_raw_format_dir <- function(data_type_chr,
                                 raw_fls_dir_1L_chr){
  directory_chr <- switch(data_type_chr, "Geometry" = "Geometries","Attribute" = "Attributes")
  raw_format_sp_dir <- make_raw_format_dir_chr(raw_fls_dir_1L_chr,directory_chr)
  if(!dir.exists(raw_format_sp_dir))
    dir.create(raw_format_sp_dir)
  raw_format_sp_dir
}

