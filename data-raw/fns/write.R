write_att_tb <- function(att_tb,
                          object_nm_1L_chr,
                          processed_fls_dir_1L_chr,
                          overwrite_1L_lgl){
  path_to_att_tb_chr <- make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                                               name_chr = object_nm_1L_chr,
                                               data_type_chr = "Attribute")
  if(overwrite_1L_lgl | !file.exists(path_to_att_tb_chr))
    saveRDS(att_tb, file = path_to_att_tb_chr)
}
write_dirs_for_imp <- function(paths_chr){
  purrr::walk(paths_chr,
              ~ dir.create(.x))
}
write_raw_format_dir <- function(data_type_chr,
                                 raw_fls_dir_1L_chr){
  category_1L_chr <- switch(data_type_chr, "Geometry" = "Geometries","Attribute" = "Attributes")
  path_to_raw_outp_dir_1L_chr <- make_path_for_raw_outp_dir(category_1L_chr = category_1L_chr,
                                                            raw_fls_dir_1L_chr = raw_fls_dir_1L_chr
                                                            )
  if(!dir.exists(path_to_raw_outp_dir_1L_chr))
    dir.create(path_to_raw_outp_dir_1L_chr)
  return(path_to_raw_outp_dir_1L_chr)
}
# write_fls_for_imp <- function(x, # Now vicinity_raw mthd
#                               data_match_value_xx,
#                               match_var_nm_1L_chr,
#                               path_1L_chr,
#                               overwrite_1L_lgl = F){
#   write_1L_lgl <- F
#   download_components_chr <- purrr::map_chr(c("file_name_chr",
#                                               "file_type_chr",
#                                               "download_url_chr",
#                                               "inc_file_main_chr",
#                                               "local_file_src_chr",
#                                               "data_repo_db_ui_chr"),
#                                             ~ ready4::get_from_lup_obj(data_lookup_tb = x,
#                                                                        match_value_xx = data_match_value_xx,
#                                                                        match_var_nm_1L_chr = match_var_nm_1L_chr,
#                                                                        target_var_nm_1L_chr = .x,
#                                                                        evaluate_1L_lgl = FALSE))
#   dest_file_1L_chr <- paste0(path_1L_chr,
#                       "/",
#                       download_components_chr[1],
#                       download_components_chr[2])
#   if(!is.na(download_components_chr[5])){
#     if(overwrite_1L_lgl | file.exists(dest_file_1L_chr))
#       file.copy(from = download_components_chr[5],to = dest_file_1L_chr)
#   }else{
#     if(!is.na(paste0(path_1L_chr,
#                      "/",
#                      download_components_chr[4]))){
#       if(overwrite_1L_lgl | !file.exists(paste0(path_1L_chr, ## NEEDS UPDATING TO REFERENCE RENAMED PRE-EXISTING FILES
#                                                 "/",
#                                                 download_components_chr[4]))){
#         if(!is.na(download_components_chr[6])){
#           procure(x %>%
#                     dplyr::select(intersect(names(ready4use_dataverses()),names(vicinity_raw()))) %>%
#                     ready4use::ready4use_dataverses(), # Does this need to be ingest / author # NEEDS TO BE ready4use_dataverses ?
#                   save_dir_path_chr = path_1L_chr,
#                   unlink_lgl = F)
#
#         }else{
#           utils::download.file(download_components_chr[3],
#                                destfile = dest_file_1L_chr,
#                                mode = 'wb')
#         }
#         if(download_components_chr[2] == ".zip"){
#           utils::unzip(dest_file_1L_chr,
#                        exdir = path_1L_chr)
#         }
#         write_to_rnm_fls_for_imp(x = x,
#                                  data_match_value_xx = data_match_value_xx,
#                                  match_var_nm_1L_chr = match_var_nm_1L_chr,
#                                  path_1L_chr = path_1L_chr,
#                                  overwrite_1L_lgl = overwrite_1L_lgl)
#         write_1L_lgl <- T
#       }
#     }
#   }
#   return(write_1L_lgl)
# }
# write_to_rnm_fls_for_imp <- function(x, # Now vicinity_raw mthd
#                                      data_match_value_xx,
#                                      match_var_nm_1L_chr,
#                                      path_1L_chr,
#                                      overwrite_1L_lgl = F){
#   old_names_list <- ready4::get_from_lup_obj(data_lookup_tb = x,
#                                              match_value_xx = data_match_value_xx,
#                                              match_var_nm_1L_chr = match_var_nm_1L_chr,
#                                              target_var_nm_1L_chr = "inc_fls_to_rename_ls",
#                                              evaluate_1L_lgl = FALSE)
#   new_names_list <- ready4::get_from_lup_obj(data_lookup_tb = x,
#                                              match_value_xx = data_match_value_xx,
#                                              match_var_nm_1L_chr = match_var_nm_1L_chr,
#                                              target_var_nm_1L_chr = "new_nms_for_inc_fls_ls",
#                                              evaluate_1L_lgl = FALSE)
#   if(!is.na(old_names_list)){
#     purrr::walk2(old_names_list,
#                  new_names_list,
#                  ~ if(overwrite_1L_lgl | !file.exists(paste0(path_1L_chr,
#                                                              "/",
#                                                              .y)))
#                    file.rename(paste0(path_1L_chr,
#                                       "/",
#                                       .x),
#                                paste0(path_1L_chr,
#                                       "/",
#                                       .y)))
#
#   }
# }
# write_procsd_imp_xx <- function(x, # Now author vicinity_raw mthd
#                                 imports_ls,
#                                 path_to_seed_sf_1L_chr,
#                                 merge_itms_chr,
#                                 package_1L_chr,
#                                 raw_fls_dir_1L_chr,
#                                 processed_fls_dir_1L_chr,
#                                 crs_nbr_dbl = NA_real_,
#                                 overwrite_1L_lgl = F){
#   if(x %>% dplyr::pull(data_type_chr) == "Geometry"){
#     write_procsd_geom_imp(x,
#                           imports_ls = imports_ls,
#                           path_to_seed_sf_1L_chr = path_to_seed_sf_1L_chr,
#                           #processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
#                           merge_itms_chr = merge_itms_chr,
#                           crs_nbr_dbl= crs_nbr_dbl,
#                           overwrite_1L_lgl = overwrite_1L_lgl)
#   }
#   if(x %>% dplyr::pull(data_type_chr) == "Attribute"){
#     purrr::walk2(imports_ls,
#                  names(imports_ls),
#                  ~ write_att_tb(att_tb = .x,
#                                  object_nm_1L_chr = .y,
#                                  processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
#                                  overwrite_1L_lgl = overwrite_1L_lgl))
#
#   }
# }
# write_procsd_geom_imp <- function(x, # Now author vicinity_raw mthd
#                                   imports_ls,
#                                   path_to_seed_sf_1L_chr,
#                                   merge_itms_chr,
#                                   crs_nbr_dbl,
#                                   overwrite_1L_lgl){
#   ready4use::assert_single_row_tb(x)
#   if(overwrite_1L_lgl | !file.exists(path_to_seed_sf_1L_chr)){
#     if(is.na(merge_itms_chr) %>% all()){
#       starter_sf <- imports_ls[[1]]
#     }else{
#       starter_sf <- purrr::reduce(merge_itms_chr,
#                                   .init = imports_ls[[1]],
#                                   ~ make_intersecting_geometries(.x,
#                                                           eval(parse(text=.y)),
#                                                           crs_nbr_dbl = crs_nbr_dbl,
#                                                           validate_1L_lgl = T
#                                   ))
#       if((sf::st_geometry_type(starter_sf) %>% as.character()!="POINT") %>% any()){
#         starter_sf <- starter_sf %>%
#           dplyr::mutate(area = sf::st_area(.)) %>%
#           dplyr::filter(area > units::set_units(0,m^2)) ## Note: Will discard points
#       }
#     }
#     if(x %>% dplyr::pull(main_feature_chr) == "Boundary")
#       starter_sf <- starter_sf %>%
#         transform_to_simpler_sf(crs_dbl = crs_nbr_dbl[1])
#     saveRDS(starter_sf, file = path_to_seed_sf_1L_chr)
#   }
# }
