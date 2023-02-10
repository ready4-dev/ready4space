write_attr_tb <- function(attr_tb,
                          obj_name,
                          processed_dir,
                          overwrite_1L_lgl){
  path_to_attr_tb_chr <- get_r_import_path_chr(r_data_dir_chr = processed_dir,
                                               name_chr = obj_name,
                                               data_type_chr = "Attribute")
  if(overwrite_1L_lgl | !file.exists(path_to_attr_tb_chr))
    saveRDS(attr_tb, file = path_to_attr_tb_chr)
}
write_dirs_for_imp <- function(directory_paths){

  purrr::walk(directory_paths,
              ~ dir.create(.x))
}
write_fls_and_mk_sngl_row_data_lup <- function(x, ## MAKE METHOD
                                merge_with,
                                pckg_name,
                                raw_data_dir,
                                processed_dir,
                                crs_nbr_dbl = NA_real_,
                                overwrite_1L_lgl = F){
  ready4use::assert_single_row_tb(x)
  lookup_tbs_r4 <- VicinityLookup()
  lookup_tbs_r4 <- `sp_import_lup<-`(lookup_tbs_r4,x)
  import_type_ls <- ready4use::procure(x)
  if(names(import_type_ls) == "script_chr"){
    make_class_fn_chr <- eval(parse(text = import_type_ls))
    script_args_ls <- list(lup_tbs_r4 = lookup_tbs_r4,
                           merge_itms_chr = merge_with,
                           processed_fls_dir_1L_chr = processed_dir,
                           raw_fls_dir_1L_chr = raw_data_dir,
                           pkg_1L_chr = pckg_name,
                           overwrite_1L_lgl = overwrite_1L_lgl,
                           crs_nbr_dbl = crs_nbr_dbl)
    script_data_r4 <- rlang::exec(make_class_fn_chr, !!!script_args_ls)
    import_data(script_data_r4)
  }else{
    VicinityLocalRaw(lup_tbs_r4 = lookup_tbs_r4,
                        merge_itms_chr = merge_with,
                        raw_fls_dir_1L_chr = raw_data_dir,
                        pkg_1L_chr = pckg_name,
                        overwrite_1L_lgl = overwrite_1L_lgl) %>% ## CLOSE CONDITIONAL, MOVE WHOLE CHUNK INTO REFORMED GET_IMPORT_TYPE_LS
      write_fls_from_imp_and_upd_r4(processed_dir_chr = processed_dir,
                             crs_nbr_dbl = crs_nbr_dbl)
  }
}
write_fls_for_imp <- function(x,
                              data_lookup_ref,
                              lookup_variable,
                              directory_path,
                              overwrite_1L_lgl = F){
  write_1L_lgl <- F
  download_components_vec <- purrr::map_chr(c("file_name_chr",
                                              "file_type_chr",
                                              "download_url_chr",
                                              "inc_file_main_chr",
                                              "local_file_src_chr",
                                              "data_repo_db_ui_chr"),
                                            ~ ready4fun::get_from_lup(data_lookup_tb = x,
                                                                      lookup_reference = data_lookup_ref,
                                                                      lookup_variable = lookup_variable,
                                                                      target_variable = .x,
                                                                      evaluate = FALSE))
  dest_file <- paste0(directory_path,
                      "/",
                      download_components_vec[1],
                      download_components_vec[2])
  if(!is.na(download_components_vec[5])){
    if(overwrite_1L_lgl | file.exists(dest_file))
      file.copy(from = download_components_vec[5],to = dest_file)
  }else{
    if(!is.na(paste0(directory_path,
                     "/",
                     download_components_vec[4]))){
      if(overwrite_1L_lgl | !file.exists(paste0(directory_path, ## NEEDS UPDATING TO REFERENCE RENAMED PRE-EXISTING FILES
                                             "/",
                                             download_components_vec[4]))){
        if(!is.na(download_components_vec[6])){
          ready4use::procure(x,
                              save_dir_path_chr = directory_path,
                              unlink_lgl = F)

        }else{
          utils::download.file(download_components_vec[3],
                               destfile = dest_file,
                               mode = 'wb')
        }
        if(download_components_vec[2] == ".zip"){
          utils::unzip(dest_file,
                       exdir = directory_path)
        }
        write_to_rnm_fls_for_imp(x = x,
                                 data_lookup_ref = data_lookup_ref,
                                 lookup_variable = lookup_variable,
                                 directory_path = directory_path,
                                 overwrite_1L_lgl = overwrite_1L_lgl)
        write_1L_lgl <- T
      }
    }
  }
  write_1L_lgl
}
write_fls_from_imp_and_upd_r4 <- function(x,
                                          processed_dir_chr,
                                          crs_nbr_dbl){
  save_raw(x,
           return_r4_lgl = T) %>%
    ready4use::`processed_fls_dir_1L_chr<-`(processed_dir_chr) %>%
    import_data(crs_nbr_dbl = crs_nbr_dbl) %>%
    update_this()
}
write_fls_from_local_imp <- function(x,
                                     raw_fls_dir_1L_chr,
                                     write_1L_lgl){
  x %>%
    ready4use::`write_1L_lgl<-`(write_1L_lgl) %>%
    ready4use::`raw_fls_dir_1L_chr<-`(raw_fls_dir_1L_chr)
}
write_fls_from_sp_imp_and_upd_imp_ls <- function(x,
                                                 crs_nbr_dbl,
                                                 return_r4_lgl = T) {
  sp_import_lup <- x@lup_tbs_r4@sp_import_lup
  ready4use::assert_single_row_tb(sp_import_lup)
  imports_ls <- import_data(x = sp_import_lup,
                                included_items_names = x@imports_chr,
                                item_data_type = sp_import_lup$data_type,
                                data_directory = x@raw_fls_dir_1L_chr,
                                r_data_dir_chr = x@processed_fls_dir_1L_chr,
                                write_1L_lgl = x@write_1L_lgl) %>%
    stats::setNames(x@imports_chr)
  if(sp_import_lup$data_type == "Geometry"){
    path_to_seed_sf_1L_chr <- get_r_import_path_chr(r_data_dir_chr = x@processed_fls_dir_1L_chr,
                                                    name_chr = names(imports_ls)[1],
                                                    data_type_chr = "Geometry")
  }else{
    path_to_seed_sf_1L_chr <- NA_character_
  }
  write_procsd_imp_xx(x = sp_import_lup,
                      imports_ls = imports_ls,
                      path_to_seed_sf_1L_chr = path_to_seed_sf_1L_chr,
                      merge_with = x@merge_itms_chr,
                      processed_dir = x@processed_fls_dir_1L_chr,
                      crs_nbr_dbl = crs_nbr_dbl,
                      overwrite_1L_lgl = x@overwrite_1L_lgl)
  if(return_r4_lgl)
    ready4use::`path_to_seed_sf_1L_chr<-`(x,path_to_seed_sf_1L_chr) %>%
    ready4use::`imports_ls<-`(imports_ls)
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
                                  ~ intersect_lon_lat_sfs(.x,
                                                          eval(parse(text=.y)),
                                                          crs_nbr_dbl = crs_nbr_dbl,
                                                          validate_lgl = T
                                  ))
      if((sf::st_geometry_type(starter_sf) %>% as.character()!="POINT") %>% any()){
        starter_sf <- starter_sf %>%
          dplyr::mutate(area = sf::st_area(.)) %>%
          dplyr::filter(area > units::set_units(0,m^2)) ## Note: Will discard points
      }
    }
    if(x %>% dplyr::pull(main_feature_chr) == "Boundary")
      starter_sf <- starter_sf %>%
        simplify_sf(crs = crs_nbr_dbl[1])
    saveRDS(starter_sf, file = path_to_seed_sf_1L_chr)
  }
}
write_procsd_imp_xx <- function(x,
                                imports_ls,
                                path_to_seed_sf_1L_chr,
                                merge_with,
                                pckg_name,
                                raw_data_dir,
                                processed_dir,
                                crs_nbr_dbl = NA_real_,
                                overwrite_1L_lgl = F){
  if(x %>% dplyr::pull(data_type_chr) == "Geometry"){
    write_procsd_geom_imp(x,
                          imports_ls = imports_ls,
                          path_to_seed_sf_1L_chr = path_to_seed_sf_1L_chr,
                          #processed_dir = processed_dir,
                          merge_itms_chr = merge_with,
                          crs_nbr_dbl= crs_nbr_dbl,
                          overwrite_1L_lgl = overwrite_1L_lgl)
  }
  if(x %>% dplyr::pull(data_type_chr) == "Attribute"){
    purrr::walk2(imports_ls,
                 names(imports_ls),
                 ~ write_attr_tb(attr_tb = .x,
                                 obj_name = .y,
                                 processed_dir = processed_dir,
                                 overwrite_1L_lgl = overwrite_1L_lgl))

  }
}
write_raw_data_from_sp_local_r4 <- function(x,
                                            return_r4_lgl){
  sp_import_lup <- x@lup_tbs_r4@sp_import_lup
  ready4use::assert_single_row_tb(sp_import_lup)
  raw_format_sp_dir <- write_raw_format_dir(data_type_chr = sp_import_lup$data_type_chr,
                                            raw_data_dir = x@raw_fls_dir_1L_chr)
  imports_chr <- get_imports_chr(x@lup_tbs_r4,
                                       data_type_chr = sp_import_lup$data_type_chr)
  write_1L_lgl <- save_raw(x = sp_import_lup,
                       required_data = imports_chr,
                       destination_directory = raw_format_sp_dir,
                       overwrite_1L_lgl = x@overwrite_1L_lgl)
  if(return_r4_lgl){
    makeProcessed_r4(x,
                       imports_chr = imports_chr,
                       raw_fls_dir_1L_chr = raw_format_sp_dir,
                       write_1L_lgl = write_1L_lgl)

  }
}
write_raw_format_dir <- function(data_type_chr,
                                 raw_data_dir){
  directory_chr <- switch(data_type_chr, "Geometry" = "Geometries","Attribute" = "Attributes")
  raw_format_sp_dir <- make_raw_format_dir_chr(raw_data_dir,directory_chr)
  if(!dir.exists(raw_format_sp_dir))
    dir.create(raw_format_sp_dir)
  raw_format_sp_dir
}
write_to_rnm_fls_for_imp <- function(x,
                                     data_lookup_ref,
                                     lookup_variable,
                                     directory_path,
                                     overwrite_1L_lgl = F){
  old_names_list <- ready4fun::get_from_lup(data_lookup_tb = x,
                                            lookup_reference = data_lookup_ref,
                                            lookup_variable = lookup_variable,
                                            target_variable = "inc_fls_to_rename_ls",
                                            evaluate = FALSE)
  new_names_list <- ready4fun::get_from_lup(data_lookup_tb = x,
                                            lookup_reference = data_lookup_ref,
                                            lookup_variable = lookup_variable,
                                            target_variable = "new_nms_for_inc_fls_ls",
                                            evaluate = FALSE)
  if(!is.na(old_names_list)){
    purrr::walk2(old_names_list,
                 new_names_list,
                 ~ if(overwrite_1L_lgl | !file.exists(paste0(directory_path,
                                                          "/",
                                                          .y)))
                   file.rename(paste0(directory_path,
                                      "/",
                                      .x),
                               paste0(directory_path,
                                      "/",
                                      .y)))

  }
}
