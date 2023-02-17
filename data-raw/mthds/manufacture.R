manufacture.vicinity_raw <- function(x, ## write_fls_and_mk_sngl_row_data_lup
                                     crs_nbr_dbl = NA_real_,
                                     match_value_xx = NULL,
                                     match_var_nm_1L_chr = character(0),
                                     merge_itms_chr = character(0),
                                     overwrite_1L_lgl = F,
                                     package_1L_chr  = character(0),
                                     processed_fls_dir_1L_chr = character(0),
                                     raw_fls_dir_1L_chr  = character(0),
                                     sub_dirs_chr  = character(0),
                                     what_1L_chr = "output"
                                     ){
  if(what_1L_chr == "ingest"){ #make_import_object
    object_xx <- NULL  # could pass custom fn to this method
    stop("A Make Import Object Method needs to be defined for the child class of vicinity_raw.") # Remove?
  }
  if(what_1L_chr == "output"){
    ready4use::assert_single_row_tb(x)
    y_VicinityLookup <- VicinityLookup()
    y_VicinityLookup <- renewSlot(y_VicinityLookup, "vicinity_raw_r3",x)
    import_type_ls <- procure.vicinity_raw(x,
                                           inc_script_1L_lgl = T,
                                           forced_choice_chr = NA_character_,
                                           what_1L_chr = "source") ####TF2A
    if(names(import_type_ls) == "script_chr"){
      make_class_fn_chr <- eval(parse(text = import_type_ls))
      script_args_ls <- list(lup_tbs_r4 = y_VicinityLookup,
                             merge_itms_chr = merge_itms_chr,
                             processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                             raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                             pkg_1L_chr = package_1L_chr,
                             overwrite_1L_lgl = overwrite_1L_lgl,
                             crs_nbr_dbl = crs_nbr_dbl)
      z_VicinityArguments <- rlang::exec(make_class_fn_chr, !!!script_args_ls)
      object_xx <- manufacture(z_VicinityArguments)
    }else{
      object_xx <- VicinityLocalRaw(lup_tbs_r4 = y_VicinityLookup,
                                    merge_itms_chr = merge_itms_chr,
                                    raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                                    pkg_1L_chr = package_1L_chr,
                                    overwrite_1L_lgl = overwrite_1L_lgl) %>% ## CLOSE CONDITIONAL, MOVE WHOLE CHUNK INTO REFORMED GET_IMPORT_TYPE_LS
        author(processed_fls_dir_1L_chr_chr = processed_fls_dir_1L_chr, # write_fls_from_imp_and_upd_r4
               crs_nbr_dbl = crs_nbr_dbl)
    }
  }
  if(what_1L_chr == "path"){ # get_sngl_path_for_imp
    # x <- x %>%
    #   dplyr::select(c(name_chr, country_chr, area_type_chr, region_chr,
    #                   main_feature_chr, year_chr, inc_file_main_chr))
    path_element_chr <- purrr::map_chr(c("country_chr", "area_type_chr", "region_chr",
                                         "main_feature_chr", "year_chr", "inc_file_main_chr"),
                                       ~ ready4::get_from_lup_obj(data_lookup_tb = x,
                                                                  match_var_nm_1L_chr = "name_chr",
                                                                  match_value_xx = match_value_xx,
                                                                  target_var_nm_1L_chr = .x,
                                                                  evaluate_1L_lgl = FALSE))
    object_xx <- paste0(raw_fls_dir_1L_chr, "/", paste(path_element_chr,collapse = "/"))
  }
  if(what_1L_chr == "paths_chr"){ # make_paths_chr
    paths_chr <- purrr::map_chr(sub_dirs_chr,
                                ~ ready4::get_from_lup_obj(data_lookup_tb = x_vicinity_raw,
                                                           match_value_xx = match_value_xx,
                                                           match_var_nm_1L_chr = match_var_nm_1L_chr,
                                                           target_var_nm_1L_chr = .x,
                                                           evaluate_1L_lgl = FALSE))
    paths_chr <- purrr::accumulate(paths_chr,
                                   ~ paste0(.x,"/",.y)) %>%
      paste0(processed_fls_dir_1L_chr,
             "/",
             .)
    object_xx <- paths_chr
  }
  if(what_1L_chr == "source"){
    assert_single_row_tb(x)
    import_type_ls <- procure(x,
                                      inc_script_1L_lgl = !is.null(script_args_ls),
                                      forced_choice_chr = forced_choice_chr,
                                      what_1L_chr = "source")
    object_xx <- switch(names(import_type_ls),
                        "script_chr" = rlang::exec(VicinityArguments, x, !!!script_args_ls), ## THIS LOOKS WRONG - Extra slot needed? Function required? ###TF2A
                        "local_chr" = ready4use::get_valid_path_chr(import_type_ls[[1]]),
                        "repo_chr"  = ready4use::manufacture.ready4use_dataverses(x %>% dplyr::select(names(ready4use::ready4use_dataverses())) %>%
                                                                                    tibble::as_tibble() %>%
                                                                                    ready4use::ready4use_dataverses() ## From ready4use manufacture mthd
                        ),
                        "source_url_chr" = url(import_type_ls[[1]])
    )
  }
  return(object_xx)
}
manufacture.vicinity_points <- function(x, # make_geomc_dist_bndys
                                        crs_nbr_dbl = numeric(0),
                                        distance_dbl = numeric(0),
                                        land_sf = NULL,
                                        service_1L_chr = character(0),
                                        time_min_1L_dbl = numeric(0),
                                        time_max_1L_dbl = numeric(0),
                                        time_steps_1L_dbl = numeric(0),
                                        type_1L_chr = "single",
                                        what_1L_chr = "geometric"
                                        ){
  if(what_1L_chr == "drive time"){
    if(type_1L_chr == "single"){ # make_drive_time_for_one_service #make_1_clstr_1_srvc_trvl_tm
        one_service_tb <- x %>%
          dplyr::filter(service_name_chr == service_1L_chr)
        one_service_sf <- make_drive_time_isochrones(lng_1L_dbl = one_service_tb %>% dplyr::select(lng_dbl) %>% dplyr::pull(),
                                                     lat_1L_dbl = one_service_tb %>% dplyr::select(lat_dbl) %>% dplyr::pull(),
                                                     time_min_1L_dbl = time_min_1L_dbl,
                                                     time_max_1L_dbl = time_max_1L_dbl,
                                                     time_steps_1L_dbl = time_steps_1L_dbl)
        object_xx <- one_service_sf
    }

  }
  if(what_1L_chr == "geometric"){ #make_geomc_dist_bndys
    distance_from_pts_sf <- sf::st_as_sf(x,
                                         coords = c("lng_dbl", "lat_dbl"),
                                         crs = crs_nbr_dbl[1]) %>% #4326)
      sf::st_transform(crs_nbr_dbl[2]) ##3577
    distance_from_pts_on_land_sf <- sf::st_buffer(distance_from_pts_sf,
                                                  dist = distance_dbl) %>%
      sf::st_union() %>%
      sf::st_intersection(land_sf %>%
                            sf::st_transform(crs_nbr_dbl[2])) %>% #3577
      sf::st_transform(crs_nbr_dbl[1]) %>%
      sf::st_sf()
    object_xx <- distance_from_pts_on_land_sf
  }

    return(object_xx)
  }
