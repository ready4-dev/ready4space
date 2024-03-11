manufacture.vicinity_abbreviations <- function(x,
                                               agent_areas_tb = NULL,
                                               area_names_chr = character(0),
                                               area_var_nm_1L_chr = "Suburb",
                                               areas_sf = NULL,
                                               correspondences_lup = NULL,
                                               large_area_var_nm_1L_chr = character(0),#STE_NAME16
                                               large_areas_chr = character(0),##state_names,
                                               large_area_idx_1L_int = 1L,
                                               match_value_xx = NULL,#"hYEPP"
                                               match_var_nm_1L_chr = character(0),#TeamName
                                               outliers_chr = character(0),
                                               small_area_var_nm_1L_chr = character(0),#SSC_NAME16
                                               title_case_1L_lgl = T,
                                               type_1L_chr = "suffix",
                                               unknown_area_val_1L_chr = "Unk",
                                               valid_names_chr = character(0),
                                               what_1L_chr = "areas"                                               ){
  if(what_1L_chr == "areas"){
    if(nrow(x)>0){
      suffix_1L_chr <- x %>% dplyr::filter(long_nm_chr == !!(match_value_xx)) %>% dplyr::pull(short_nm_chr)
      if(type_1L_chr == "geometry"){
        agent_areas_tb <- transform_agent_areas(agent_areas_tb,
                                                area_var_nm_1L_chr = area_var_nm_1L_chr,
                                                match_var_nm_1L_chr = match_var_nm_1L_chr,
                                                match_value_xx = match_value_xx ,
                                                title_case_1L_lgl = title_case_1L_lgl)
        included_areas_chr <- get_agent_areas(agent_areas_tb,
                                              area_var_nm_1L_chr = area_var_nm_1L_chr,
                                              type_1L_chr = "names")
        ## Get rid of / reassign "Unk"
        all_areas_included_regions_chr <- areas_sf %>%
          dplyr::filter(!!rlang::sym(large_area_var_nm_1L_chr) %in% large_areas_chr) %>% dplyr::pull(!!rlang::sym(small_area_var_nm_1L_chr)) %>%
          as.character()
        unmatched_areas_chr <- setdiff(included_areas_chr,
                                       all_areas_included_regions_chr)
        matched_as_raw_chr <- included_areas_chr[!included_areas_chr %in% unmatched_areas_chr]
        with_sfx_chr <- manufacture.vicinity_abbreviations(x,#add_area_suffix
                                                           area_names_chr = unmatched_areas_chr,
                                                           match_value_xx = large_areas_chr[large_area_idx_1L_int],
                                                           type_1L_chr = "suffix",
                                                           what_1L_chr = "areas")
        unmatched_areas_with_sfx_chr <- setdiff(with_sfx_chr, all_areas_included_regions_chr)
        matched_with_sfx_chr <- setdiff(with_sfx_chr,unmatched_areas_with_sfx_chr)
        suffix_length_1L_int <- get_from_lup_obj(x,
                                                 match_value_xx = large_areas_chr[large_area_idx_1L_int],
                                                 match_var_nm_1L_chr = "long_name_chr",
                                                 target_var_nm_1L_chr = "long_name_chr") %>%
          nchar() + 3
        unmatched_areas_raw_chr <- unmatched_areas_with_sfx_chr %>% stringr::str_sub(1,stringr::str_length(.)-suffix_length_1L_int)
        manual_changes_chr <- purrr::map(unmatched_areas_raw_chr,
                                         ~ rename_areas(.x,
                                                        correspondences_lup = correspondences_lup)) %>%
          unlist()
        matched_manual_changes_chr <- manual_changes_chr[!stringr::str_detect(manual_changes_chr,
                                                                              setdiff(manual_changes_chr,
                                                                                      areas_sf %>%
                                                                                        dplyr::pull(SSC_NAME16) %>%
                                                                                        as.character()))]
        updated_included_areas_chr <- c(matched_as_raw_chr,
                                        matched_with_sfx_chr,
                                        matched_manual_changes_chr) %>%
          stringr::str_sort()
        updated_client_locations <- agent_areas_tb  %>% ################# CHECK IF ROWWISE APPROPRIATE
          dplyr::rowwise() %>%
          dplyr::mutate(!!rlang::sym(area_var_nm_1L_chr) := manufacture.vicinity_abbreviations(x,#transform_area_nms(Suburb,valid_names_chr = updated_included_areas_chr, match_value_xx = large_areas_chr[1], correspondences_lup = correspondences_lup)
                                                                                               area_names_chr = !!rlang::sym(area_var_nm_1L_chr),
                                                                                               correspondences_lup = correspondences_lup,
                                                                                               match_value_xx = large_areas_chr[large_area_idx_1L_int],
                                                                                               type_1L_chr = "transformation",
                                                                                               valid_names_chr = updated_included_areas_chr,
                                                                                               what_1L_chr = "areas"))
        area_summary_tb <- updated_client_locations %>%
          dplyr::group_by(!!rlang::sym(area_var_nm_1L_chr)) %>%
          dplyr::summarise(clients=n())
        unknown_area_1L_chr <-  manufacture.vicinity_abbreviations(x,#add_area_suffix(x="Unk",  s_t = large_areas_chr[1])
                                                                   area_names_chr = unknown_area_val_1L_chr,
                                                                   match_value_xx = large_areas_chr[large_area_idx_1L_int],
                                                                   type_1L_chr = "suffix",
                                                                   what_1L_chr = "areas")
        known_area_summary_tb <- area_summary_tb %>%
          dplyr::filter(!!rlang::sym(area_var_nm_1L_chr) != unknown_area_1L_chr)
        known_area_summary_tb <- known_area_summary_tb %>%
          dplyr::rename(!!rlang::sym(small_area_var_nm_1L_chr):=!!rlang::sym(area_var_nm_1L_chr))
        areas_sf <- dplyr::inner_join(areas_sf,
                                      known_area_summary_tb)
        if(!identical(outliers_chr, character(0))){
          areas_sf <- remove_outlier_areas(areas_sf,# suburb_based_effective_catch_excl_outliers
                                           outliers_chr = outliers_chr,
                                           area_var_nm_1L_chr = small_area_var_nm_1L_chr)
        }
        object_xx <- areas_sf
      }
      if(type_1L_chr == "suffix"){
        object_xx <- paste0(area_names_chr,
                            paste0(" (",suffix_1L_chr,")"))
      }
      if(type_1L_chr == "main"){
        object_xx <- stringr::str_replace(area_names_chr,
                                          paste0(" (",suffix_1L_chr,")"),
                                          "")
      }
      if(type_1L_chr == "transformation"){
        if(title_case_1L_lgl){
          area_names_chr <- stringr::str_to_title(area_names_chr)
        }
        if(!is.null(correspondences_lup)){
          area_names_chr <- rename_areas(area_names_chr,
                                         correspondences_lup = correspondences_lup)
        }
        if(any(!area_names_chr %in% valid_names_chr)){ # any? all?
          area_names_chr <- manufacture.vicinity_abbreviations(x,#add_area_suffix(area_names_chr = area_names_chr,large_area_1L_chr = match_value_xx) #paste0(area_names_chr," (NSW)")
                                                               area_names_chr = area_names_chr,
                                                               match_value_xx = match_value_xx,
                                                               type_1L_chr = "suffix",
                                                               what_1L_chr = "areas")


        }
        object_xx <- area_names_chr
      }
    }else{
      object_xx <- area_names_chr
    }
  }
  return(object_xx)
}
manufacture.vicinity_parameters<- function(x,# make_env_param_tb #env_str_param_tb
                                           y_vicinity_mapes = NULL,#mape_str_param_tb,
                                           n_its_int = integer(0),
                                           joint_dstr_1L_lgl = T,
                                           what_1L_chr = "values"){
  if(what_1L_chr == "values"){
    param_vals_tb <- reckon(x,
                            n_its_int = n_its_int)
    if(!is.null(y_vicinity_mapes)){
      mape_vals_tb <- reckon(y_vicinity_mapes,
                             n_its_int = n_its_int,
                             joint_dstr_1L_lgl = joint_dstr_1L_lgl)
      param_vals_tb  <- dplyr::bind_rows(param_vals_tb,
                                         mape_vals_tb)
    }
    object_xx <- param_vals_tb
  }
  return(object_xx)
}
manufacture.vicinity_points <- function(x, # make_geomc_dist_bndys
                                        bands_1L_dbl = numeric(0),
                                        crs_nbr_dbl = numeric(0),
                                        land_sf = NULL,
                                        metres_1L_dbl = numeric(0),
                                        service_1L_chr = character(0),
                                        time_min_1L_dbl = numeric(0),
                                        time_max_1L_dbl = numeric(0),
                                        time_steps_1L_dbl = numeric(0),
                                        travel_mode_1L_chr = "car",
                                        type_1L_chr = "single",
                                        what_1L_chr = "geometric"
                                        ){
  if(what_1L_chr == "isochrones"){ #"drive time"
    if(type_1L_chr == "single"){ # make_drive_time_for_one_service #make_1_clstr_1_srvc_trvl_tm # possibly: make_isochrs_for_1_srvc
        one_service_tb <- x %>%
          dplyr::filter(service_name_chr == service_1L_chr)
        one_service_sf <- make_isochrones(lat_1L_dbl = one_service_tb %>% dplyr::select(lat_dbl) %>% dplyr::pull(),
                                                     lng_1L_dbl = one_service_tb %>% dplyr::select(lng_dbl) %>% dplyr::pull(),
                                                     time_min_1L_dbl = time_min_1L_dbl,
                                                     time_max_1L_dbl = time_max_1L_dbl,
                                                     time_steps_1L_dbl = time_steps_1L_dbl,
                                                     travel_mode_1L_chr = travel_mode_1L_chr)
        object_xx <- one_service_sf
    }

  }
  if(what_1L_chr == "geometric"){ #make_geomc_dist_bndys
    if(type_1L_chr == "single"){
      distance_from_pts_sf <- sf::st_as_sf(x,
                                           coords = c("lng_dbl", "lat_dbl"),
                                           crs = crs_nbr_dbl[1]) %>% #4326)
        sf::st_transform(crs_nbr_dbl[2]) ##3577
      distance_from_pts_on_land_sf <- sf::st_buffer(distance_from_pts_sf,
                                                    dist = metres_1L_dbl) %>%
        sf::st_union() %>%
        sf::st_intersection(land_sf %>%
                              sf::st_transform(crs_nbr_dbl[2])) %>% #3577
        sf::st_transform(crs_nbr_dbl[1]) %>%
        sf::st_sf()
      object_xx <- distance_from_pts_on_land_sf
    }
    if(type_1L_chr == "bands"){#make_distance_based_bands
            # make_distance_based_bands <- function(x,
      #                                       distance_in_km_1L_dbl, #####
      #                                       profiled_sf, ### land_sf,
      #                                       ){
        distance_in_km_1L_dbl <- metres_1L_dbl * 1000
        distances_dbl <- seq(from = distance_in_km_1L_dbl/bands_1L_dbl,
                             to = distance_in_km_1L_dbl,
                             by = distance_in_km_1L_dbl/bands_1L_dbl)

        service_clusters_chr <- x %>% dplyr::pull(cluster_name_chr) %>% unique()
        service_vicinity_points_ls <- purrr::map(service_clusters_chr,
                                                ~ x %>%
                                                  dplyr::filter(cluster_name_chr == .x)) %>%
          stats::setNames(service_clusters_chr)
        service_clusters_by_distance_ls <- purrr::map(distances_dbl,
                                                        ~ make_cluster_bndys(clusters_chr = service_clusters_chr,
                                                                             crs_nbr_dbl = crs_nbr_dbl,
                                                                             distance_in_km_1L_dbl = .x,
                                                                             land_boundary_sf = land_sf,
                                                                             vicinity_points_ls = service_vicinity_points_ls)) %>%
          stats::setNames(., paste0("km_",
                                    distances_dbl,
                                    "from_service"))
        geometric_distance_by_cluster_circles <- purrr::map(1:length(service_clusters_chr),
                                                            ~ reorder_clusters_by_distances(clusters_by_distance_ls = service_clusters_by_distance_ls,
                                                                                               distances_dbl = distances_dbl,
                                                                                               index_val_1L_int = .x)) %>%
          stats::setNames(., service_vicinity_points_ls %>% names())
        geometric_distance_by_cluster_bands <- purrr::map(geometric_distance_by_cluster_circles,
                                                          ~ transform_circles_to_bands(geomc_dist_circles_ls = .x)) %>%
          stats::setNames(., service_vicinity_points_ls %>% names())
        geometric_distance_by_cluster_circles_merged_list <- purrr::map(geometric_distance_by_cluster_circles,
                                                                        ~ do.call(rbind,.x)) %>%
          stats::setNames(., service_vicinity_points_ls %>% names()) %>%
          purrr::map(.,
                     ~ .x %>% dplyr::arrange(desc(distance_in_km_dbl)))
        geometric_distance_by_cluster_bands_merged_list <- purrr::map(geometric_distance_by_cluster_bands,
                                                                      ~ do.call(rbind,.x)) %>%
          stats::setNames(., service_vicinity_points_ls %>% names()) %>%
          purrr::map(.,
                     ~ .x %>% dplyr::arrange(desc(distance_in_km_dbl)) %>%
                       transform_to_simpler_sf(crs_dbl = crs_nbr_dbl[1]))
        object_xx <- geometric_distance_by_cluster_bands_merged_list
    }
  }
    return(object_xx)
  }
manufacture.vicinity_processed <- function(x,
                                           approximation_1L_chr = "abs",
                                           main_incld_feature_chr = character(0),
                                           target_area_1L_chr = character(0),
                                           target_year_1L_chr = character(0),
                                           what_1L_chr = "closest years"){
  if(what_1L_chr == "closest years"){
    closest_yrs_ls <- make_closest_yrs_ls(x,
                                          main_incld_feature_chr = main_incld_feature_chr,
                                          target_area_1L_chr = target_area_1L_chr,
                                          target_year_1L_chr = target_year_1L_chr,
                                          approximation_1L_chr = approximation_1L_chr)
    object_xx <- closest_yrs_ls
  }
    return(object_xx)
}
manufacture.vicinity_raw <- function(x, ## write_fls_and_mk_sngl_row_data_lup
                                     args_ls = NULL,
                                     crs_nbr_dbl = NA_real_,
                                     fn = function(x,...){NULL},
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
    object_xx <- rlang::exec(fn,x,!!!args_ls)#NULL  # could pass custom fn to this method
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
      script_args_ls <- list(a_VicinityLookup = y_VicinityLookup,
                             merge_itms_chr = merge_itms_chr,
                             processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                             raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                             pkg_1L_chr = package_1L_chr,
                             overwrite_1L_lgl = overwrite_1L_lgl,
                             crs_nbr_dbl = crs_nbr_dbl)
      z_VicinityArguments <- rlang::exec(make_class_fn_chr, !!!script_args_ls)
      object_xx <- author(z_VicinityArguments) ## Changed from manufacture
    }else{
      object_xx <- VicinityLocalRaw(a_VicinityLookup = y_VicinityLookup,
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
                                ~ ready4::get_from_lup_obj(data_lookup_tb = x,#x_vicinity_raw
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
                        "script_chr" = rlang::exec(VicinityArguments, ## THIS MAY BE WRONG - Does the specified class need to allow for inheriting structures ###TF2A
                                                  # x,
                                                   !!!script_args_ls),
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
