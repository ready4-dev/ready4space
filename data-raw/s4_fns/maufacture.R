manufacture_VicinityArguments <- function(x,
                                          ...){
return(NULL)
}
manufacture_VicinityLookup <- function(x,
                                       area_sf = NULL,
                                       area_unit_1L_chr = character(0),
                                       att_data_xx = NULL,
                                       boundary_year_1L_chr = character(0),
                                       match_value_xx = character(0),
                                       path_1L_chr = character(0),
                                       type_1L_chr = "Geometry",
                                       what_1L_chr = "attribute",
                                       y_vicinity_raw = NULL,
                                       ... # Can remove???
                                       ){
  if(what_1L_chr == "attribute"){
    if(!identical(match_value_xx, character(0))){ # add_attr_list_to_sf
      att_data_xx <- manufacture(x, #make_att_data_xx
                                  match_value_xx = match_value_xx,
                                  area_sf = area_sf,
                                  what_1L_chr = "attribute_inner")
      object_xx <- add_att_to_sf(area_sf = area_sf,
                                  att_data_tb = att_data_xx,
                                  att_data_desc = ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3,
                                                                            match_value_xx = match_value_xx,
                                                                            match_var_nm_1L_chr = "name_chr",
                                                                            target_var_nm_1L_chr = "main_feature_chr"))

    }else{
      if(!identical(area_unit_1L_chr, character(0))){ # add_attr_recrly_to_sf
        boundary_sf <- ingest(x@vicinity_processed_r3 %>%
                                dplyr::filter(area_type_chr == area_unit_1L_chr) %>%
                                dplyr::filter(main_feature_chr == "Boundary") %>%
                                dplyr::filter(as.numeric(year_start_chr) == max(as.numeric(year_start_chr)[as.numeric(year_start_chr) <= as.numeric(boundary_year_1L_chr)])),
                              match_value_xx = "Boundary")
        attribute_data_ls <- purrr::map(att_data_xx,#attribute_data_chr,
                                        ~ .x) %>%
          stats::setNames(att_data_xx,#attribute_data_chr,
          )
        object_xx <- purrr::map(attribute_data_ls,
                                ~ manufacture(x, #add_attr_list_to_sf
                                              area_sf = boundary_sf,
                                              match_value_xx = .x)) %>%
          transform_sf_ls() %>%
          purrr::reduce(~rbind(.x,.y))

      }else{
        object_xx <- att_data_xx
      }
    }
  }
  if(what_1L_chr == "attribute_inner"){ # make_att_data_xx
    att_data_xx <- ingest(x@vicinity_processed_r3,
                           col_nm_1L_chr = "name_chr",
                           match_value_xx = match_value_xx)
    if(is.data.frame(att_data_xx)){
      att_data_xx <- list(att_data_xx) %>%
        stats::setNames(ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3,
                                                 match_value_xx = match_value_xx,
                                                 match_var_nm_1L_chr = "name_chr",
                                                 target_var_nm_1L_chr = "year_chr",
                                                 evaluate_1L_lgl = FALSE))
    }
    # START MOVE TO s2lsd
    region_short_nm_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3,
                                                       match_value_xx = match_value_xx,
                                                       match_var_nm_1L_chr = "name_chr",
                                                       target_var_nm_1L_chr = "region_chr",
                                                       evaluate_1L_lgl = FALSE)
    region_short_long_chr <- c(region_short_nm_1L_chr,
                               ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_abbreviations_r3,
                                                        match_value_xx = region_short_nm_1L_chr,
                                                        match_var_nm_1L_chr = "short_name_chr",
                                                        target_var_nm_1L_chr = "long_name_chr",
                                                        evaluate_1L_lgl = FALSE))
    area_names_var_chr <- ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3,
                                                   match_value_xx = match_value_xx,
                                                   match_var_nm_1L_chr = "name_chr",
                                                   target_var_nm_1L_chr = "area_type_chr",
                                                   evaluate_1L_lgl = FALSE) %>%
      ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_templates_r3,
                               match_value_xx = .,
                               match_var_nm_1L_chr = "area_type_chr",
                               target_var_nm_1L_chr = "subdivision_chr",
                               evaluate_1L_lgl = FALSE)
    area_names_var_chr <- area_names_var_chr[area_names_var_chr %in% names(area_sf)]
    boundary_year_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3,
                                                     match_value_xx = match_value_xx,
                                                     match_var_nm_1L_chr = "name_chr",
                                                     target_var_nm_1L_chr = "area_bndy_yr_chr",
                                                     evaluate_1L_lgl = F)
    area_names_var_chr <- x@vicinity_identifiers_r3 %>%
      dplyr::filter(var_name_chr %in% area_names_var_chr) %>%
      dplyr::filter(as.numeric(year_chr) == max(as.numeric(year_chr)[as.numeric(year_chr) <= as.numeric(boundary_year_1L_chr)])) %>%
      dplyr::pull(var_name_chr)
    ## End move to s2lsd
    att_data_xx <- manufacture(x,# updateAttrDataXx
                                att_data_xx = att_data_xx,
                                area_sf = area_sf,
                                area_names_var_chr = area_names_var_chr, # Remove when moved to s2lsd
                                region_short_long_chr = region_short_long_chr, # Remove when moved to s2lsd
                                match_value_xx = match_value_xx)
    object_xx <- att_data_xx
  }
  if(what_1L_chr == "imports"){ #make_imports_chr
    if(type_1L_chr == "Geometry"){
      imports_chr <- x@vicinity_raw_r3 %>%
        dplyr::filter(main_feature_chr == "Boundary") %>% dplyr::pull(name_chr)
    }else{
      imports_chr <- x@vicinity_raw_r3 %>%
        dplyr::filter(type_1L_chr == "Attribute") %>% dplyr::pull(name_chr)
      return(imports_chr)
    }
    object_xx <- imports_chr

  }
  if(what_1L_chr == "import_script"){ # make_merge_sf_chr
      if(is.null(y_vicinity_raw %>% dplyr::pull(add_boundaries_chr) %>% purrr::pluck(1))){
        script_1L_chr <- NA_character_
      }else{
        if(is.na(y_vicinity_raw %>% dplyr::pull(add_boundaries_chr) %>% purrr::pluck(1)) %>% any()){
          script_1L_chr <- NA_character_
        }else{
          script_1L_chr <- purrr::map_chr(y_vicinity_raw %>% pull(add_boundaries_chr) %>% purrr::pluck(1),
                                         ~ ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3,
                                                                    match_value_xx = .x,
                                                                    match_var_nm_1L_chr = "uid_chr",
                                                                    target_var_nm_1L_chr = "name_chr",
                                                                    evaluate_1L_lgl = FALSE) %>%
                                           ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3,
                                                                    match_value_xx = .,
                                                                    match_var_nm_1L_chr = "name_chr",
                                                                    target_var_nm_1L_chr = "source_reference_chr",
                                                                    evaluate_1L_lgl = FALSE) %>%
                                           ifelse(stringr::str_detect(.,"::"),
                                                  .,
                                                  paste0("readRDS(\"",path_1L_chr,"/",.,".rds\")")))
        }
      }
    object_xx <- script_1L_chr
  }
  return(object_xx)
}
manufacture_VicinityProfile <- function(x,
                                        attributes_ls = NULL,
                                        attributes_to_import_chr = character(0),
                                        exclude_dif_bndy_yr_1L_lgl = TRUE,
                                        highest_rsl_chr = character(0),
                                        #input_ls = NULL,
                                        key_var_1L_chr  = character(0),
                                        match_year_1L_lgl = FALSE,#TRUE,
                                        model_start_ymdhms_dtm = NULL,
                                        nbr_steps_start_to_end_1L_int = integer(0),
                                        reference_var_rsl_1L_chr = NULL,
                                        reference_vals_chr = c("tot_pop","age_sex"),
                                        simulation_steps_ymwd_dtm = NULL,
                                        specified_rsl_chr  = character(0),
                                        subdivision_1L_chr = NULL,
                                        type_1L_chr = "inner",
                                        what_1L_chr = "attributes",
                                        years_chr  = character(0)){
  if(what_1L_chr == "attributes"){
    if(type_1L_chr == "inner"){
      boundary_rsl_chr <- stringr::str_sub(attributes_to_import_chr,5,7) %>% unique() %>% toupper() ## Ammend from naming convention to lookup
      data_names_ls <- purrr::map(boundary_rsl_chr,
                                  ~ attributes_to_import_chr[stringr::str_sub(attributes_to_import_chr,5,7) == tolower(.x )]) %>%
        stats::setNames(boundary_rsl_chr)
      extra_names_chr <- purrr::map(specified_rsl_chr,
                                    ~ x@a_VicinityLookup@vicinity_processed_r3 %>%
                                      dplyr::filter(main_feature_chr == .x[1]) %>%
                                      dplyr::filter(make_filter_by_year_logic(data_tb = .,
                                                                              years_chr = years_chr)) %>%
                                      ready4::get_from_lup_obj(match_value_xx = .x[1],
                                                               match_var_nm_1L_chr = "main_feature_chr",
                                                               target_var_nm_1L_chr = "name_chr",
                                                               evaluate_1L_lgl = FALSE)) %>%
        stats::setNames(purrr::map_chr(specified_rsl_chr, ~.x[2]))
      rsl_to_merge_chr <- names(extra_names_chr)[names(extra_names_chr) %in% boundary_rsl_chr]
      if(!identical(rsl_to_merge_chr,character(0))){
        merged_elements_ls <-  purrr::map2(data_names_ls[rsl_to_merge_chr],
                                           extra_names_chr[rsl_to_merge_chr],
                                           ~ c(.x,.y))
        if(length(merged_elements_ls) == length(data_names_ls)){
          data_names_ls <- merged_elements_ls
        }else{
          data_names_ls <- append(data_names_ls[names(data_names_ls)[!names(data_names_ls) %in% rsl_to_merge_chr]],
                                  merged_elements_ls)

        }
      }
      extra_rsls_chr <- names(extra_names_chr)[!names(extra_names_chr) %in% boundary_rsl_chr]
      if(!identical(extra_rsls_chr,character(0))){
        data_names_ls <- append(data_names_ls,extra_names_chr[extra_rsls_chr])
        boundary_rsl_chr <- c(boundary_rsl_chr, extra_rsls_chr)
      }
      attributes_ls <- purrr::map2(boundary_rsl_chr,
                                   data_names_ls,
                                   ~ manufacture(x@a_VicinityLookup,#input_ls = input_ls, # add_attr_recrly_to_sf
                                                 #subdivision_1L_chr = subdivision_1L_chr,
                                                 area_unit_1L_chr = .x,
                                                 att_data_xx = .y,
                                                 boundary_year_1L_dbl = x@a_VicinityLookup@vicinity_processed_r3 %>%
                                                   dplyr::filter(name_chr %in% .y) %>%
                                                   dplyr::pull(year_chr) %>%
                                                   min(as.numeric()),
                                                 what_1L_chr = "attribute")) %>%
        stats::setNames(boundary_rsl_chr)
      index_ppr_dbl <- purrr::map_lgl(data_names_ls,
                                      ~ ratify.vicinity_processed(x@a_VicinityLookup@vicinity_processed_r3,#aus_spatial_lookup_tb,
                                                                  data_items_chr = .x,#validate_popl_predns_incld
                                                                  key_var_1L_chr = key_var_1L_chr,
                                                                  what_1L_chr = "population")) %>%
                                        which() + 1
      attributes_ls <- append(attributes_ls,# purrr::prepend
                              list(index_ppr_dbl=index_ppr_dbl), # index_ppr
                              after = 0)
    }
    if(type_1L_chr == "outer"){ #make_attributes_ls # get_sp_data or similar ???
      years_chr <- manufacture(x,
                               model_start_ymdhms_dtm = model_start_ymdhms_dtm,
                               nbr_steps_start_to_end_1L_int = nbr_steps_start_to_end_1L_int,
                               simulation_steps_ymwd_dtm = simulation_steps_ymwd_dtm,
                               what_1L_chr = "years")#make_years_chr
      attributes_to_import_chr = procure(x,#input_ls$x_VicinityProfile, # Formally get_spatial_attr_names(
                                         exclude_dif_bndy_yr_1L_lgl = exclude_dif_bndy_yr_1L_lgl,
                                         highest_rsl_chr = highest_rsl_chr,#input_ls$at_highest_res,
                                         key_var_1L_chr = key_var_1L_chr,#input_ls$key_var_1L_chr,
                                         #subdivision_1L_chr = NULL,
                                         match_year_1L_lgl = match_year_1L_lgl,
                                         years_chr = years_chr,
                                         what_1L_chr = "grouping")
      attributes_ls <- manufacture(x,#input_ls$x_VicinityProfile,
                                   attributes_to_import_chr = attributes_to_import_chr,
                                   key_var_1L_chr  = key_var_1L_chr,#input_ls$key_var_1L_chr,
                                   specified_rsl_chr  = specified_rsl_chr,#input_ls$at_specified_res,
                                   type_1L_chr = "inner",
                                   what_1L_chr = "attributes",
                                   years_chr  = years_chr)
    }
    if(type_1L_chr == "preliminary"){ # make_spatial_atts_ls
      lists_to_merge_ls <- purrr::map(subdivisions_chr,
                                      ~ manufacture(x,#make_attributes_ls
                                                    attributes_to_import_chr = attributes_to_import_chr,#
                                                    exclude_dif_bndy_yr_1L_lgl = exclude_dif_bndy_yr_1L_lgl,#TRUE,
                                                    # input_ls = input_ls,
                                                    highest_rsl_chr = highest_rsl_chr,#
                                                    key_var_1L_chr  = key_var_1L_chr,#
                                                    match_year_1L_lgl = match_year_1L_lgl,#FALSE,
                                                    model_start_ymdhms_dtm = model_start_ymdhms_dtm,#
                                                    nbr_steps_start_to_end_1L_int = nbr_steps_start_to_end_1L_int,#
                                                    reference_var_rsl_1L_chr = NULL,
                                                    simulation_steps_ymwd_dtm = simulation_steps_ymwd_dtm,#
                                                    specified_rsl_chr  = specified_rsl_chr,#
                                                    subdivision_1L_chr = .x,
                                                    type_1L_chr = "outer",
                                                    what_1L_chr = "attributes",
                                                    years_chr  = years_chr
                                      ))
      lists_to_merge_ls <- purrr::transpose(lists_to_merge_ls)
      merged_ls <- purrr::map(lists_to_merge_ls[2:length(lists_to_merge_ls)],
                              ~ do.call(rbind,.x))
      names_ppr_chr <- purrr::map_chr(lists_to_merge_ls[[1]],
                                      ~ ifelse(length(.x[1])==0,
                                               NA_character_,
                                               names(.x[1])))
      ppr_idx_dbl <- purrr::map_dbl(lists_to_merge_ls[[1]],
                                    ~ ifelse(length(.x[1])==0,
                                             NA_real_,
                                             .x[1])) %>%
        stats::setNames(names_ppr_chr)
      attribtues_ls <- append(merged_ls,list(ppr_idx_dbl = ppr_idx_dbl),
                              after = 0)
    }
    if(type_1L_chr == "updated"){ #update_spatial_atts_ls # Probably formerly extend_sp_data_ls or else combination of extend_sp_data_ls and reformatting from https://github.com/orygennp/ready4sim/blob/master/R/fn_make_sim_data_env.R
      # required in call: attributes_ls,highest_rsl_chr, key_var_1L_chr,reference_var_rsl_1L_chr,type_1L_chr, what_1L_chr
        group_by_var_1L_chr <- procure(x,#input_ls$x_VicinityProfile,#get_group_by_var_from_VicinityProfile
                                       what_1L_chr = "grouping")
        dynamic_var_rsl_1L_chr <- names(attributes_ls)[which(highest_rsl_chr == key_var_1L_chr) + 1] #input_ls$age_sex_pop_str
        reference_grouping_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = x@a_VivinityLookup@vicinity_identifiers_r3 %>% #lookup_tb(input_ls$x_VicinityProfile) %>% sp_uid_lup() %>%
                                                                dplyr::filter(year_chr %in% c(x@data_year_1L_chr)),
                                                              match_var_nm_1L_chr = "spatial_unit_chr",
                                                              match_value_xx = dynamic_var_rsl_1L_chr,
                                                              target_var_nm_1L_chr = "var_name_chr",
                                                              evaluate_1L_lgl = FALSE)
        #reference_var_rsl_1L_chr <- NULL
        if(!is.null(reference_var_rsl_1L_chr)){#input_ls$tot_pop_str
          reference_var_rsl_1L_chr <- names(attributes_ls)[which(highest_rsl_chr == reference_var_rsl_1L_chr) + 1] #input_ls$tot_pop_str
          use_reference_var_1L_lgl <- c(dynamic_var_rsl_1L_chr,reference_var_rsl_1L_chr) %>%
            purrr::map_dbl(~ready4::get_from_lup_obj(data_lookup_tb = x@a_VivinityLookup@vicinity_resolutions_r3,#input_ls$x_VicinityProfile %>% lookup_tb() %>% sp_resolution_lup()
                                                     match_var_nm_1L_chr = "area_type_chr",
                                                     match_value_xx = .x,
                                                     target_var_nm_1L_chr = "mean_size_dbl",
                                                     evaluate_1L_lgl = F)) %>%
            nnet::which.is.max() == 1
          if(!use_reference_var_1L_lgl)
            reference_var_rsl_1L_chr <- NULL
        }
        # if(use_coord_lup(input_ls$x_VicinityProfile))
        #   profiled_area_bands_ls <- purrr::map(profiled_area_bands_ls,
        #                                          ~ .x %>%
        #                                            sf::st_transform(crs_nbr(input_ls$x_VicinityProfile)[1]))
        by_band_pop_counts_sf_ls <- purrr::map(profiled_area_bands_ls,
                                               ~ make_reconciled_intersecting_area(profiled_sf = .x,
                                                                                   profiled_sf_col_1L_chr = NA,
                                                                                   profiled_sf_row_1L_chr = NA,
                                                                                   spatial_atts_ls = attributes_ls,
                                                                                   reference_var_rsl_1L_chr = reference_var_rsl_1L_chr,
                                                                                   dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                                                                   group_by_var_1L_chr = group_by_var_1L_chr,
                                                                                   reference_grouping_1L_chr = reference_grouping_1L_chr,
                                                                                   reference_vals_chr = reference_vals_chr,
                                                                                   data_year_1L_chr = x@data_year_1L_chr,#input_ls$x_VicinityProfile
                                                                                   crs_nbr_dbl = x@crs_dbl))
        by_band_pop_counts_sf_ls <- purrr::map2(by_band_pop_counts_sf_ls,
                                                names(by_band_pop_counts_sf_ls),
                                                ~ .x %>%
                                                  dplyr::mutate(popl_spatial_unit_chr = paste0(.y,
                                                                                               "_",
                                                                                               tolower(dynamic_var_rsl_1L_chr),
                                                                                               "_",
                                                                                               rownames(.x))) %>%
                                                  dplyr::mutate(popl_spatial_unit_area_dbl = sf::st_area(.)))
        profiled_sf <- do.call(rbind,by_band_pop_counts_sf_ls)
        featured_var_pfx_1L_chr <- make_featured_var_pfx(dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                                         reference_var_rsl_1L_chr = reference_var_rsl_1L_chr,
                                                         data_year_1L_chr = x@data_year_1L_chr)#input_ls$x_VicinityProfile
        profiled_sf <- remove_grouped_popl_vars(profiled_sf = profiled_sf,
                                                featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
        profiled_sf <- add_dynamic_vars_to_sf(dynamic_vars_sf = attributes_ls[[attributes_ls$ppr_idx_dbl[1]]] %>%
                                                dplyr::select(1),
                                              profiled_sf = profiled_sf,
                                              dynamic_var_rsl_1L_chr = "UNIT_ID",
                                              dynamic_var_nm_1L_chr = "popl_spatial_unit_chr",
                                              featured_var_pfx_1L_chr = featured_var_pfx_1L_chr,
                                              data_year_1L_chr = x@data_year_1L_chr,#input_ls$x_VicinityProfile
                                              crs_nbr_dbl = x@crs_dbl,
                                              reference_vals_chr = reference_vals_chr) #crs_nbr_dbl
        attributes_ls <- append(attributes_ls,# extended_spatial_atts_ls
                                list(profiled_sf = profiled_sf,
                                     featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)) # Is pop_val_prefix needed in this list?
    }
    object_xx <- attribtues_ls
  }
  if(what_1L_chr == "subdivisions"){ #make_profiled_area_objs
    #make_profiled_area_objs <- function(x_VicinityProfile){
      group_by_var_1L_chr <- procure(x,#get_group_by_var_from_VicinityProfile
                                     what_1L_chr = "grouping")
      st_profiled_sf <- ingest(x, # get_starter_sf_for_profiled_area
                               key_var_1L_chr = group_by_var_1L_chr)
      subdivision_var_nm_1L_chr <- ifelse(x@use_coord_lup_lgl,
                                          x@VicinityLookup@vicinity_identifiers_r3 %>%
                                            ready4::get_from_lup_obj(match_var_nm_1L_chr = "spatial_unit_chr",
                                                                     match_value_xx = x@rregion_type_chr,
                                                                     target_var_nm_1L_chr = "var_name_chr",
                                                                     evaluate_1L_lgl = F),
                                          ready4::get_from_lup_obj(data_lookup_tb = x@VicinityLookup@vicinity_templates_r3 %>%
                                                                     dplyr::filter(country_chr == x@country_chr) %>%
                                                                     dplyr::filter(area_bndy_yr_dbl == x@area_bndy_yr_dbl),
                                                                   match_var_nm_1L_chr = "area_type_chr",
                                                                   match_value_xx = x@area_type_chr,
                                                                   target_var_nm_1L_chr = "subdivision_chr",
                                                                   evaluate_1L_lgl = FALSE))
      if(!x@use_coord_lup_lgl){
        profiled_sf <- st_profiled_sf
        profiled_area_bands_ls <- make_sf_ls(profiled_sf = profiled_sf,
                                             group_by_var_1L_chr = group_by_var_1L_chr)
        subdivisions_chr <- profiled_sf %>%
          dplyr::pull(!!rlang::sym(subdivision_var_nm_1L_chr)) %>%
          as.character() %>%
          unique()
      }else{
        y_vicinity_points <- x@a_VicinityLookup@vicinity_points_r3 %>%
          dplyr::filter(service_name_chr %in% x@features_chr)
        if(!is.na(geom_dist_limit_km(x))){
          profiled_sf <- manufacture.vicinity_points(y_vicinity_points,
                                                     bands_1L_dbl = x@nbr_bands_dbl,#,
                                                     crs_nbr_dbl = x@crs_dbl,
                                                     land_sf =  st_profiled_sf,
                                                     metres_1L_dbl = x@geomc_dist_limit_km_dbl *1000, # make_distance_based_bands
                                                     type_1L_chr = "bands",
                                                     what_1L_chr = "geometric"
          )[[1]]
          profiled_area_bands_ls <- make_sf_ls(profiled_sf = profiled_sf,
                                               group_by_var_1L_chr = group_by_var_1L_chr)
        }
        if(!is.na(drive_time_limit_mins(x))){
          profiled_area_bands_ls <- make_cluster_isochrones(vicinity_points_ls = list(y_vicinity_points),
                                                            index_val_1L_int = 1,
                                                            time_min_1L_dbl = 0,
                                                            time_max_1L_dbl = x@drive_time_limit_mins_dbl,
                                                            time_steps_1L_dbl = x@nbr_bands_dbl,
                                                            travel_mode_1L_chr = x@travel_mode_chr
                                                            )
          names(profiled_area_bands_ls) <- paste0("dt_band_",1:length(profiled_area_bands_ls))
          profiled_sf <- do.call(rbind,profiled_area_bands_ls) %>%
            sf::st_transform(x@crs_dbl[1]) %>%
            transform_to_simpler_sf() # add crs??
        }
        subdivisions_chr <- make_intersecting_geometries(geometry_one_sf = st_profiled_sf,
                                                         geometry_two_sf = profiled_sf,
                                                         crs_nbr_dbl = x@crs_dbl) %>%
          dplyr::pull(!!rlang::sym(subdivision_var_nm_1L_chr)) %>%
          as.vector()%>%
          unique()
      }
      profiled_area_objs_ls <- list(subdivisions_chr = subdivisions_chr,
                                    profiled_sf = profiled_sf,
                                    profiled_area_bands_ls = profiled_area_bands_ls)
      object_xx <- profiled_area_objs_ls
    #}
  }
  if(what_1L_chr == "years"){ #make_years_chr
    #make_years_chr <- function(input_ls){
      model_end_year_dbl <- calculate_end_date(model_start_ymdhms_dtm = model_start_ymdhms_dtm,
                                               nbr_steps_start_to_end_1L_int = nbr_steps_start_to_end_1L_int,
                                               simulation_steps_ymwd_dtm = simulation_steps_ymwd_dtm) %>% lubridate::year()
      #key_var_1L_chr <- input_ls$key_var_1L_chr
      #data_year_1L_chr <- x@data_year_1L_chr
      #spatial_lookup_tb <- x@a_VicinityLookup@vicinity_processed_r3
      year_opts_chr <- x@a_VicinityLookup@vicinity_processed_r3 %>%
        dplyr::filter(main_feature_chr == key_var_1L_chr) %>%
        dplyr::pull(year_end_chr)
      year_opts_chr <- year_opts_chr[stringr::str_length(year_opts_chr)==4]
      year_opts_ref_dbl <- which((year_opts_chr %>%
                                as.numeric() %>%
                                sort()) >= model_end_year_dbl) %>% min()
      model_end_year_dbl <- year_opts_chr %>%
        as.numeric() %>%
        sort() %>% purrr::pluck(year_opts_ref_dbl) %>%
        as.character()
      years_chr <- as.character(as.numeric(x@data_year_1L_chr):as.numeric(model_end_year_dbl))
      object_xx <- years_chr
    #}
  }
  return(object_xx)
}

# methods::setMethod("updateAttrDataXx",
#                    "VicinityLookup",
#                    function(x,
#                             att_data_xx,
#                             altv_names_sf,
#                             area_names_var_chr,
#                             region_short_long_chr) {
#                      att_data_xx
#                    })
