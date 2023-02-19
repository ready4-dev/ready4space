manufacture_VicinityArguments <- function(x,
                                          ...){
return(NULL)
}
manufacture_VicinityLookup <- function(x,
                                       area_sf = NULL,
                                       area_unit_1L_chr = character(0),
                                       attr_data_xx = NULL,
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
      attr_data_xx <- manufacture(x, #make_attr_data_xx
                                  match_value_xx = match_value_xx,
                                  area_sf = area_sf,
                                  what_1L_chr = "attribute_inner")
      object_xx <- add_attr_to_sf(area_sf = area_sf,
                                  attr_data_tb = attr_data_xx,
                                  attr_data_desc = ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3,
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
        attribute_data_ls <- purrr::map(attr_data_xx,#attribute_data_chr,
                                        ~ .x) %>%
          stats::setNames(attr_data_xx,#attribute_data_chr,
          )
        object_xx <- purrr::map(attribute_data_ls,
                                ~ manufacture(x, #add_attr_list_to_sf
                                              area_sf = boundary_sf,
                                              match_value_xx = .x)) %>%
          transform_sf_ls() %>%
          purrr::reduce(~rbind(.x,.y))

      }else{
        object_xx <- attr_data_xx
      }
    }
  }
  if(what_1L_chr == "attribute_inner"){ # make_attr_data_xx
    attr_data_xx <- ingest(x@vicinity_processed_r3,
                           col_nm_1L_chr = "name_chr",
                           match_value_xx = match_value_xx)
    if(is.data.frame(attr_data_xx)){
      attr_data_xx <- list(attr_data_xx) %>%
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
    attr_data_xx <- manufacture(x,# updateAttrDataXx
                                attr_data_xx = attr_data_xx,
                                area_sf = area_sf,
                                area_names_var_chr = area_names_var_chr, # Remove when moved to s2lsd
                                region_short_long_chr = region_short_long_chr, # Remove when moved to s2lsd
                                match_value_xx = match_value_xx)
    object_xx <- attr_data_xx
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
                                        attributes_to_import_chr = character(0),
                                        key_var_1L_chr  = character(0),
                                        specified_rsl_chr  = character(0),
                                        what_1L_chr = "attributes",
                                        years_chr  = character(0)){
  if(what_1L_chr == "attributes"){
    boundary_rsl_chr <- stringr::str_sub(attributes_to_import_chr,5,7) %>% unique() %>% toupper() ## Ammend from naming convention to lookup
    data_names_ls <- purrr::map(boundary_rsl_chr,
                                ~ attributes_to_import_chr[stringr::str_sub(attributes_to_import_chr,5,7) == tolower(.x )]) %>%
      stats::setNames(boundary_rsl_chr)
    extra_names_chr <- purrr::map(specified_rsl_chr,
                                  ~ x@a_VicinityLookup@vicinity_processed_r3 %>%
                                    dplyr::filter(main_feature_chr == .x[1]) %>%
                                    dplyr::filter(make_year_filter_logic_vec(data_tb = .,
                                                                             included_years_vec = years_chr)) %>%
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
                                               attr_data_xx = .y,
                                               boundary_year_1L_dbl = x@a_VicinityLookup@vicinity_processed_r3 %>%
                                                 dplyr::filter(name_chr %in% .y) %>%
                                                 dplyr::pull(year_chr) %>%
                                                 min(as.numeric()))) %>%
      stats::setNames(boundary_rsl_chr)
    index_ppr <- purrr::map_lgl(data_names_ls,
                                ~ validate_popl_predns_incld(.x,
                                                             data_lookup_tb = x@a_VicinityLookup@vicinity_processed_r3,#aus_spatial_lookup_tb,
                                                             key_var_1L_chr = key_var_1L_chr)) %>%
      which() + 1
    attributes_ls <- purrr::prepend(attributes_ls,
                                    list(index_ppr=index_ppr))
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
          profiled_area_bands_ls <- make_servc_clstr_isochrs_ls(vicinity_points_ls = list(y_vicinity_points),
                                                                index_val_1L_int = 1,
                                                                time_min_1L_dbl = 0,
                                                                time_max_1L_dbl = drive_time_limit_mins(x),
                                                                time_steps_1L_dbl = x@nbr_bands_dbl)
          names(profiled_area_bands_ls) <- paste0("dt_band_",1:length(profiled_area_bands_ls))
          profiled_sf <- do.call(rbind,profiled_area_bands_ls) %>%
            sf::st_transform(x@crs_dbl[1]) %>%
            simplify_sf()
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
  return(object_xx)
}

# methods::setMethod("updateAttrDataXx",
#                    "VicinityLookup",
#                    function(x,
#                             attr_data_xx,
#                             altv_names_sf,
#                             area_names_var_chr,
#                             region_short_long_chr) {
#                      attr_data_xx
#                    })
