procure_VicinityProfile <- function(x, #get_group_by_var_from_VicinityProfile
                                    exclude_dif_bndy_yr_1L_lgl = TRUE,
                                    highest_rsl_chr = character(0),
                                    key_var_1L_chr = character(0),
                                    #subdivision_1L_chr = NULL,
                                    match_year_1L_lgl = TRUE,
                                    travel_mode_1L_chr = character(0), # "car"
                                    years_chr = character(0),
                                    what_1L_chr = "grouping"){
  if(what_1L_chr == "attribute_names"){ # From get_spatial_attr_names  - Probably needs work
    #### NEED TO WORK ON SECOND HALF
    # x <- input_ls$x_VicinityProfile
    #subdivision_1L_chr = NULL
    spatial_lookup_tb <- x@a_VicinityLookup@vicinity_processed_r3 #sp_data_pack_lup(Y)
    # if(exclude_dif_bndy_yr_1L_lgl){
    #   spatial_lookup_tb <- spatial_lookup_tb %>%
    #     dplyr::filter(is.na(additional_detail_chr) | additional_detail_chr != " for 2016 boundaries")
    # }else
    #   spatial_lookup_tb <- spatial_lookup_tb
    lookup_tbl_ls <- purrr::map(highest_rsl_chr,
                                ~ spatial_lookup_tb %>%
                                  dplyr::filter(main_feature_chr == .x) %>%
                                  dplyr::filter(year_chr %in% years_chr[if(.x==key_var_1L_chr) 1:length(years_chr) else 1]))
    data_rsl_chr <- purrr::map_chr(lookup_tbl_ls,
                                   ~ .x %>%
                                     dplyr::pull(area_type_chr) %>%
                                     unique() %>%
                                     procure.vicinity_resolutions(x = x@a_VicinityLookup@vicinity_resolutions_r3,
                                                                  year_1L_dbl = as.numeric(x@data_year_1L_chr)))
    data_unavail_for_year <-  is.na(data_rsl_chr)
    if(match_year_1L_lgl & sum(data_unavail_for_year) > 0)
      stop("Data not available for specified year for all data requested")
    matched_years_chr <- highest_rsl_chr[!data_unavail_for_year]
    matched_yr_lookup_tbl_ls <- lookup_tbl_ls[!data_unavail_for_year]
    matched_yr_data_rsl_chr <- data_rsl_chr[!data_unavail_for_year]
    non_matched_years_chr <- highest_rsl_chr[is.na(data_rsl_chr)]
    matched_yr_lookup_tbl_ls <- purrr::map2(matched_yr_lookup_tbl_ls,
                                            matched_yr_data_rsl_chr,
                                            ~ .x %>%
                                              dplyr::filter(area_type_chr == .y))
    # if(!is.null(subdivision_1L_chr)){
    #   region_lookup <- purrr::map_chr(subdivision_1L_chr,
    #                                   ~ ready4::get_from_lup_obj(data_lookup_tb = x@a_VicinityLookup@vicinity_abbreviations_r3,
    #                                                          match_value_xx = .,
    #                                                          match_var_nm_1L_chr = "long_name_chr",
    #                                                          target_var_nm_1L_chr = "short_name_chr",
    #                                                          evaluate_1L_lgl = FALSE))
    #   matched_yr_lookup_tbl_ls <- purrr::map2(matched_yr_lookup_tbl_ls,
    #                                            region_lookup,
    #                                            ~  .x %>% dplyr::filter(region %in% .y))
    # }
    attribute_names_chr <- purrr::map(matched_yr_lookup_tbl_ls,
                                      ~ .x %>%
                                        dplyr::pull(name)) %>%
      purrr::flatten_chr()
    if(!identical(non_matched_years_chr,character(0))){
      closest_yrs_ls <- manufacture.vicinity_processed(spatial_lookup_tb,#make_closest_yrs_ls
                                            main_incld_feature_chr = non_matched_years_chr,#inc_main_ft_vec
                                            target_year_1L_chr = x@data_year_1L_chr,
                                            what_1L_chr = "closest year")
      extra_names <- purrr::map2_chr(non_matched_years_chr,
                                     closest_yrs_ls,
                                     ~     ready4::get_from_lup_obj(data_lookup_tb = spatial_lookup_tb %>%
                                                                      dplyr::filter(year_chr == .y)
                                                                    # %>%
                                                                    #   dplyr::filter(region == region_lookup)
                                                                    ,
                                                                    match_value_xx = .x,
                                                                    match_var_nm_1L_chr = "main_feature_chr",
                                                                    target_var_nm_1L_chr = "name_chr",
                                                                    evaluate_1L_lgl = FALSE))
      non_matched_positions <- purrr::map_dbl(non_matched_years_chr,
                                              ~ which(highest_rsl_chr==.x))
      attribute_names_chr <- purrr::reduce(1:length(non_matched_positions),
                                           .init = attribute_names_chr,
                                           ~ append(.x,
                                                    extra_names[.y],
                                                    after=non_matched_positions[.y]-1))
      #c(attribute_names_chr,extra_names)
    }
    # unname()
    #c(attribute_names_chr,extra_names)
    object_xx <- attribute_names_chr
  }
  if(what_1L_chr == "grouping"){
    y_vicinity_identifiers = x@a_VicinityLookup@vicinity_identifiers_r3
    if(!x@use_coord_lup_lgl){
      object_xx <- procure.vicinity_identifiers(y_vicinity_identifiers,#get_group_by_var
                                                          geometry_rsl_1L_chr = x@area_type_chr,#get_group_by_var
                                                          area_bndy_yr_chr = as.character(x@area_bndy_yr_dbl))
    }
    if(what_1L_chr == "proximity"){
      if(is.na(x@geomc_dist_limit_km_dbl))
        object_xx <- paste0(travel_mode_1L_chr,"_times")#"drive_times"
      else
        object_xx <- "distance_in_km_dbl"
      # procure.vicinity_identifiers(y_vicinity_identifiers,#get_group_by_var
      #                              geometry_rsl_1L_chr = "GEOMETRIC_DISTANCE",
      #                              area_bndy_yr_chr = as.character(x@area_bndy_yr_dbl) ## Addition - Not sure if correct.
      #                              ) ## MAY NEED REPLACING
    }
  }


  return(object_xx)
}
