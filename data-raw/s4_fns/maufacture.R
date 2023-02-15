manufacture_VicinityArguments <- function(x,
                                          ...){
return(NULL)
}
manufacture_VicinityLookup <- function(x,
                                       area_sf = NULL,
                                       area_unit_1L_chr,
                                       attr_data_xx = NULL,
                                       #attribute_data_chr,
                                       boundary_year_1L_chr,
                                       match_1L_chr = character(0),
                                       #sub_div_unit = NULL,
                                       ...
                                       # alt_names_sf = NULL,
                                       # area_names_var_chr = NULL,
                                       # region_short_long_chr = NULL
                                       ){
  if(!identical(match_1L_chr, character(0))){ # add_attr_list_to_sf
    attr_data_xx <- make_attr_data_xx(x_VicinityLookup = x,
                                      match_value_xx = match_1L_chr,
                                      starter_sf = area_sf)
    object_xx <- add_attr_to_sf(area_sf = area_sf,
                                attr_data_tb = attr_data_xx,
                                attr_data_desc = ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3,
                                                                                match_value_xx = match_1L_chr,
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
                                        match_1L_chr = .x)) %>%
        transform_sf_ls() %>%
        purrr::reduce(~rbind(.x,.y))

    }else{
      object_xx <- attr_data_xx
    }
  }
  return(attr_data_xx)
}
# methods::setMethod("updateAttrDataXx",
#                    "VicinityLookup",
#                    function(x,
#                             attr_data_xx,
#                             alt_names_sf,
#                             area_names_var_chr,
#                             region_short_long_chr) {
#                      attr_data_xx
#                    })
