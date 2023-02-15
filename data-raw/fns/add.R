add_attr_to_sf <- function(area_sf,
                           attr_data_tb,
                           attr_data_desc_1L_chr){
  if(attr_data_desc_1L_chr == "PPR"){ # "Population projections"
    updated_area_sf <- dplyr::inner_join(area_sf,
                                      attr_data_tb)
  }
  if(stringr::str_detect(attr_data_desc_1L_chr, "ERP_TOT")){ #"ERP by age and sex"
    updated_area_sf <- dplyr::inner_join(area_sf,
                                      attr_data_tb) %>%
      sf::st_as_sf()
  }
  if(attr_data_desc_1L_chr == "ERP_ASX"){ # "ERP"
    updated_area_sf <- dplyr::inner_join(area_sf,
                                      attr_data_tb) %>%
      sf::st_as_sf()
  }
  return(updated_area_sf)
}
add_data_pack_lup <- function(x_VicinityLookup,
                              tbl_data_type_1L_chr = "Geometry",
                              template_ls = NULL,
                              package_1L_chr){
  data_pk_lup_arguments_ls <- purrr::map2(template_ls, # remove (carefully)
                                          names(template_ls),
                                          ~ list(.x, # remove (carefully)
                                                 .y,
                                                 ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
                                                                          target_var_nm_1L_chr = "area_type_chr",
                                                                          match_var_nm_1L_chr = "name_chr",
                                                                          match_value_xx = .y,
                                                                          evaluate_1L_lgl = FALSE),
                                                 ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
                                                                          target_var_nm_1L_chr = "area_bndy_yr_chr",
                                                                          match_var_nm_1L_chr = "name_chr",
                                                                          match_value_xx = .y,
                                                                          evaluate_1L_lgl = FALSE),
                                                 ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
                                                                          target_var_nm_1L_chr = "region_chr",
                                                                          match_var_nm_1L_chr = "name_chr",
                                                                          match_value_xx = .y,
                                                                          evaluate_1L_lgl = FALSE),
                                                 ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
                                                                          target_var_nm_1L_chr = "year_chr",
                                                                          match_var_nm_1L_chr = "name_chr",
                                                                          match_value_xx = .y,
                                                                          evaluate_1L_lgl = FALSE),
                                                 ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
                                                                          target_var_nm_1L_chr = "year_start_chr",
                                                                          match_var_nm_1L_chr = "name_chr",
                                                                          match_value_xx = .y,
                                                                          evaluate_1L_lgl = FALSE),
                                                 ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
                                                                          target_var_nm_1L_chr = "year_end_chr",
                                                                          match_var_nm_1L_chr = "name_chr",
                                                                          match_value_xx = .y,
                                                                          evaluate_1L_lgl = FALSE),
                                                 ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
                                                                          target_var_nm_1L_chr = "main_feature_chr",
                                                                          match_var_nm_1L_chr = "name_chr",
                                                                          match_value_xx = .y,
                                                                          evaluate_1L_lgl = FALSE))) # Set names here to allow names based referencing in destination function.
  data_pack_lup_r3 <- purrr::reduce(data_pk_lup_arguments_ls,
                                    .init = x_VicinityLookup@vicinity_processed_r3,
                                    ~ add_attr_tb_to_processed_lup(.x,.y)) %>%
    dplyr::mutate(data_type_chr = tbl_data_type_1L_chr)
  package_1L_chr <- ifelse(package_1L_chr ==""|is.na(package_1L_chr),"", paste0(package_1L_chr,"::"))
  data_pack_lup_r3 <- data_pack_lup_r3 %>%
    dplyr::mutate(source_reference_chr = paste0(package_1L_chr,source_reference_chr))  %>%
    dplyr::mutate(source_reference_chr = purrr::map2_chr(main_feature_chr,
                                                         source_reference_chr,
                                                         ~ ifelse(.x == "Boundary",
                                                                  paste0(.y,
                                                                         "_sf"),
                                                                  .y)))
  x_VicinityLookup <- renewSlot(x_VicinityLookup,
                                "vicinity_processed_r3",#"vicinity_raw_r3",# Correct?
                                data_pack_lup_r3)
  return(x_VicinityLookup)
}
add_dynamic_vars_to_sf <- function(dynamic_vars_sf,
                                   profiled_sf,
                                   dynamic_var_rsl_1L_chr,
                                   dynamic_var_nm_1L_chr,
                                   featured_var_pfx_1L_chr,
                                   data_year_chr,
                                   crs_nbr_dbl){
  profiled_sf <- make_intersecting_profiled_area(profiled_sf = dynamic_vars_sf,
                                           profiled_sf_col_1L_chr = NA_character_,
                                           profiled_sf_row_1L_chr = NA_character_,
                                           attribute_sf = profiled_sf,
                                           attribute_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                           data_type_chr = "processed_age_sex",
                                           data_year_chr = data_year_chr,
                                           featured_var_pfx_1L_chr = featured_var_pfx_1L_chr,
                                           crs_nbr_dbl = crs_nbr_dbl) %>%
    add_km_sqd_by_group(group_by_var_1L_chr = dynamic_var_nm_1L_chr,
                           feature_nm_1L_chr = dynamic_var_rsl_1L_chr)
  dyn_param_unit_id_1L_chr <- names(dynamic_vars_sf)[1] # Should be read from lookup
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(dynamic_var_nm_1L_chr) := paste0(!!rlang::sym(dyn_param_unit_id_1L_chr),"_",!!rlang::sym(dynamic_var_nm_1L_chr)))
  update_pop_count_by_areas(profiled_sf = profiled_sf,
                            group_by_var_1L_chr = group_by_var_1L_chr,
                            dynamic_var_nm_1L_chr = dynamic_var_nm_1L_chr,
                            data_year_chr = data_year_chr,
                            dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                            tot_pop_resolution = NULL,
                            featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)

}
add_km_sqd <- function(geometry_sf,
                       feature_nm_1L_chr,
                       prefix_1L_chr = "whl_",
                       suffix_1L_chr = "_area"){
  geometry_sf <- geometry_sf %>%
    dplyr::mutate(!!rlang::sym(paste0(prefix_1L_chr,
                                      feature_nm_1L_chr,
                                      suffix_1L_chr)) := sf::st_area(.) %>%
                    units::set_units(km^2))
  return(geometry_sf)
}
add_km_sqd_by_group <- function(geometry_sf,
                                group_by_var_1L_chr,
                                feature_nm_1L_chr,
                                prefix_1L_chr = "whl_",
                                suffix_1L_chr = "_area"){
  geometry_sf <- merge(geometry_sf,
                       geometry_sf %>%
                         dplyr::group_by(!!rlang::sym(group_by_var_1L_chr)) %>%
                         dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
                         add_km_sqd(feature_nm_1L_chr = feature_nm_1L_chr,
                                    prefix_1L_chr = prefix_1L_chr,
                                    suffix_1L_chr = suffix_1L_chr) %>%
                         dplyr::ungroup() %>%
                         sf::st_set_geometry(NULL))
  return(geometry_sf)

}
add_names <- function(ds_tb){
  data(ISO_3166_1, package = "ISOcodes", envir = environment())
  ds_tb <- ds_tb %>%
    dplyr::mutate(name_chr = purrr::pmap_chr(list(country_chr,
                                              area_type_chr,
                                              region_chr,
                                              data_type_chr,
                                              main_feature_chr,
                                              year_chr),
                                         ~ paste0(ready4::get_from_lup_obj(data_lookup_tb = ISO_3166_1,
                                                                           match_value_xx = ..1,
                                                                           match_var_nm_1L_chr = "Name",
                                                                           target_var_nm_1L_chr = "Alpha_3",
                                                                           evaluate_1L_lgl = FALSE) %>% tolower(),
                                                  "_",
                                                  tolower(..2),
                                                  "_",
                                                  tolower(..3 %>% stringr::str_sub(end=3)),
                                                  ifelse(..4 == "Geometry",
                                                         ifelse(..5 == "Boundary","_bnd_","_crd_"),
                                                         paste0("_",tolower(..5),"_")),
                                                  ..6
                                         )))
  ds_tb <- ds_tb %>% dplyr::mutate(name_chr = make.unique(name_chr)) %>% dplyr::mutate(name_chr = map_chr(name_chr, ~ ifelse(stringr::str_sub(.x,start = -2, end = -2) == ".",
                                                                                                             paste0(stringr::str_sub(.x, end = 11),
                                                                                                                    stringr::str_sub(.x,start = -1),
                                                                                                                    stringr::str_sub(.x, start = 12, end = -3)),
                                                                                                             .x)))
  return(ds_tb)
}
add_resolutions_lup <- function(x_VicinityLookup,
                                processed_fls_dir_1L_chr){
  dr_dp_tb <- x_VicinityLookup@vicinity_processed_r3 %>%
    dplyr::filter(main_feature_chr == "Boundary") %>%
    dplyr::select(area_type_chr,country_chr,region_chr,source_reference_chr,year_chr) %>%
    dplyr::mutate(source_reference_chr = paste0(processed_fls_dir_1L_chr,
                                                "/",
                                                source_reference_chr,
                                                ".RDS"))
  dr_dp_vec <- dr_dp_tb  %>%
    dplyr::pull(source_reference_chr)
  dr_nt_vec <- dr_dp_tb  %>%
    dplyr::pull(region_chr)
  if(any(dr_nt_vec=="National")){
    nat_sf <- readRDS(dr_dp_vec[stringr::str_which(dr_nt_vec,"National") %>% min()])
    nat_area <- nat_sf %>% make_km_sqd_dbl()
  }else{
    nat_area <- NA_real_
  }
  resolution_lup_r3 <- purrr::pmap_dfr(dr_dp_tb,
                                       ~ tibble::tibble(parent_area_chr= ..2,
                                                        boundary_year_dbl = as.numeric(..5),
                                                        area_type_chr = ..1,
                                                        area_count_dbl = nrow(readRDS(..4)) %>% as.double(),
                                                        complete_lgl = T,
                                                        summed_area_dbl = ifelse(..3=="National",
                                                                                 nat_area,
                                                                                 readRDS(..4) %>% make_km_sqd_dbl()),
                                                        mean_size_dbl =  summed_area_dbl / area_count_dbl))
  resolution_lup_r3 <- resolution_lup_r3 %>%
    vicinity_resolutions() %>%
    dplyr::arrange(mean_size_dbl)
  x_VicinityLookup <- renewSlot(x_VicinityLookup,
                                "vicinity_resolutions_r3",
                                resolution_lup_r3)
  return(x_VicinityLookup)
}
add_templates <- function(x_VicinityLookup,
                          path_to_seed_sf_1L_chr){
  starter_sf_nm_1L_chr <- get_name_from_path_chr(path_to_seed_sf_1L_chr, with_ext_1L_lgl = F)
  starter_sf_lup_r3 <- tibble::add_row(x_VicinityLookup@vicinity_templates_r3 ,
                                       country_chr = x_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(country_chr),
                                       area_type_chr = x_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(area_type_chr),
                                       area_bndy_yr_chr = x_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(area_bndy_yr_chr),
                                       starter_sf = starter_sf_nm_1L_chr,
                                       subdivision_chr = x_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(uid_chr)) ## Assumes length one list
  x_VicinityLookup <- renewSlot(x_VicinityLookup,
                                "vicinity_templates_r3",
                                starter_sf_lup_r3)
  return(x_VicinityLookup)
}
add_uid_lup <- function(x_VicinityLookup#lookup_tbs_r4
                        ){
  uid_lup_r3 <- tibble::add_row(vicinity_identifiers(),
                                spatial_unit_chr = x_VicinityLookup@vicinity_raw_r3 %>% # sp_import_lup(x_VicinityLookup) %>%
                                  dplyr::pull(area_type_chr),
                                year_chr =   x_VicinityLookup@vicinity_raw_r3 %>% # sp_import_lup(x_VicinityLookup) %>%
                                  dplyr::pull(area_bndy_yr_chr), ## "All".
                                var_name_chr =  x_VicinityLookup@vicinity_raw_r3 %>% # sp_import_lup(x_VicinityLookup) %>%
                                  dplyr::pull(uid_chr))
  x_VicinityLookup <- renewSlot(x_VicinityLookup,"vicinity_identifiers_r3", uid_lup_r3) #`sp_uid_lup<-`(x_VicinityLookup, uid_lup_r3)
  return(x_VicinityLookup)
}


##### In progress

##### STAGED

# add_attribute_to_data_pack_from_tb <- function(attr_tb,
#                                                object_name_1L_chr){
#   eval(parse(text = paste0(object_name_1L_chr,
#                            "<<-attr_tb")))
#   eval(parse(text = paste0("usethis::use_data(",object_name_1L_chr,
#                            ", overwrite = TRUE)")))
# }
# add_attribute_to_data_pack <- function(combined_ste_ppr_ls,
#                                        object_name_stub){
#   add_attr_to_global(combined_ste_ppr_ls = combined_ste_ppr_ls,
#                      object_name_stub = object_name_stub)
#   purrr::walk(names(combined_ste_ppr_ls),
#               ~ eval(parse(text = paste0("usethis::use_data(",object_name_stub,.x %>% stringr::str_sub(start = 2),
#                                          ", overwrite = TRUE)"))))
# }
# add_attr_to_global <- function(combined_ste_ppr_ls,
#                                object_name_stub){
#   purrr::walk(names(combined_ste_ppr_ls),
#               ~ eval(parse(text = paste0(object_name_stub,.x %>% stringr::str_sub(start = 2),
#                                          "<<-combined_ste_ppr_ls$",.x))))
# }
# add_popl_predn_ls <- function(x,y){ ## update with names and check calling function(s).
#   add_popl_predn(data_pack_lup = x,
#                            combined_ste_ppr_ls = y[[1]],
#                            object_name_stub = y[[2]],
#                            area_type_chr = y[[3]],
#                            area_bndy_yr_chr = y[[4]],
#                            region_chr = y[[5]])
# }
# add_popl_predn <- function(data_pack_lup,
#                                      combined_ste_ppr_ls,
#                                      object_name_stub,
#                                      area_type_chr,
#                                      area_bndy_yr_chr,
#                                      region_chr){
#   tibble::tibble(name = paste0(object_name_stub,names(combined_ste_ppr_ls) %>% stringr::str_sub(start = 2)),
#                  country_chr = "Australia", # Change to context based method (lookup table)
#                  area_type_chr = area_type_chr,
#                  area_bndy_yr_chr = area_bndy_yr_chr,
#                  region_chr = region_chr,
#                  data_type_chr = "Attribute",
#                  main_feature_chr = "Population projections",
#                  year_chr = names(combined_ste_ppr_ls) %>% stringr::str_sub(start = 2),
#                  source_reference_chr = paste0(object_name_stub,names(combined_ste_ppr_ls) %>% stringr::str_sub(start = 2))) %>%
#     dplyr::bind_rows(data_pack_lup,
#                      .)
# }
# add_attrs_to_processed_lup <- function(data_pack_lup, # NOW renew method
#                                        #attr_tb, # remove (carefully)
#                                        object_name_1L_chr,
#                                        area_type_chr,
#                                        area_bndy_yr_chr,
#                                        region_chr,
#                                        year_chr,
#                                        year_start_chr,
#                                        year_end_chr,
#                                        main_feature_chr){ # replace with names based referencing
#   tibble::tibble(name_chr = object_name_1L_chr, #name
#                  country_chr = "Australia", # Pull this from context data.
#                  area_type_chr = area_type_chr,
#                  area_bndy_yr_chr = area_bndy_yr_chr,
#                  region_chr = region_chr,
#                  data_type_chr = "Attribute",
#                  main_feature_chr = main_feature_chr,
#                  year_chr = year_chr,
#                  year_start_chr = year_start_chr,
#                  year_end_chr = year_end_chr,
#                  source_reference_chr = object_name_1L_chr) %>%
#     dplyr::bind_rows(data_pack_lup,
#                      .)
# }
# add_attr_list_to_sf <- function(area_sf,#x, ## NOW manufacture
#                                 match_1L_chr,#y,
#                                 x_VicinityLookup#lookup_tb_r4
# ){
#   attr_data_xx <- make_attr_data_xx(x_VicinityLookup = x_VicinityLookup,
#                                     match_value_xx = match_1L_chr,
#                                     starter_sf = area_sf)
#   updated_area_sf <- add_attr_to_sf(area_sf = area_sf,
#                                     attr_data_tb = attr_data_xx,
#                                     attr_data_desc = ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_processed_r3,
#                                                                               match_value_xx = match_1L_chr,
#                                                                               match_var_nm_1L_chr = "name_chr",
#                                                                               target_var_nm_1L_chr = "main_feature_chr"))
#   return(updated_area_sf)
# }
# add_attr_recrly_to_sf <- function(input_ls, # Now Manufacture
#                                   sub_div_unit = NULL,
#                                   area_unit_1L_chr,
#                                   boundary_year_1L_chr,
#                                   attribute_data_chr){
#   x_VicinityLookup <- input_ls$x_VicinityProfile@a_VicinityLookup
#   data_lookup_tb <- x_VicinityLookup@vicinity_processed_r3
#   boundary_sf <- ingest(data_lookup_tb %>%
#                           dplyr::filter(area_type_chr == area_unit_1L_chr) %>%
#                           dplyr::filter(main_feature_chr == "Boundary") %>%
#                           dplyr::filter(as.numeric(year_start_chr) == max(as.numeric(year_start_chr)[as.numeric(year_start_chr) <= as.numeric(boundary_year_1L_chr)])),
#                         match_value_xx = "Boundary")
#   attribute_data_ls <- purrr::map(attribute_data_chr,
#                                   ~ .x) %>%
#     stats::setNames(attribute_data_chr)
#   sf_ls <- purrr::map(attribute_data_ls,
#                       ~ manufacture(x_VicinityLookup,
#                                     area_sf = boundary_sf, #add_attr_list_to_sf
#                                     match_1L_chr = .x)) %>%
#     transform_sf_ls() %>%
#     purrr::reduce(~rbind(.x,.y))
#   return(sf_ls)
# }
