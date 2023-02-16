get_included_yrs <- function(geometry_sf,
                             pfx_1L_chr = "y2"){
  geometry_sf <- geometry_sf %>%
    sf::`st_geometry<-`(NULL)
  if(pfx_1L_chr!="")
    geometry_sf <- geometry_sf %>%
    dplyr::select(dplyr::starts_with(pfx_1L_chr))

  years_dbl <- geometry_sf %>%
    names() %>%
    stringr::str_sub(start = 2, end = 5) %>%
    unique() %>%
    as.numeric()

  return(years_dbl)
}
get_max_or_min_yr_of_sf <- function(geometry_sf,
                                    max_1L_lgl = T,
                                    pfx_1L_chr = "y2"){
  years_dbl <- get_included_yrs(geometry_sf,
                               pfx_1L_chr = pfx_1L_chr)
  if(max_1L_lgl)
    year_1L_dbl <- max(years_dbl)
  else
    year_1L_dbl <- min(years_dbl)
  return(year_1L_dbl)
}
get_name_from_path_chr <- function(path_1L_chr,
                                   with_ext_1L_lgl = TRUE){
  if(with_ext_1L_lgl){
    name_1L_chr <- stringr::str_sub(path_1L_chr,
                     start = stringi::stri_locate_last_regex(path_1L_chr, "/")[,2] %>%
                       as.vector() +1)
  }else{
    name_1L_chr <-stringr::str_sub(path_1L_chr,
                     start = stringi::stri_locate_last_regex(path_1L_chr, "/")[,2] %>%
                       as.vector() +1,
                     end = stringi::stri_locate_last_regex(path_1L_chr, "\\.")[,2] %>%
                       as.vector() -1)
  }
  return(name_1L_chr)
}

make_featured_var_pfx <- function(dynamic_var_rsl_1L_chr,
                                tot_pop_resolution = NULL,
                                data_year_1L_dbl){
  if(!is.null(tot_pop_resolution)){
    nse_names_ls <- make_nse_objs_ls(spatial_unit_1L_chr = tot_pop_resolution,
                                             concept = "tot_pop",
                                             tot_pop_col = paste0("year_",
                                                                  data_year_1L_dbl,
                                                                  "pr"),
                                             grouping_1 = dynamic_var_rsl_1L_chr,
                                             data_year_1L_dbl = data_year_1L_dbl)
  }else{
    nse_names_ls <- make_nse_objs_ls(spatial_unit_1L_chr = dynamic_var_rsl_1L_chr,

                                             concept = "age_sex",
                                             grouping_1 = dynamic_var_rsl_1L_chr,
                                             data_year_1L_dbl = data_year_1L_dbl)
  }
  paste0(nse_names_ls$popl_inc_unit,"_")
}
get_r_import_path_chr <- function(processed_fls_dir_1L_chr,
                                  name_chr,
                                  data_type_chr){
  if(data_type_chr=="Geometry")
    name_chr <- paste0(name_chr,"_sf")

  paste0(processed_fls_dir_1L_chr,"/",name_chr,".RDS")

}

get_res_specific_vars <- function(var_names, # THIS NEEDS TO BE MADE A CONTEXT SPECIFIC METHOD WITH THIS FUNCTION MOVED TO AusSPR4c
                                  data_type_chr,
                                  data_year_1L_dbl,
                                  featured_var_pfx_1L_chr){
  if(data_type_chr == "age_sex"){
    res_sp_vars <- var_names[var_names %>% startsWith("AREASQKM") |
                               var_names %>%
                               startsWith(paste0("y",
                                                 data_year_1L_dbl,
                                                 ".Females.")) |
                               var_names %>%
                               startsWith(paste0("y",
                                                 data_year_1L_dbl,
                                                 ".Males.")) |
                               var_names %>%
                               startsWith(paste0("y",
                                                 data_year_1L_dbl,
                                                 ".total")) |
                               var_names %>%
                               startsWith("seifa.percentile")]
  }
  if(data_type_chr == "tot_pop"){
    res_sp_vars <-  var_names[var_names %>%
        startsWith("year_")]

  }
  if(data_type_chr == "processed_age_sex"){
    res_sp_vars <-  var_names[var_names %>%
                                startsWith("popl_spatial_unit_area_dbl") |
                                var_names %>%
                                startsWith(featured_var_pfx_1L_chr)]
  }
  return(res_sp_vars)
}
get_set_diff_lon_lat_sf <- function(profile_sf,
                                    cut_sf,
                                    crs_nbr_dbl,
                                    validate_lgl = T,
                                    min_poly_area_dbl = units::set_units(0.05,km^2)){
  new_sf <- sf::st_difference(profile_sf %>% sf::st_transform(crs = crs_nbr_dbl[2]),
                              sf::st_union(cut_sf) %>% sf::st_transform(crs = crs_nbr_dbl[2])) %>%
    sf::st_transform(crs = crs_nbr_dbl[1])
  if(validate_lgl)
    new_sf <-  new_sf %>% make_valid_new_sf()
  new_sf <-  new_sf %>%
    dplyr::mutate(feature_idx_int = 1:dplyr::n())
  new_ls <- purrr::map(dplyr::pull(new_sf,
                                   feature_idx_int),
                       ~ new_sf %>%
                         dplyr::filter(feature_idx_int == .x)  %>%
                         sf::st_cast("POLYGON") %>%
                         dplyr::mutate(new_area = sf::st_area(.)) %>%
                         dplyr::filter(new_area > units::set_units(0.05,km^2)) %>%
                         sf::st_cast() %>%
                         dplyr::select(-new_area,-feature_idx_int)
  )
  if(length(new_ls)>1){
    purrr::map_dfr(new_ls,~.x)
  }else{
    new_ls[[1]]
  }
}
get_sngl_path_for_imp <- function(downloaded_data_tb,
                                  match_value_xx,
                                  raw_fls_dir_1L_chr) {
  path_element_vector <- purrr::map_chr(downloaded_data_tb %>% dplyr::select(-name) %>% names(),
                                        ~ ready4::get_from_lup_obj(data_lookup_tb = downloaded_data_tb,
                                                                  match_var_nm_1L_chr = "name_chr",
                                                                  match_value_xx = match_value_xx,
                                                                  target_var_nm_1L_chr = .x,
                                                                  evaluate_1L_lgl = FALSE))
  paste0(raw_fls_dir_1L_chr,
         "/",
         paste(path_element_vector,collapse = "/"))
}
get_spatial_data_list <- function(input_ls,
                                  sub_div_unit = NULL,
                                  require_year_match = TRUE,
                                  excl_diff_bound_yr = TRUE){
  attributes_to_import <- get_spatial_data_names(input_ls = input_ls,
                                                 sub_div_unit = sub_div_unit,
                                                 require_year_match = require_year_match,
                                                 excl_diff_bound_yr = excl_diff_bound_yr)
  boundary_res <- stringr::str_sub(attributes_to_import,5,7) %>% unique() %>% toupper() ## Ammend from naming convention to lookup
  data_names_list <- purrr::map(boundary_res,
                                ~ attributes_to_import[stringr::str_sub(attributes_to_import,5,7) == tolower(.x )]) %>%
    stats::setNames(boundary_res)
  year_vec <- make_year_vec(input_ls = input_ls)
  extra_names <- purrr::map(input_ls$at_specified_res,
                            ~ lookup_tb(X) %>%
                              sp_data_pack_lup() %>%
                              dplyr::filter(main_feature_chr == .x[1]) %>%
                              dplyr::filter(make_year_filter_logic_vec(data_tb = .,
                                                                       included_years_vec = year_vec)) %>%
                              ready4::get_from_lup_obj(match_value_xx = .x[1],
                                                      match_var_nm_1L_chr = "main_feature_chr",
                                                      target_var_nm_1L_chr = "name_chr",
                                                      evaluate_1L_lgl = FALSE)) %>%
    stats::setNames(purrr::map_chr(input_ls$at_specified_res, ~.x[2]))
  res_to_merge <- names(extra_names)[names(extra_names) %in% boundary_res]
  if(!identical(res_to_merge,character(0))){
    merged_elements_ls <-  purrr::map2(data_names_list[res_to_merge],
                                       extra_names[res_to_merge],
                                       ~ c(.x,.y))
    if(length(merged_elements_ls) == length(data_names_list)){
      data_names_list <- merged_elements_ls
    }else{
      data_names_list <- append(data_names_list[names(data_names_list)[!names(data_names_list) %in% res_to_merge]],
                                merged_elements_ls)

    }
  }
  extra_res <- names(extra_names)[!names(extra_names) %in% boundary_res]
  if(!identical(extra_res,character(0))){
    data_names_list <- append(data_names_list,extra_names[extra_res])
    boundary_res <- c(boundary_res, extra_res)
  }
  ##
  X <- input_ls$x_VicinityProfile
  data_sf_list <- purrr::map2(boundary_res,
                              data_names_list,
                              ~ manufacture(X@a_VicinityLookup,#input_ls = input_ls, # add_attr_recrly_to_sf
                                            #sub_div_unit = sub_div_unit,
                                            area_unit_1L_chr = .x,
                                            attr_data_xx = .y,
                                            boundary_year_1L_dbl = X@a_VicinityLookup@vicinity_processed_r3 %>%
                                                        dplyr::filter(name_chr %in% .y) %>%
                                                        dplyr::pull(year_chr) %>%
                                                        min(as.numeric()))) %>%
    stats::setNames(boundary_res)
  index_ppr <- purrr::map_lgl(data_names_list,
                              ~ validate_popl_predns_incld(.x,
                                             data_lookup_tb = lookup_tb(X) %>%
                                               sp_data_pack_lup(),#aus_spatial_lookup_tb,
                                             popl_predns_var_1L_chr = input_ls$popl_predns_var_1L_chr)) %>%
    which() + 1
  data_sf_list <- purrr::prepend(data_sf_list,
                                 list(index_ppr=index_ppr))
  return(data_sf_list)
}
get_spatial_data_names <- function(input_ls,
                                   sub_div_unit = NULL,
                                   require_year_match = TRUE,
                                   excl_diff_bound_yr = TRUE){
  #### NEED TO WORK ON SECOND HALF
  at_highest_res <- input_ls$at_highest_res
  X <- input_ls$x_VicinityProfile
  data_year_chr <- X@data_year_chr
  at_specified_res <- input_ls$at_specified_res
  country_chr <- X@country_chr
  #sub_div_unit = NULL
  popl_predns_var_1L_chr <- input_ls$popl_predns_var_1L_chr

  spatial_lookup_tb <- X@a_VicinityLookup@vicinity_processed_r3 #sp_data_pack_lup(Y)
  abbreviations_lookup_tb <- X@a_VicinityLookup@vicinity_abbreviations_r3 # sp_abbreviations_lup(lookup_tb_r4)
  # if(excl_diff_bound_yr){
  #   spatial_lookup_tb <- spatial_lookup_tb %>%
  #     dplyr::filter(is.na(additional_detail_chr) | additional_detail_chr != " for 2016 boundaries")
  # }else
  #   spatial_lookup_tb <- spatial_lookup_tb
  year_vec <- make_year_vec(input_ls = input_ls)
  lookup_tb_list <- purrr::map(at_highest_res,
                               ~ spatial_lookup_tb %>%
                                 dplyr::filter(main_feature_chr == .x) %>%
                                 dplyr::filter(year_chr %in% year_vec[if(.x==popl_predns_var_1L_chr) 1:length(year_vec) else 1]))
  data_res_vec <- purrr::map_chr(lookup_tb_list,
                                 ~ .x %>%
                                   dplyr::pull(area_type_chr) %>%
                                   unique() %>%
                                   procure.vicinity_resolutions(x = X@a_VicinityLookup@vicinity_resolutions_r3,
                                                                year_1L_dbl = data_year_chr))
  data_unavail_for_year <-  is.na(data_res_vec)
  if(require_year_match & sum(data_unavail_for_year) > 0)
    stop("Data not available for specified year for all data requested")
  matched_year_vec <- at_highest_res[!data_unavail_for_year]
  matched_yr_lookup_tb_list <- lookup_tb_list[!data_unavail_for_year]
  matched_yr_data_res_vec <- data_res_vec[!data_unavail_for_year]
  non_matched_year_vec <- at_highest_res[is.na(data_res_vec)]
  matched_yr_lookup_tb_list <- purrr::map2(matched_yr_lookup_tb_list,
                                           matched_yr_data_res_vec,
                                           ~ .x %>%
                                             dplyr::filter(area_type_chr == .y))
  # if(!is.null(sub_div_unit)){
  #   region_lookup <- purrr::map_chr(sub_div_unit,
  #                                   ~ ready4::get_from_lup_obj(data_lookup_tb = abbreviations_lookup_tb,
  #                                                          match_value_xx = .,
  #                                                          match_var_nm_1L_chr = "long_name_chr",
  #                                                          target_var_nm_1L_chr = "short_name_chr",
  #                                                          evaluate_1L_lgl = FALSE))
  #   matched_yr_lookup_tb_list <- purrr::map2(matched_yr_lookup_tb_list,
  #                                            region_lookup,
  #                                            ~  .x %>% dplyr::filter(region %in% .y))
  # }
  names_of_data_vec <- purrr::map(matched_yr_lookup_tb_list,
                                  ~ .x %>%
                                    dplyr::pull(name)) %>%
    purrr::flatten_chr()
  if(!identical(non_matched_year_vec,character(0))){
    closest_yrs_ls <- make_closest_yrs_ls(data_lookup_tb = spatial_lookup_tb,
                                      inc_main_ft_vec = non_matched_year_vec,
                                      target_year = data_year_chr)
    extra_names <- purrr::map2_chr(non_matched_year_vec,
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
    non_matched_positions <- purrr::map_dbl(non_matched_year_vec,
                                            ~ which(at_highest_res==.x))
    names_of_data_vec <- purrr::reduce(1:length(non_matched_positions),
                                       .init = names_of_data_vec,
                                       ~ append(.x,
                                                extra_names[.y],
                                                after=non_matched_positions[.y]-1))
    #c(names_of_data_vec,extra_names)
  }
  # unname()
  #c(names_of_data_vec,extra_names)
  names_of_data_vec
}

get_starter_sf_for_profiled_area <- function(x_VicinityProfile,
                                             group_by_var_1L_chr){
  sp_data_starter_sf_lup <- x_VicinityProfile %>%
    lookup_tb() %>%
    sp_starter_sf_lup() %>%
    dplyr::filter(country_chr == x_VicinityProfile@country_chr)
  if(!is.na(x_VicinityProfile@area_bndy_yr_dbl))
    sp_data_starter_sf_lup <- sp_data_starter_sf_lup %>%
      dplyr::filter(area_bndy_yr_chr == x_VicinityProfile@area_bndy_yr_chr)
  starter_sf_nm <- ready4::get_from_lup_obj(data_lookup_tb = sp_data_starter_sf_lup,
                                           match_var_nm_1L_chr = "area_type_chr",
                                           match_value_xx = ifelse(x_VicinityProfile@area_type_chr %in% sp_data_starter_sf_lup$area_type_chr,
                                                                     x_VicinityProfile@area_type_chr,
                                                                     x_VicinityProfile@region_type_chr#region_type(x_VicinityProfile)#"STE"#"PNT"
                                           ),
                                           target_var_nm_1L_chr = "starter_sf",
                                           evaluate_1L_lgl = FALSE)
  # starter_sf <-  ready4::get_from_lup_obj(data_lookup_tb = x_VicinityProfile %>%
  #                         lookup_tb() %>%
  #                         sp_data_pack_lup(),
  #                       match_var_nm_1L_chr = "name",
  #                       match_value_xx = starter_sf_nm %>% stringr::str_sub(end=-4),
  #                       target_var_nm_1L_chr = "source_reference_chr",
  #                       evaluate_1L_lgl = FALSE) %>%
  # parse(file="",n=NULL,text = .) %>%
  # eval()
  starter_sf <- procure(x_VicinityProfile %>%
                           lookup_tb() %>%
                           sp_data_pack_lup(),
                         col_nm_1L_chr = "name_chr",
                         match_value_xx = starter_sf_nm %>% stringr::str_sub(end=-4))
  if(use_coord_lup(x_VicinityProfile)){
    starter_sf <- starter_sf %>%
      sf::`st_crs<-`(crs_nbr(x_VicinityProfile)[1])
  }else{
    starter_sf <-  starter_sf %>%
      dplyr::filter(!!rlang::sym(group_by_var_1L_chr) %in% features(x_VicinityProfile))
  }
  return(starter_sf)
}

# get_sys_data_tbs_ls <- function(){
#   list(aus_spatial_lookup_tb = aus_spatial_lookup_tb,
#        aus_data_resolution_tb = aus_data_resolution_tb,
#        aus_state_short_tb = aus_state_short_tb,
#        group_by_var_1L_chr_lookup_tb = group_by_var_1L_chr_lookup_tb)
# }
# get_menu_detail_for_imp <- function(x){
#   x %>%
#     dplyr::select(c(1:8,12))
# }
# get_menu_names_for_imp <- function(x){
#   # get_menu_detail_for_imp(x = x) %>%
#   x %>%
#     dplyr::select(name) %>%
#     dplyr::pull()
# }

# get_group_by_var <- function(geometry_rsl_1L_chr, ## procure
#                              data_rsl_1L_chr,
#                              group_at_geom_unit_1L_lgl = TRUE,
#                              x_vicinity_identifiers, # "x_vicinity_identifiers ???
#                              area_bndy_yr_dbl){ ### REPLACE ?????
#   group_by_1L_chr <- ifelse(group_at_geom_unit_1L_lgl,
#                             ready4::get_from_lup_obj(data_lookup_tb = x_vicinity_identifiers %>% dplyr::filter(spatial_unit_chr == geometry_rsl_1L_chr) %>%
#                                                        dplyr::filter(as.numeric(year_chr)==area_bndy_yr_dbl),
#                                                      match_var_nm_1L_chr = "spatial_unit_chr",
#                                                      match_value_xx = geometry_rsl_1L_chr,
#                                                      target_var_nm_1L_chr = "var_name_chr",
#                                                      evaluate_1L_lgl = FALSE),
#                             ready4::get_from_lup_obj(data_lookup_tb = x_vicinity_identifiers,
#                                                      match_var_nm_1L_chr = "spatial_unit_chr",
#                                                      match_value_xx = data_rsl_1L_chr,
#                                                      target_var_nm_1L_chr = "var_name_chr",
#                                                      evaluate_1L_lgl = FALSE))
#   return(group_by_1L_chr)
# }
# get_group_by_var_from_VicinityProfile <- function(x_VicinityProfile){ Now procire mthd
#   y_vicinity_identifiers = x_VicinityProfile@a_VicinityLookup@vicinity_identifiers_r3
#   if(!x_VicinityProfile@use_coord_lup_lgl){
#     group_by_var_1L_chr <- procure.vicinity_identifiers(y_vicinity_identifiers,#get_group_by_var
#                                                         geometry_rsl_1L_chr = x_VicinityProfile@area_type_chr,#get_group_by_var
#                                                         area_bndy_yr_chr = as.character(x_VicinityProfile@area_bndy_yr_dbl))
#   }else{
#     if(is.na(x_VicinityProfile@geomc_dist_limit_km_dbl))
#       group_by_var_1L_chr <- "drive_times"
#     else
#       group_by_var_1L_chr <- "distance_km"
#     # procure.vicinity_identifiers(y_vicinity_identifiers,#get_group_by_var
#     #                              geometry_rsl_1L_chr = "GEOMETRIC_DISTANCE",
#     #                              area_bndy_yr_chr = as.character(x_VicinityProfile@area_bndy_yr_dbl) ## Addition - Not sure if correct.
#     #                              ) ## MAY NEED REPLACING
#   }
#   return(group_by_var_1L_chr)
# }

# get_highest_res <- function(options_vec, Now procure mthd
#                             year_1L_dbl,
#                             resolution_lup_r3){
#   if(!is.na(options_vec[1])){
#     res_hierarchy <- get_resolution_hierarchy(data_year_1L_dbl = as.numeric(year_1L_dbl),
#                                               resolution_lup_r3 = resolution_lup_r3)
#     res_hierarchy[min(which(res_hierarchy %in% options_vec))]
#   }else
#     NA
# }

# get_resolution_hierarchy <- function(data_year_1L_dbl, # NOW PROCURE MTHD
#                                      resolution_lup_r3,
#                                      whole_area = TRUE){
#   resolution_hierarchy <- resolution_lup_r3  %>%
#     dplyr::filter(boundary_year_dbl == data_year_1L_dbl)
#   if(whole_area){
#     resolution_hierarchy <- resolution_hierarchy %>%
#       dplyr::filter(complete_lgl==TRUE)
#   }
#   resolution_hierarchy %>%
#     dplyr::arrange(dplyr::desc(area_count_dbl)) %>%
#     dplyr::pull(area_type_chr)
# }
# get_non_shape_items_for_imp <- function(path_1L_chr, # now ingest mthd
#                                         x){
#   file_name <-  get_name_from_path_chr(path_1L_chr)
#   file_ext <- file_name %>% stringr::str_sub(start = stringi::stri_locate_last_regex(file_name, "\\.")[,2] %>%
#                                                as.vector())
#   data_type_chr <- ready4::get_from_lup_obj(data_lookup_tb = x,
#                                        match_value_xx = file_name,
#                                        match_var_nm_1L_chr = "inc_file_main_chr",
#                                        target_var_nm_1L_chr = "data_type_chr",
#                                        evaluate_1L_lgl = FALSE)
#   var_name_vec <- c("area_type_chr",
#                     # #"area_bndy_yr_chr", ????
#                     "main_feature_chr",
#                     "year_chr",
#                     "region")
#   var_val_chr <- purrr::map_chr(var_name_vec,
#                                 ~ ready4::get_from_lup_obj(data_lookup_tb = get_menu_of_type_detail_for_imp(data_type_chr,
#                                                                                                                 x = x),
#                                                           match_value_xx = file_name,
#                                                           match_var_nm_1L_chr = "inc_file_main_chr",
#                                                           target_var_nm_1L_chr = .x,
#                                                           evaluate_1L_lgl = FALSE))
#   items_xx <- manufacture.vicinity_raw(x,#make_import_object # could pass custom fn to this method
#                            var_val_chr = var_val_chr,
#                            path_1L_chr = path_1L_chr)
#   return(items_xx)
# }
# get_menu_of_type_detail_for_imp <- function(x,#procure.vicinity_raw
#                                             match_value_xx){
#   #get_menu_detail_for_imp(x = x) %>%
#   x %>%
#     dplyr::filter(data_type_chr==match_value_xx)
# }
# get_menu_of_type_nms_for_imp <- function(x,
#                                          match_value_xx){
#   procure.vicinity_raw(x = x,#get_menu_of_type_detail_for_imp #get_menu_of_type_nms_for_imp
#                                        match_value_xx = match_value_xx,
#                        what_1L_chr = "match") %>%
#     dplyr::select(name) %>%
#     dplyr::pull()
# }
