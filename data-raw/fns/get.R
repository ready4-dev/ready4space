get_area_sqkm_sf <- function(data_sf){
  data_sf %>%
    dplyr::mutate(FT_AREA_SQKM = sf::st_area(.) %>%
                    units::set_units(km^2)) %>%
    dplyr::summarise(TOT_AREA_SQKM = sum(FT_AREA_SQKM)) %>%
    dplyr::pull(TOT_AREA_SQKM)
}
get_closest_year <- function(data_lookup_tb,
                             incl_main_ft_vec,
                             target_year,
                             target_area = NULL,
                             find_closest = "abs"){
  if(!is.null(target_area)){
    data_lookup_tb <- data_lookup_tb %>%
      dplyr::filter(area_type == target_area)
  }
  avail_years <- purrr::map(incl_main_ft_vec,
                            ~ data_lookup_tb %>%
                              dplyr::filter(main_feature == .x) %>%
                              dplyr::pull(year) %>%
                              as.numeric())
  if(find_closest == "abs"){
    closest_year <- purrr::map(avail_years,
                               ~ .x[which(abs(.x - as.numeric(target_year)) == min(abs(.x - as.numeric(target_year))))])
  }
  if(find_closest == "previous"){
    closest_year <- purrr::map(avail_years,
                               ~ .x[which(as.numeric(target_year) - .x == min(max(as.numeric(target_year) - .x,0)))])
  }

  if(find_closest == "next"){
    closest_year <- purrr::map(avail_years,
                               ~ .x[which(.x - as.numeric(target_year) == min(max(.x - as.numeric(target_year),0)))])
  }
  return(closest_year)
}
get_common_vars_sf_ls <- function(sf_ls){
  vec_ls <- purrr::map(sf_ls, ~ names(.x))
  Reduce(intersect, vec_ls)
}
get_common_yrs_sf_ls <- function(sf_ls){
  vec_ls <- purrr::map(list_of_sfs, ~ get_included_yrs_sf(.x))
  Reduce(intersect, vec_ls)
}
get_data_year_chr <- function(data_ymdhms){
  data_ymdhms %>%
    lubridate::year() %>%
    as.character()
}
get_dir_paths_for_data_imp <- function(x,
                                       destination_directory,
                                       data_lookup_ref,
                                       lookup_variable,
                                       directory_sub_divs){
  directory_names <- purrr::map_chr(directory_sub_divs,
                                    ~ ready4fun::get_from_lup(data_lookup_tb = x,
                                                              lookup_reference = data_lookup_ref,
                                                              lookup_variable = lookup_variable,
                                                              target_variable = .x,
                                                              evaluate = FALSE))
  purrr::accumulate(directory_names,
                    ~ paste0(.x,
                             "/",
                             .y)) %>%
    paste0(destination_directory,
           "/",
           .)
}
get_group_by_var <- function(profile_unit,
                             data_unit,
                             group_at_profile_unit = TRUE,
                             group_by_lookup_tb,
                             area_bound_year){ ### REPLACE ?????
  group_by <- ifelse(group_at_profile_unit,
                     ready4fun::get_from_lup(data_lookup_tb = group_by_lookup_tb %>% dplyr::filter(spatial_unit == profile_unit) %>%
                                               dplyr::filter(as.numeric(year)==area_bound_year),
                                             lookup_variable = "spatial_unit",
                                             lookup_reference = profile_unit,
                                             target_variable = "var_name",
                                             evaluate = FALSE),
                     ready4fun::get_from_lup(data_lookup_tb = group_by_lookup_tb,
                                             lookup_variable = "spatial_unit",
                                             lookup_reference = data_unit,
                                             target_variable = "var_name",
                                             evaluate = FALSE))
  return(group_by)
}
get_group_by_var_from_pai <- function(pa_r4){
  group_by_lookup_tb = sp_uid_lup(pa_r4 %>% lookup_tb())
  if(!use_coord_lup(pa_r4)){
    group_by_var <- get_group_by_var(profile_unit = pa_r4 %>% area_type(),
                                     group_by_lookup_tb = group_by_lookup_tb,
                                     area_bound_year = area_bound_year(pa_r4))
  }else{
    if(is.na(geom_dist_limit_km(pa_r4)))
      group_by_var <- "drive_times"
    else
      group_by_var <- "distance_km"
    get_group_by_var(profile_unit = "GEOMETRIC_DISTANCE",
                     group_by_lookup_tb = group_by_lookup_tb) ## MAY NEED REPLACING
  }
  return(group_by_var)
}
get_highest_res <- function(options_vec,
                            year,
                            resolution_lup_r3){
  if(!is.na(options_vec[1])){
    res_hierarchy <- get_resolution_hierarchy(data_year = as.numeric(year),
                                              resolution_lup_r3 = resolution_lup_r3)
    res_hierarchy[min(which(res_hierarchy %in% options_vec))]
  }else
    NA
}
get_import_chr_vec <- function(lookup_tbs_r4,
                               data_type_chr){
  if(data_type_chr == "Geometry"){
    sp_import_lup(lookup_tbs_r4) %>%
      dplyr::filter(main_feature == "Boundary") %>% dplyr::pull(name)
  }else{
    sp_import_lup(lookup_tbs_r4) %>%
      dplyr::filter(data_type == "Attribute") %>% dplyr::pull(name)
  }
}
get_included_yrs_sf <- function(sf){
  sf %>%
    sf::`st_geometry<-`(NULL) %>%
    dplyr::select(dplyr::starts_with("y2")) %>%
    names() %>%
    stringr::str_sub(start = 2, end = 5) %>%
    unique() %>%
    as.numeric()
}
get_max_or_min_yr_of_sf <- function(sf,
                                    max = T){
  year_vec <- get_included_yrs_sf(sf)
  if(max)
     max(year_vec)
  else
    min(year_vec)
}
get_menu_detail_for_imp <- function(x){
  x %>%
    dplyr::select(c(1:8,12))
}
get_menu_names_for_imp <- function(x){
  # get_menu_detail_for_imp(x = x) %>%
  x %>%
    dplyr::select(name) %>%
    dplyr::pull()
}
get_menu_of_type_detail_for_imp <- function(x,
                                            lookup_ref){
  #get_menu_detail_for_imp(x = x) %>%
  x %>%
    dplyr::filter(data_type==lookup_ref)
}
get_menu_of_type_nms_for_imp <- function(x,
                                         lookup_ref){
  get_menu_of_type_detail_for_imp(x = x,
                                       lookup_ref = lookup_ref) %>%
    dplyr::select(name) %>%
    dplyr::pull()
}
get_merge_sf_str <- function(lookup_r4,
                             sp_import_r3_slice,
                             processed_dir = NULL){
  if(is.null(sp_import_r3_slice %>% dplyr::pull(add_boundaries) %>% purrr::pluck(1))){
    NA_character_
  }else{
    if(is.na(sp_import_r3_slice %>% dplyr::pull(add_boundaries) %>% purrr::pluck(1)) %>% any()){
      NA_character_
    }else{
      purrr::map_chr(sp_import_r3_slice %>% pull(add_boundaries) %>% purrr::pluck(1),
                     ~ ready4fun::get_from_lup(data_lookup_tb = sp_import_lup(lookup_r4),
                                               lookup_reference = .x,
                                               lookup_variable = "uid",
                                               target_variable = "name",
                                               evaluate = FALSE) %>%
                       ready4fun::get_from_lup(data_lookup_tb = sp_data_pack_lup(lookup_r4),
                                               lookup_reference = .,
                                               lookup_variable = "name",
                                               target_variable = "source_reference",
                                               evaluate = FALSE) %>%
                       ifelse(stringr::str_detect(.,"::"),.,paste0("readRDS(\"",processed_dir,"/",.,".rds\")")))
    }
  }
}
get_model_end_ymdhs <- function(input_ls){
  input_ls$model_start_ymdhms +
    lubridate::years(input_ls$simulation_steps_ymwd[1]) * input_ls$nbr_steps_start_to_end +
    months(input_ls$simulation_steps_ymwd[2]) * input_ls$nbr_steps_start_to_end +
    lubridate::weeks(input_ls$simulation_steps_ymwd[3]) * input_ls$nbr_steps_start_to_end +
    lubridate::days(input_ls$simulation_steps_ymwd[4]) * input_ls$nbr_steps_start_to_end +
    lubridate::hours(input_ls$simulation_steps_ymwd[5]) * input_ls$nbr_steps_start_to_end +
    lubridate::minutes(input_ls$simulation_steps_ymwd[6]) * input_ls$nbr_steps_start_to_end +
    lubridate::seconds(input_ls$simulation_steps_ymwd[7]) * input_ls$nbr_steps_start_to_end
}
get_name_from_path_chr <- function(path_str,
                                   with_ext = TRUE){
  if(with_ext){
    stringr::str_sub(path_str,
                     start = stringi::stri_locate_last_regex(path_str, "/")[,2] %>%
                       as.vector() +1)
  }else{
    stringr::str_sub(path_str,
                     start = stringi::stri_locate_last_regex(path_str, "/")[,2] %>%
                       as.vector() +1,
                     end = stringi::stri_locate_last_regex(path_str, "\\.")[,2] %>%
                       as.vector() -1)
  }
}
get_non_shape_items_for_imp <- function(path_str,
                                        x){
  file_name <-  get_name_from_path_chr(path_str)
  file_ext <- file_name %>% stringr::str_sub(start = stringi::stri_locate_last_regex(file_name, "\\.")[,2] %>%
                                               as.vector())
  data_type <- ready4fun::get_from_lup(data_lookup_tb = x,
                                       lookup_reference = file_name,
                                       lookup_variable = "inc_file_main_chr",
                                       target_variable = "data_type",
                                       evaluate = FALSE)
  var_name_vec <- c("area_type",
                    # "area_bound_yr",
                    "main_feature",
                    "year",
                    "region")
  var_val_vec <- purrr::map_chr(var_name_vec,
                                ~ ready4fun::get_from_lup(data_lookup_tb = get_menu_of_type_detail_for_imp(data_type,
                                                                                                                x = x),
                                                          lookup_reference = file_name,
                                                          lookup_variable = "inc_file_main_chr",
                                                          target_variable = .x,
                                                          evaluate = FALSE))
  make_import_object(x,
                     var_val_vec = var_val_vec,
                     path_str = path_str)
}
get_popl_var_prefix <- function(age_sex_pop_resolution,
                                tot_pop_resolution = NULL,
                                data_year){
  if(!is.null(tot_pop_resolution)){
    nse_names_ls <- make_nse_objs_ls(sp_unit = tot_pop_resolution,
                                             concept = "tot_pop",
                                             tot_pop_col = paste0("year_",
                                                                  data_year,
                                                                  "pr"),
                                             grouping_1 = age_sex_pop_resolution,
                                             data_year = data_year)
  }else{
    nse_names_ls <- make_nse_objs_ls(sp_unit = age_sex_pop_resolution,

                                             concept = "age_sex",
                                             grouping_1 = age_sex_pop_resolution,
                                             data_year = data_year)
  }
  paste0(nse_names_ls$popl_inc_unit,"_")
}
get_r_import_path_chr <- function(r_data_dir_chr,
                                  name_chr,
                                  data_type_chr){
  if(data_type_chr=="Geometry")
    name_chr <- paste0(name_chr,"_sf")

  paste0(r_data_dir_chr,"/",name_chr,".rds")

}
get_resolution_hierarchy <- function(data_year,
                                     resolution_lup_r3,
                                     whole_area = TRUE){
  resolution_hierarchy <- resolution_lup_r3  %>%
    dplyr::filter(boundary_year == data_year)
  if(whole_area){
    resolution_hierarchy <- resolution_hierarchy %>%
      dplyr::filter(complete==TRUE)
  }
  resolution_hierarchy %>%
    dplyr::arrange(dplyr::desc(area_count)) %>%
    dplyr::pull(area_type)
}
get_res_specific_vars <- function(var_names, # THIS NEEDS TO BE MADE A CONTEXT SPECIFIC METHOD WITH THIS FUNCTION MOVED TO AusSPR4c
                                  data_type,
                                  data_year,
                                  popl_var_prefix){
  if(data_type == "age_sex"){
    res_sp_vars <- var_names[var_names %>% startsWith("AREASQKM") |
                               var_names %>%
                               startsWith(paste0("y",
                                                 data_year,
                                                 ".Females.")) |
                               var_names %>%
                               startsWith(paste0("y",
                                                 data_year,
                                                 ".Males.")) |
                               var_names %>%
                               startsWith(paste0("y",
                                                 data_year,
                                                 ".total")) |
                               var_names %>%
                               startsWith("seifa.percentile")]
  }
  if(data_type == "tot_pop"){
    res_sp_vars <-  var_names[var_names %>%
        startsWith("year_")]

  }
  if(data_type == "processed_age_sex"){
    res_sp_vars <-  var_names[var_names %>%
                                startsWith("pop_sp_unit_area") |
                                var_names %>%
                                startsWith(popl_var_prefix)]
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
                                  lookup_reference,
                                  data_directory) {
  path_element_vector <- purrr::map_chr(downloaded_data_tb %>% dplyr::select(-name) %>% names(),
                                        ~ ready4fun::get_from_lup(data_lookup_tb = downloaded_data_tb,
                                                                  lookup_variable = "name",
                                                                  lookup_reference = lookup_reference,
                                                                  target_variable = .x,
                                                                  evaluate = FALSE))
  paste0(data_directory,
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
                            ~ lookup_tb(input_ls$pa_r4) %>%
                              sp_data_pack_lup() %>%
                              dplyr::filter(main_feature == .x[1]) %>%
                              dplyr::filter(make_year_filter_logic_vec(data_tb = .,
                                                                       included_years_vec = year_vec)) %>%
                              ready4fun::get_from_lup(lookup_reference = .x[1],
                                                      lookup_variable = "main_feature",
                                                      target_variable = "name",
                                                      evaluate = FALSE)) %>%
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
  data_sf_list <- purrr::map2(boundary_res,
                              data_names_list,
                              ~ add_attr_recrly_to_sf(input_ls = input_ls,
                                                      sub_div_unit = sub_div_unit,
                                                      area_unit = .x,
                                                      boundary_year = lookup_tb(input_ls$pa_r4) %>%
                                                        sp_data_pack_lup() %>%
                                                        dplyr::filter(name %in% .y) %>%
                                                        dplyr::pull(year) %>%
                                                        min(as.numeric()),
                                                      attribute_data = .y)) %>%
    stats::setNames(boundary_res)
  index_ppr <- purrr::map_lgl(data_names_list,
                              ~ check_if_ppr(.x,
                                             data_lookup_tb = lookup_tb(input_ls$pa_r4) %>%
                                               sp_data_pack_lup(),#aus_spatial_lookup_tb,
                                             pop_projs_str = input_ls$pop_projs_str)) %>%
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
  data_year <- data_year(input_ls$pa_r4)
  at_specified_res <- input_ls$at_specified_res
  country <- country(input_ls$pa_r4)
  #sub_div_unit = NULL
  pop_projs_str <- input_ls$pop_projs_str

  lookup_tb_r4 <- input_ls$pa_r4 %>% lookup_tb()
  spatial_lookup_tb <- sp_data_pack_lup(lookup_tb_r4)
  abbreviations_lookup_tb <- sp_abbreviations_lup(lookup_tb_r4)
  # if(excl_diff_bound_yr){
  #   spatial_lookup_tb <- spatial_lookup_tb %>%
  #     dplyr::filter(is.na(additional_detail) | additional_detail != " for 2016 boundaries")
  # }else
  #   spatial_lookup_tb <- spatial_lookup_tb
  year_vec <- make_year_vec(input_ls = input_ls)
  lookup_tb_list <- purrr::map(at_highest_res,
                               ~ spatial_lookup_tb %>%
                                 dplyr::filter(main_feature == .x) %>%
                                 dplyr::filter(year %in% year_vec[if(.x==pop_projs_str) 1:length(year_vec) else 1]))
  data_res_vec <- purrr::map_chr(lookup_tb_list,
                                 ~ .x %>%
                                   dplyr::pull(area_type) %>%
                                   unique() %>%
                                   get_highest_res(year = data_year,
                                                   resolution_lup_r3 = sp_resolution_lup(lookup_tb_r4)))
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
                                             dplyr::filter(area_type == .y))
  # if(!is.null(sub_div_unit)){
  #   region_lookup <- purrr::map_chr(sub_div_unit,
  #                                   ~ ready4fun::get_from_lup(data_lookup_tb = abbreviations_lookup_tb,
  #                                                          lookup_reference = .,
  #                                                          lookup_variable = "long_name",
  #                                                          target_variable = "short_name",
  #                                                          evaluate = FALSE))
  #   matched_yr_lookup_tb_list <- purrr::map2(matched_yr_lookup_tb_list,
  #                                            region_lookup,
  #                                            ~  .x %>% dplyr::filter(region %in% .y))
  # }
  names_of_data_vec <- purrr::map(matched_yr_lookup_tb_list,
                                  ~ .x %>%
                                    dplyr::pull(name)) %>%
    purrr::flatten_chr()
  if(!identical(non_matched_year_vec,character(0))){
    closest_years <- get_closest_year(data_lookup_tb = spatial_lookup_tb,
                                      incl_main_ft_vec = non_matched_year_vec,
                                      target_year = data_year)
    extra_names <- purrr::map2_chr(non_matched_year_vec,
                                   closest_years,
                                   ~     ready4fun::get_from_lup(data_lookup_tb = spatial_lookup_tb %>%
                                                                   dplyr::filter(year == .y)
                                                                 # %>%
                                                                 #   dplyr::filter(region == region_lookup)
                                                                 ,
                                                                 lookup_reference = .x,
                                                                 lookup_variable = "main_feature",
                                                                 target_variable = "name",
                                                                 evaluate = FALSE))
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

get_starter_sf_for_profiled_area <- function(pa_r4,
                                             group_by_var){
  sp_data_starter_sf_lup <- pa_r4 %>%
    lookup_tb() %>%
    sp_starter_sf_lup() %>%
    dplyr::filter(country == country(pa_r4))
  if(!is.na(area_bound_year(pa_r4)))
    sp_data_starter_sf_lup <- sp_data_starter_sf_lup %>%
      dplyr::filter(area_bound_yr == area_bound_year(pa_r4))
  starter_sf_nm <- ready4fun::get_from_lup(data_lookup_tb = sp_data_starter_sf_lup,
                                           lookup_variable = "area_type",
                                           lookup_reference = ifelse(area_type(pa_r4) %in% sp_data_starter_sf_lup$area_type,
                                                                     area_type(pa_r4),
                                                                     region_type(pa_r4)#"STE"#"PNT"
                                           ),
                                           target_variable = "starter_sf",
                                           evaluate = FALSE)
  # starter_sf <-  ready4fun::get_from_lup(data_lookup_tb = pa_r4 %>%
  #                         lookup_tb() %>%
  #                         sp_data_pack_lup(),
  #                       lookup_variable = "name",
  #                       lookup_reference = starter_sf_nm %>% stringr::str_sub(end=-4),
  #                       target_variable = "source_reference",
  #                       evaluate = FALSE) %>%
  # parse(file="",n=NULL,text = .) %>%
  # eval()
  starter_sf <- get_data(pa_r4 %>%
                           lookup_tb() %>%
                           sp_data_pack_lup(),
                         col_chr = "name",
                         value_chr = starter_sf_nm %>% stringr::str_sub(end=-4))
  if(use_coord_lup(pa_r4)){
    starter_sf <- starter_sf %>%
      sf::`st_crs<-`(crs_nbr(pa_r4)[1])
  }else{
    starter_sf <-  starter_sf %>%
      dplyr::filter(!!rlang::sym(group_by_var) %in% features(pa_r4))
  }
  return(starter_sf)
}
get_sys_data_tbs_ls <- function(){
  list(aus_spatial_lookup_tb = aus_spatial_lookup_tb,
       aus_data_resolution_tb = aus_data_resolution_tb,
       aus_state_short_tb = aus_state_short_tb,
       group_by_var_lookup_tb = group_by_var_lookup_tb)
}

