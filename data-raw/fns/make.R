make_attr_data_xx <- function(x_VicinityLookup,
                              match_1L_chr,
                              starter_sf){
  data_lookup_tb <- x_VicinityLookup@vicinity_processed_r3
  attr_data_xx <- ingest(data_lookup_tb,
                          col_nm_1L_chr = "name_chr",
                          match_value_xx = match_1L_chr)
  if(is.data.frame(attr_data_xx)){
    attr_data_xx <- list(attr_data_xx) %>%
      stats::setNames(ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb,
                                               match_1L_chrerence = match_1L_chr,
                                               match_var_nm_1L_chr = "name_chr",
                                               target_var_nm_1L_chr = "year_chr",
                                               evaluate_1L_lgl = FALSE))
  }
  region_short_nm_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb,
                                                     match_value_xx = match_1L_chr,
                                                     match_var_nm_1L_chr = "name_chr",
                                                     target_var_nm_1L_chr = "region_chr",
                                                     evaluate_1L_lgl = FALSE)
  region_short_long_chr <- c(region_short_nm_1L_chr,
                             ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_abbreviations_r3,
                                                      match_value_xx = region_short_nm_1L_chr,
                                                      match_var_nm_1L_chr = "short_name_chr",
                                                      target_var_nm_1L_chr = "long_name_chr",
                                                      evaluate_1L_lgl = FALSE))
  area_names_var_chr <- ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb,
                                                 match_value_xx = match_1L_chr,
                                                 match_var_nm_1L_chr = "name_chr",
                                                 target_var_nm_1L_chr = "area_type_chr",
                                                 evaluate_1L_lgl = FALSE) %>%
    ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_templates_r3,
                             match_value_xx = .,
                             match_var_nm_1L_chr = "area_type_chr",
                             target_var_nm_1L_chr = "subdivision_chr",
                             evaluate_1L_lgl = FALSE)
  area_names_var_chr <- area_names_var_chr[area_names_var_chr %in% names(starter_sf)]
  boundary_year_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb,
                                            match_value_xx = match_1L_chr,
                                            match_var_nm_1L_chr = "name_chr",
                                            target_var_nm_1L_chr = "area_bndy_yr_chr",
                                            evaluate_1L_lgl = F)
  area_names_var_chr <- x_VicinityLookup@vicinity_identifiers_r3 %>%
    dplyr::filter(var_name_chr %in% area_names_var_chr) %>%
    dplyr::filter(as.numeric(year_chr) == max(as.numeric(year_chr)[as.numeric(year_chr) <= as.numeric(boundary_year_1L_chr)])) %>%
    dplyr::pull(var_name_chr)
  attr_data_xx <- manufacture(x_VicinityLookup,# updateAttrDataXx
                              attr_data_xx = attr_data_xx,
                              alt_names_sf = starter_sf,
                              area_names_var_chr = area_names_var_chr,
                              region_short_long_chr = region_short_long_chr,
                              match_value_xx = match_1L_chr)
  return(attr_data_xx)
}
make_closest_yrs_ls <- function(data_lookup_tb,
                                inc_main_ft_vec,
                                target_year,
                                target_area = NULL,
                                find_closest = "abs"){
  if(!is.null(target_area)){
    data_lookup_tb <- data_lookup_tb %>%
      dplyr::filter(area_type_chr == target_area)
  }
  available_yrs_ls <- purrr::map(inc_main_ft_vec,
                                 ~ data_lookup_tb %>%
                                   dplyr::filter(main_feature_chr == .x) %>%
                                   dplyr::pull(year_chr) %>%
                                   as.numeric())
  if(find_closest == "abs"){
    closest_yrs_ls <- purrr::map(available_yrs_ls,
                                 ~ .x[which(abs(.x - as.numeric(target_year)) == min(abs(.x - as.numeric(target_year))))])
  }
  if(find_closest == "previous"){
    closest_yrs_ls <- purrr::map(available_yrs_ls,
                                 ~ .x[which(as.numeric(target_year) - .x == min(max(as.numeric(target_year) - .x,0)))])
  }

  if(find_closest == "next"){
    closest_yrs_ls <- purrr::map(available_yrs_ls,
                                 ~ .x[which(.x - as.numeric(target_year) == min(max(.x - as.numeric(target_year),0)))])
  }
  return(closest_yrs_ls)
}
make_common_sf_vars_ls <- function(sf_ls){
  vec_ls <- purrr::map(sf_ls, ~ names(.x))
  common_sf_vars_ls <- Reduce(intersect, vec_ls)
  return(common_sf_vars_ls)
}
make_common_sf_yrs_ls <- function(sf_ls){
  vec_ls <- purrr::map(list_of_sfs, ~ get_included_yrs(.x))
  common_sf_yrs_ls <- Reduce(intersect, vec_ls)
  return(common_sf_yrs_ls)
}
make_data_yrs_chr <- function(data_dtm){
  data_yrs_chr <- data_dtm %>%
    lubridate::year() %>%
    as.character()
  return(data_yrs_chr)
}
make_imports_chr <- function(x_VicinityLookup,#lookup_tbs_r4,
                             data_type_1L_chr){
  if(data_type_1L_chr == "Geometry"){
    imports_chr <- x_VicinityLookup@vicinity_raw_r3 %>%
      dplyr::filter(main_feature_chr == "Boundary") %>% dplyr::pull(name_chr)
  }else{
    imports_chr <- x_VicinityLookup@vicinity_raw_r3 %>%
      dplyr::filter(data_type_1L_chr == "Attribute") %>% dplyr::pull(name_chr)
    return(imports_chr)
  }
}
make_intersecting_profiled_area <- function(profiled_sf,
                                            profiled_sf_col_1L_chr = NA,
                                            profiled_sf_row_1L_chr = NA,
                                            attribute_sf,
                                            attribute_rsl_1L_chr,
                                            data_type_chr,
                                            data_year_chr,
                                            crs_nbr_dbl,
                                            featured_var_pfx_1L_chr = NULL){
  if(!is.na(profiled_sf_col_1L_chr)){
    if(!is.na(profiled_sf_row_1L_chr)){
      profiled_sf <- profiled_sf %>%
        dplyr::select(!!profiled_sf_col_1L_chr)
      index.nbr <- profiled_sf %>%
        dplyr::pull(profiled_sf_col_1L_chr) %>%
        stringr::str_which(profiled_sf_row_1L_chr)
      profiled_sf <- profiled_sf %>%
        dplyr::slice(index.nbr)
    }else
      profiled_sf <- profiled_sf %>%
        dplyr::select(!!profiled_sf_col_1L_chr)
  }
  attribute_sf <- rename_vars_based_on_res(sf = attribute_sf,
                                           data_type_chr = data_type_chr,
                                           data_year_chr = data_year_chr,
                                           featured_var_pfx_1L_chr = featured_var_pfx_1L_chr,
                                           feature_nm_1L_chr = attribute_rsl_1L_chr)

  profiled_sf <- make_intersecting_geometries(sf_1 = profiled_sf,
                                              sf_2 = attribute_sf,
                                              crs_nbr_dbl = crs_nbr_dbl)
  return(profiled_sf)
}
make_intersecting_geometries <- function(sf_1,
                                         sf_2,
                                         crs_nbr_dbl,
                                         validate_lgl = T){
  sf_3 <- sf::st_intersection(sf_1 %>% sf::st_transform(crs_nbr_dbl[2]),
                              sf_2  %>% sf::st_transform(crs_nbr_dbl[2])) %>%
    sf::st_transform(crs_nbr_dbl[1])
  if(validate_lgl)
    sf_3 %>% make_valid_new_sf()
  else
    sf_3
}
make_km_sqd_dbl <- function(data_sf){
  data_sf %>%
    dplyr::mutate(FT_AREA_SQKM = sf::st_area(.) %>%
                    units::set_units(km^2)) %>%
    dplyr::summarise(TOT_AREA_SQKM = sum(FT_AREA_SQKM)) %>%
    dplyr::pull(TOT_AREA_SQKM)
}
make_merge_sf_chr <- function(x_VicinityLookup,
                              y_vicinity_raw,
                              processed_fls_dir_1L_chr = NULL){
  if(is.null(y_vicinity_raw %>% dplyr::pull(add_boundaries_chr) %>% purrr::pluck(1))){
    merge_sf_chr <- NA_character_
  }else{
    if(is.na(y_vicinity_raw %>% dplyr::pull(add_boundaries_chr) %>% purrr::pluck(1)) %>% any()){
      merge_sf_chr <- NA_character_
    }else{
      merge_sf_chr <- purrr::map_chr(y_vicinity_raw %>% pull(add_boundaries_chr) %>% purrr::pluck(1),
                     ~ ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
                                                match_value_xx = .x,
                                                match_var_nm_1L_chr = "uid_chr",
                                                target_var_nm_1L_chr = "name_chr",
                                                evaluate_1L_lgl = FALSE) %>%
                       ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
                                                match_value_xx = .,
                                                match_var_nm_1L_chr = "name_chr",
                                                target_var_nm_1L_chr = "source_reference_chr",
                                                evaluate_1L_lgl = FALSE) %>%
                       ifelse(stringr::str_detect(.,"::"),.,paste0("readRDS(\"",processed_fls_dir_1L_chr,"/",.,".rds\")")))
    }
  }
  return(merge_sf_chr)
}
make_paths_chr <- function(x_vicinity_raw,
                           dir_1L_chr,
                           data_match_value_xx,
                           match_var_nm_1L_chr,
                           sub_dirs_chr){
  paths_chr <- purrr::map_chr(sub_dirs_chr,
                              ~ ready4::get_from_lup_obj(data_lookup_tb = x_vicinity_raw,
                                                         match_value_xx = data_match_value_xx,
                                                         match_var_nm_1L_chr = match_var_nm_1L_chr,
                                                         target_var_nm_1L_chr = .x,
                                                         evaluate_1L_lgl = FALSE))
  paths_chr <- purrr::accumulate(paths_chr,
                                 ~ paste0(.x,"/",.y)) %>%
    paste0(dir_1L_chr,
           "/",
           .)
  return(paths_chr)
}
make_reconciled_intersecting_area <- function(profiled_sf,
                                              profiled_sf_col_1L_chr = NA,
                                              profiled_sf_row_1L_chr = NA,
                                              sp_data_list,
                                              tot_pop_resolution,
                                              dynamic_var_rsl_1L_chr,
                                              group_by_var_1L_chr,
                                              age_sex_counts_grouped_by,
                                              data_year_chr,
                                              crs_nbr_dbl){
  if(!is.null(tot_pop_resolution)){
    if(age_sex_counts_grouped_by %in% names(sp_data_list[[tot_pop_resolution]])){
      sp_data_list[[dynamic_var_rsl_1L_chr]] <- merge(sp_data_list[[tot_pop_resolution]],
                                                      sf::st_set_geometry(sp_data_list[[dynamic_var_rsl_1L_chr]],NULL),
                                                      by = age_sex_counts_grouped_by) %>%
        dplyr::distinct(.keep_all = T) %>%
        dplyr::select(-dplyr::ends_with(".x")) %>%
        dplyr::rename_at(.vars = dplyr::vars(dplyr::ends_with(".y")),
                         ~ stringi::stri_replace_last_regex(.x,"\\.y$",""))
      sp_data_list[[dynamic_var_rsl_1L_chr]] <- rename_vars_based_on_res(sf = sp_data_list[[dynamic_var_rsl_1L_chr]],
                                                                         data_type_chr = "tot_pop",
                                                                         data_year_chr = data_year_chr,
                                                                         feature_nm_1L_chr = tot_pop_resolution) %>%
        add_km_sqd(feature_nm_1L_chr = tot_pop_resolution)
    }
  }
  sp_data_list[[dynamic_var_rsl_1L_chr]] <- sp_data_list[[dynamic_var_rsl_1L_chr]] %>%
    add_km_sqd_by_group(group_by_var_1L_chr = age_sex_counts_grouped_by,
                           feature_nm_1L_chr = dynamic_var_rsl_1L_chr)
  profiled_sf <- make_intersecting_profiled_area(profiled_sf = profiled_sf,
                                                 profiled_sf_col_1L_chr = profiled_sf_col_1L_chr,
                                                 profiled_sf_row_1L_chr = profiled_sf_row_1L_chr,
                                                 attribute_sf = sp_data_list[[dynamic_var_rsl_1L_chr]],
                                                 attribute_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                                 data_type_chr = "age_sex",
                                                 data_year_chr = data_year_chr,
                                                 crs_nbr_dbl = crs_nbr_dbl)
  if(!is.null(tot_pop_resolution)){
    if(!age_sex_counts_grouped_by %in% names(sp_data_list[[tot_pop_resolution]])){
      profiled_sf <- make_intersecting_profiled_area(profiled_sf = profiled_sf,
                                                     profiled_sf_col_1L_chr = profiled_sf_col_1L_chr,
                                                     profiled_sf_row_1L_chr = profiled_sf_row_1L_chr,
                                                     attribute_sf = sp_data_list[[tot_pop_resolution]] %>%
                                                       add_km_sqd(feature_nm_1L_chr = tot_pop_resolution) ,
                                                     attribute_rsl_1L_chr = tot_pop_resolution,
                                                     data_type_chr = "tot_pop")
    }
  }
  profiled_sf <- update_pop_count_by_areas(profiled_sf = profiled_sf,
                                           group_by_var_1L_chr = group_by_var_1L_chr,
                                           dynamic_var_nm_1L_chr = age_sex_counts_grouped_by,
                                           data_year_chr = data_year_chr,
                                           dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                           tot_pop_resolution = tot_pop_resolution)

  return(profiled_sf)
}

make_sf_ls <- function(profiled_sf,
                       group_by_var_1L_chr){
  sf_ls <- purrr::map(profiled_sf %>%
               dplyr::pull(!!rlang::sym(group_by_var_1L_chr)) %>%
               unique(),
             ~ profiled_sf %>%
               dplyr::filter(!!rlang::sym(group_by_var_1L_chr) == .x)) %>%
    stats::setNames(profiled_sf %>%
                      dplyr::pull(!!rlang::sym(group_by_var_1L_chr)) %>%
                      unique())
  return(sf_ls)
}
##### In Progress



##### STAGED

make_1_clstr_1_srvc_trvl_tm<- function(cluster_tb,
                                       service,
                                       time_min,
                                       time_max,
                                       nbr_time_steps){
  one_service <- cluster_tb %>%
    dplyr::filter(service_name == service)
  one_service_sf <- make_isochrs(long = one_service %>% dplyr::select(long) %>% dplyr::pull(),
                                                             lat = one_service %>% dplyr::select(lat) %>% dplyr::pull(),
                                                             time_min = time_min,
                                                             time_max = time_max,
                                                             nbr_time_steps = nbr_time_steps)
  return(one_service_sf)
}
make_agt_coords_tb <- function(profiled_area_sf,
                               disorder,
                               year_chr,
                               case_type = "expected.incidence",
                               person_type = "p",
                               resolution_unit){
  unit_col_name <- paste0(resolution_unit,
                          "_MAIN",
                          stringr::str_sub(year_chr,3,4))
  cases_col_name <- paste0("proj_",
                           disorder,
                           "_",
                           person_type,
                           "_",
                           year_chr)
  profiled_area_df <- profiled_area_sf %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(!!rlang::sym(unit_col_name),
                  !!rlang::sym(cases_col_name))
  agent_coordinates_tb <- purrr::map2_dfr(profiled_area_df %>% dplyr::select(!!unit_col_name) %>% dplyr::pull(),
                                          profiled_area_df %>% dplyr::select(!!rlang::sym(cases_col_name)) %>% dplyr::pull(),
                                          ~ sample_agent_coords(profiled_sf = profiled_area_sf %>%
                                                                  dplyr::filter(!!rlang::sym(unit_col_name)==.x),
                                                                incident_cases = .y))
  return(agent_coordinates_tb)
}


make_geomc_dist_boundrs <- function(point_locations,
                                    land_sf,
                                    distance,
                                    crs_nbr){
  distance_from_pts_sf <- sf::st_as_sf(point_locations,
                                       coords = c("long", "lat"),
                                       crs = crs_nbr[1]) %>% #4326)
    sf::st_transform(crs_nbr[2]) ##3577
  distance_from_pts_on_land_sf <- sf::st_buffer(distance_from_pts_sf,
                                                dist = distance) %>%
    sf::st_union() %>%
    sf::st_intersection(land_sf %>%
                          sf::st_transform(crs_nbr[2])) %>% #3577
    sf::st_transform(crs_nbr[1]) %>%
    sf::st_sf()
  return(distance_from_pts_on_land_sf)
}
make_isochrs <- function(long,
                         lat,
                         time_min,
                         time_max,
                         nbr_time_steps){
  time_step <- (time_max-time_min)/nbr_time_steps
  iso <- osrm::osrmIsochrone(loc = c(long, lat),
                             breaks = seq(from = time_min,
                                          to = time_max,
                                          by = time_step))
  iso_sf <- sf::st_as_sf(iso) %>%
    dplyr::mutate(drive_times = paste0(min,
                                       " to ",
                                       max,
                                       " mins")) %>%
    dplyr::arrange(id)

  return(iso_sf)
}
make_nse_objs_ls <- function(sp_unit,
                             concept,
                             tot_pop_col = NULL,
                             grouping_1 = NULL,
                             data_year_chr,
                             featured_var_pfx_1L_chr){
  if(concept == "age_sex"){
    popl_multiplier <- paste0("inc_",sp_unit,"_prop")
    whl_pop_str_1 <- paste0("whl_",sp_unit,"_",featured_var_pfx_1L_chr,"y",data_year_chr,".Females.")
    whl_pop_str_2 <- paste0("whl_",sp_unit,"_",featured_var_pfx_1L_chr,"y",data_year_chr,".Males.")
    inc_str_to_delete <- paste0("whl_",sp_unit,"_")
    grouping_1_age_sex_pop_str <- NA_character_
  }
  if(concept == "tot_pop"){
    popl_multiplier <- "pop_prop_multiplier_tot_pop"
    grouping_1_age_sex_pop_str <- paste0("grp_by_",grouping_1,"_inc_age_sex_")
    whl_pop_str_1 <- paste0(grouping_1_age_sex_pop_str,"y",data_year_chr,".Females.")
    whl_pop_str_2 <- paste0(grouping_1_age_sex_pop_str,"y",data_year_chr,".Males.")
    inc_str_to_delete <- grouping_1_age_sex_pop_str
    grouping_1_age_sex_pop_str <- paste0("grp_by_",grouping_1,"_inc_age_sex_")
  }
  list(area_whl_unit = paste0("whl_",sp_unit,"_area"),
       area_inc_unit = paste0("inc_",sp_unit,"_area"),
       prop_inc_unit = paste0("inc_",sp_unit,"_prop"),
       popl_inc_unit = paste0("inc_",sp_unit,"_popl"),
       popl_whl_unit = paste0("whl_",sp_unit,"_",tot_pop_col),
       popl_multiplier = popl_multiplier,
       popl_whl_starts_with_1 = ifelse(is.null(whl_pop_str_1),
                                       NA_character_,
                                       whl_pop_str_1),
       popl_whl_starts_with_2 = ifelse(is.null(whl_pop_str_2),
                                       NA_character_,
                                       whl_pop_str_2),
       grouping_1_concept_tot = ifelse(is.null(grouping_1),
                                       NA_character_,
                                       paste0("grp_by_",
                                              grouping_1,
                                              "_inc_",
                                              concept)),
       grouping_1_age_sex_pop = grouping_1_age_sex_pop_str,
       inc_str_to_delete = inc_str_to_delete)
}
make_raw_format_dir_chr <- function(raw_fls_dir_1L_chr,
                                    category){
  paste0(raw_fls_dir_1L_chr,"/",category)
}
make_srvc_clstr_geomc_dist_boundrs <- function(distance_km,
                                    clusters_vec,
                                    clusters_tbs_list,
                                    land_boundary_sf,
                                    crs_nbr){
  purrr::map(1:length(clusters_vec),
             ~ make_geomc_dist_boundrs(point_locations = clusters_tbs_list %>%
                                         purrr::pluck(.x),
                                       land_sf = land_boundary_sf,
                                       distance = distance_km *1000,
                                       crs_nbr = crs_nbr)) %>%
    stats::setNames(., clusters_tbs_list %>% names())

}
make_servc_clstr_isochrs_ls <- function(cluster_tbs_list,
                                        look_up_ref,
                                        time_min = 0,
                                        time_max = 60,
                                        nbr_time_steps = 5){
  ##
  require(osrm)
  one_cluster_services_vec <- cluster_tbs_list %>%
    purrr::pluck(look_up_ref) %>%
    dplyr::select(service_name) %>%
    dplyr::pull()
  ##
  cluster_tb = cluster_tbs_list %>%
    purrr::pluck(look_up_ref)
  ##
  one_cluster_travel_time_sf_list <- purrr::map(one_cluster_services_vec,
                                                ~ make_isochrs_for_1_srvc(cluster_tb = cluster_tb,
                                                                                      service = .x,
                                                                                      time_min = time_min,
                                                                                      time_max = time_max,
                                                                                      nbr_time_steps = nbr_time_steps)) %>%
    stats::setNames(., one_cluster_services_vec)
  detach("package:osrm", unload=TRUE)
  ##
  one_cluster_time_bands_list <- purrr::map(1:length(one_cluster_travel_time_sf_list),
                                            ~ make_time_band_sf_ls(look_up_ref = .x,
                                                                   one_cluster_travel_time_sf_list = one_cluster_travel_time_sf_list)) %>%
    stats::setNames(one_cluster_travel_time_sf_list %>% names())
  ##
  one_cluster_unioned_time_bands_list <- purrr::map(1:(one_cluster_time_bands_list %>%
                                                         purrr::pluck(1) %>%
                                                         length()),
                                                    ~ union_one_travel_time_band_across_sites(time_band_ref = .x,
                                                                                              one_cluster_time_bands_list =  one_cluster_time_bands_list)) %>%
    stats::setNames(paste0("tb_"
                           ,1:(one_cluster_time_bands_list %>%
                                 purrr::pluck(1) %>%
                                 length())))
  ##
  one_cluster_up_to_xmin_list <- purrr::accumulate(one_cluster_unioned_time_bands_list,
                                                   ~ sf::st_union(.x,.y))  %>%
    stats::setNames(paste0("tb_"
                           ,1:(one_cluster_unioned_time_bands_list %>%
                                 length())))
  ##
  one_cluster_up_to_xmin_list <- purrr::map(1:length(one_cluster_up_to_xmin_list),
                                            ~ update_sf_boundary_descr(look_up_ref = .x,
                                                                  one_cluster_up_to_xmin_list = one_cluster_up_to_xmin_list)) %>%
    stats::setNames(paste0("tb_"
                           ,1:(one_cluster_up_to_xmin_list  %>%
                                 length())))
  ##
  one_cluster_joint_travel_time_list <- purrr::map(1:(length(one_cluster_unioned_time_bands_list)-1),
                                                   ~ sf::st_difference(one_cluster_unioned_time_bands_list %>% purrr::pluck(.x+1),
                                                                       one_cluster_up_to_xmin_list %>% purrr::pluck(.x)) %>%
                                                     dplyr::select(id,min,max,center,drive_times)) %>%
    stats::setNames(paste0("tb_"
                           ,2:(one_cluster_up_to_xmin_list  %>%
                                 length())))  %>%
    purrr::prepend(list(tb_1 = one_cluster_unioned_time_bands_list %>% purrr::pluck(1)))
  return(one_cluster_joint_travel_time_list)
}

make_distance_based_bands <- function(distance_km_outer,
                                      nbr_distance_bands,
                                      service_cluster_tb,
                                      profiled_sf,
                                      crs_nbr){
  distances_vec <- seq(from = distance_km_outer/nbr_distance_bands,
                       to = distance_km_outer,
                       by = distance_km_outer/nbr_distance_bands)

  service_clusters_vec <- service_cluster_tb %>% dplyr::pull(cluster_name_chr) %>% unique()
  service_clusters_tbs_list <- purrr::map(service_clusters_vec,
                                          ~ service_cluster_tb %>%
                                            dplyr::filter(cluster_name_chr == .x)) %>%
    stats::setNames(service_clusters_vec)
  service_clusters_by_distance_list <- purrr::map(distances_vec,
                                                  ~ make_srvc_clstr_geomc_dist_boundrs(distance_km = .x,
                                                                                       clusters_vec = service_clusters_vec,
                                                                                       clusters_tbs_list = service_clusters_tbs_list,
                                                                                       land_boundary_sf = profiled_sf,
                                                                                       crs_nbr = crs_nbr)) %>%
    stats::setNames(., paste0("km_",
                              distances_vec,
                              "from_service"))
  geometric_distance_by_cluster_circles <- purrr::map(1:length(service_clusters_vec),
                                                      ~ reorder_distance_list_by_cluster(look_up_ref = .x,
                                                                                         clusters_by_distance_list = service_clusters_by_distance_list,
                                                                                         distances_vec = distances_vec)) %>%
    stats::setNames(., service_clusters_tbs_list %>% names())
  geometric_distance_by_cluster_bands <- purrr::map(geometric_distance_by_cluster_circles,
                                                    ~ transform_circles_to_bands(geom_distance_circle_sfs_list = .x)) %>%
    stats::setNames(., service_clusters_tbs_list %>% names())
  geometric_distance_by_cluster_circles_merged_list <- purrr::map(geometric_distance_by_cluster_circles,
                                                                  ~ do.call(rbind,.x)) %>%
    stats::setNames(., service_clusters_tbs_list %>% names()) %>%
    purrr::map(.,
               ~ .x %>% dplyr::arrange(desc(distance_km)))
  geometric_distance_by_cluster_bands_merged_list <- purrr::map(geometric_distance_by_cluster_bands,
                                                                ~ do.call(rbind,.x)) %>%
    stats::setNames(., service_clusters_tbs_list %>% names()) %>%
    purrr::map(.,
               ~ .x %>% dplyr::arrange(desc(distance_km)) %>%
                 simplify_sf(crs = crs_nbr[1]))
  return(geometric_distance_by_cluster_bands_merged_list)
}
make_each_uid_a_poly_sf <- function(sf,
                                    uid_chr){
  sf <- sf %>% dplyr::filter(sf::st_is_valid(sf))
  duplicate_chr_vec <- sf %>% dplyr::filter(!!rlang::sym(uid_chr) %>%
                                              duplicated()) %>%
    dplyr::pull(!!rlang::sym(uid_chr)) %>%
    unique()
  sf_1 <- sf %>% dplyr::filter(!(!!rlang::sym(uid_chr) %in%
                                   duplicate_chr_vec))
  sf_2 <- sf %>% dplyr::filter(!!rlang::sym(uid_chr) %in%
                                 duplicate_chr_vec)
  purrr::map(duplicate_chr_vec,
             ~ sf::st_sf(sf_2 %>%
                           dplyr::filter(!!rlang::sym(uid_chr) == .x) %>%
                           sf::st_set_geometry(NULL) %>%
                           dplyr::summarise_all(.funs = dplyr::first),
                         geometry = sf_2 %>%
                           dplyr::filter(!!rlang::sym(uid_chr) == .x) %>%
                           sf::st_union() %>%
                           sf::st_sfc())) %>%
    append(list(sf_1))  %>%
    purrr::reduce(~rbind(.x,.y))
}
make_env_param_tb <- function(n_its_int,
                              env_str_param_tb,
                              mape_str_param_tb,
                              joint_dstr_1L_lgl){
  param_val_mape <- reckon(x = mape_str_param_tb,
                                             n_its_int = n_its_int,
                                             joint_dstr_1L_lgl = joint_dstr_1L_lgl)
  param_val_env <- reckon(x = env_str_param_tb,
                                            n_its_int = n_its_int)
  dplyr::bind_rows(param_val_env,
                   param_val_mape)
}
make_sp_data_list <- function(input_ls,
                              sub_div_units_vec){
  lists_to_merge <- purrr::map(sub_div_units_vec,
                               ~ get_spatial_data_list(input_ls = input_ls,
                                                       sub_div_unit = .x,
                                                       require_year_match = FALSE,
                                                       excl_diff_bound_yr = TRUE))
  lists_to_merge <- purrr::transpose(lists_to_merge)
  merged_list <- purrr::map(lists_to_merge[2:length(lists_to_merge)],
                            ~ do.call(rbind,.x))
  names_ppr <- purrr::map_chr(lists_to_merge[[1]],
                              ~ ifelse(length(.x[1])==0,
                                       NA_character_,
                                       names(.x[1])))
  ppr_ref <- purrr::map_dbl(lists_to_merge[[1]],
                            ~ ifelse(length(.x[1])==0,
                                     NA_real_,
                                     .x[1])) %>%
    stats::setNames(names_ppr)
  sp_data_list <- purrr::prepend(merged_list,list(ppr_ref = ppr_ref))
  return(sp_data_list)
}
make_profiled_area_objs <- function(x_VicinityProfile){
  group_by_var_1L_chr <- procure(x_VicinityProfile,#get_group_by_var_from_VicinityProfile
                                 what_1L_chr = "grouping")
  st_profiled_sf <- get_starter_sf_for_profiled_area(x_VicinityProfile = x_VicinityProfile,
                                                     group_by_var_1L_chr = group_by_var_1L_chr)
  main_sub_div_var <- ifelse(use_coord_lup(x_VicinityProfile),
                             x_VicinityProfile@lookup_tb@sp_uid_lup %>%
                               ready4::get_from_lup_obj(match_var_nm_1L_chr = "spatial_unit_chr",
                                                       match_value_xx = x_VicinityProfile@region_type,
                                                       target_var_nm_1L_chr = "var_name_chr",
                                                       evaluate_1L_lgl = F),
                             ready4::get_from_lup_obj(data_lookup_tb = x_VicinityProfile %>%
                                                       lookup_tb() %>%
                                                       sp_starter_sf_lup() %>%
                                                       dplyr::filter(country_chr == x_VicinityProfile@country_chr) %>%
                                                       dplyr::filter(area_bndy_yr_dbl == x_VicinityProfile@area_bndy_yr_dbl),
                                                     match_var_nm_1L_chr = "area_type_chr",
                                                     match_value_xx = x_VicinityProfile@area_type_chr,
                                                     target_var_nm_1L_chr = "subdivision_chr",
                                                     evaluate_1L_lgl = FALSE))
  if(!use_coord_lup(x_VicinityProfile)){
    profiled_sf <- st_profiled_sf
    profiled_area_bands_list <- make_sf_ls(profiled_sf = profiled_sf,
                                                     group_by_var_1L_chr = group_by_var_1L_chr)
    sub_div_units_vec <- profiled_sf %>%
      dplyr::pull(!!rlang::sym(main_sub_div_var)) %>%
      as.character() %>%
      unique()
  }else{
    cluster_tb = lookup_tb(x_VicinityProfile) %>%
      sp_site_coord_lup() %>%
      dplyr::filter(service_name %in% features(x_VicinityProfile))
    if(!is.na(geom_dist_limit_km(x_VicinityProfile))){
      profiled_sf <- make_distance_based_bands(distance_km_outer = geom_dist_limit_km(x_VicinityProfile), # *1000
                                              nbr_distance_bands = nbr_bands(x_VicinityProfile),
                                              service_cluster_tb = cluster_tb,
                                              profiled_sf =  st_profiled_sf,
                                              crs_nbr = crs_nbr(x_VicinityProfile))[[1]]
      profiled_area_bands_list <- make_sf_ls(profiled_sf = profiled_sf,
                                                       group_by_var_1L_chr = group_by_var_1L_chr)
    }
    if(!is.na(drive_time_limit_mins(x_VicinityProfile))){
      profiled_area_bands_list <- make_servc_clstr_isochrs_ls(cluster_tbs_list = list(cluster_tb),
                                                     look_up_ref = 1,
                                                     time_min = 0,
                                                     time_max = drive_time_limit_mins(x_VicinityProfile),
                                                     nbr_time_steps = nbr_bands(x_VicinityProfile))
      names(profiled_area_bands_list) <- paste0("dt_band_",1:length(profiled_area_bands_list))
      profiled_sf <- do.call(rbind,profiled_area_bands_list) %>%
        sf::st_transform(crs_nbr(x_VicinityProfile)[1]) %>%
        simplify_sf()
    }
    sub_div_units_vec <- make_intersecting_geometries(sf_1 = st_profiled_sf,
                                               sf_2 = profiled_sf,
                                               crs_nbr_dbl = crs_nbr(x_VicinityProfile)) %>%
      dplyr::pull(!!rlang::sym(main_sub_div_var)) %>%
      as.vector()%>%
      unique()
  }
  return(list(sub_div_units_vec = sub_div_units_vec,
              profiled_sf = profiled_sf,
              profiled_area_bands_list = profiled_area_bands_list))
}
make_time_band_sf_ls <- function(look_up_ref,
                                 one_cluster_travel_time_sf_list){
  travel_time_bands <- one_cluster_travel_time_sf_list %>%
    purrr::pluck(look_up_ref) %>% dplyr::pull(drive_times)
  time_band_sf_ls <- purrr::map(travel_time_bands,
                                ~ one_cluster_travel_time_sf_list %>%
                                  purrr::pluck(look_up_ref) %>%
                                  dplyr::filter(drive_times == .x)) %>%
    stats::setNames(paste0("tb_"
                           ,stringr::str_replace_all(travel_time_bands,
                                                     " ",
                                                     "_")))
  return(time_band_sf_ls)
}
# https://stackoverflow.com/questions/40489162/draw-time-radius-around-lat-long-on-map
make_trvl_tm_isochrs <- function(appID,
                                 apiKey,
                                 origin,
                                 mode_of_transport = "driving",
                                 travel_time_hours,
                                 crs){
  location <- origin
  travel_time_secs <- travel_time_hours * 60 * 60
  url <- "http://api.traveltimeapp.com/v4/time-map"
  requestBody <- paste0('{
                        "departure_searches" : [
                        {"id" : "test",
                        "coords": {"lat":', origin[1], ', "lng":', origin[2],' },
                        "transportation" : {"type" : "', mode_of_transport,'" } ,
                        "travel_time" : ', travel_time_secs, ',
                        "departure_time" : "2017-05-03T08:00:00z"
                        }
                        ]
}')
  res <- httr::POST(url = url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    httr::add_headers('Accept' = 'application/json'),
                    httr::add_headers('X-Application-Id' = appId),
                    httr::add_headers('X-Api-Key' = apiKey),
                    body = requestBody,
                    encode = "json")

  res <- jsonlite::fromJSON(as.character(res))
  pl <- lapply(res$results$shapes[[1]]$shell, function(x){
    googleway::encode_pl(lat = x[['lat']], lon = x[['lng']])
  })
  df <- data.frame(polyline = unlist(pl))
  #df_marker <- data.frame(lat = location[1], lon = location[2])
  polyline_vec <- df %>% dplyr::pull(polyline)
  list_of_sfs <- purrr::map(polyline_vec,
                            ~ transform_tt_polygon_to_sf(tt_polyline = .x,
                                                         mode_of_transport = mode_of_transport,
                                                         travel_time_hours = travel_time_hours,
                                                         crs = crs))

  new_sf <- purrr::reduce(list_of_sfs,
                          rbind)
  return(new_sf)
}
make_valid_new_sf <- function(sf){
  valid_sf <- sf %>%
    dplyr::filter(sf::st_is_valid(.))
  if(nrow(valid_sf)!=nrow(sf)){
    fixed_sf <- sf %>%
      dplyr::filter(!sf::st_is_valid(.)) %>%
      sf::st_make_valid()
    valid_sf <- rbind(valid_sf, fixed_sf)
  }
  gc_sf <- valid_sf %>%
    dplyr::filter(sf::st_geometry_type(.)=="GEOMETRYCOLLECTION")
  if(nrow(gc_sf)>0){
    valid_sf <- gc_sf %>%
      sf::st_collection_extract(type = c("POLYGON")) %>%
      rbind(valid_sf %>%
              dplyr::filter(sf::st_geometry_type(.)!="GEOMETRYCOLLECTION"))
  }
  valid_sf %>%
    dplyr::filter(sf::st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    sf::st_cast("MULTIPOLYGON") %>%
    dplyr::distinct(.keep_all = T)
}
make_year_filter_logic_vec <- function(data_tb,
                                       included_years_vec){
  purrr::map2_lgl(data_tb$year_chr, data_tb$year_start_chr, ~ (.x %in% included_years_vec | .y %in% included_years_vec))
}
make_year_vec <- function(input_ls){
  data_year_chr <- input_ls$x_VicinityProfile@data_year_chr
  x_VicinityLookup <- input_ls$x_VicinityProfile@a_VicinityLookup
  spatial_lookup_tb <- x_VicinityLookup@vicinity_processed_r3
  popl_predns_var_1L_chr <- input_ls$popl_predns_var_1L_chr
  model_end_year <- get_model_end_ymdhs(input_ls = input_ls) %>% lubridate::year()
  year_opts <- spatial_lookup_tb %>%
    dplyr::filter(main_feature_chr == popl_predns_var_1L_chr) %>%
    dplyr::pull(year_end_chr)
  year_opts <- year_opts[stringr::str_length(year_opts)==4]
  year_opts_ref <- which((year_opts %>%
                            as.numeric() %>%
                            sort()) >= model_end_year) %>% min()
  model_end_year <- year_opts %>%
    as.numeric() %>%
    sort() %>% purrr::pluck(year_opts_ref) %>%
    as.character()
  as.character(as.numeric(data_year_chr):as.numeric(model_end_year))
}
# make_sf_rows_fn <- function(...){
#   attribution_1L_chr <- "Based on: https://github.com/r-spatial/sf/issues/49"
#   sf_list <- rlang::dots_values(...)[[1]]
#   sfg_list_column <- lapply(sf_list, function(sf) sf$geometry[[1]]) %>% sf::st_sfc()
#   df <- lapply(sf_list, function(sf) sf::st_set_geometry(sf, NULL)) %>% dplyr::bind_rows()
#   sf_appended_fn <- sf::st_sf(data.frame(df, geom=sfg_list_column))
#   return(sf_appended_fn)
# }
