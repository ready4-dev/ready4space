make_agent_locations_tb <- function(profiled_area_sf,
                                    crs_dbl,
                                    person_ctg_1L_chr = character(0),
                                    person_type_1L_chr = "p",
                                    resolution_unit_1L_chr,
                                    year_1L_chr
                                    #case_type_1L_chr = "expected.incidence",
                                    ){
  unit_col_name_1L_chr <- paste0(resolution_unit_1L_chr,
                                 "_MAIN",
                                 stringr::str_sub(year_1L_chr,3,4))
  cases_col_name_1L_chr <- paste0("proj_",
                                  person_ctg_1L_chr,
                                  ifelse(identical(person_ctg_1L_chr, character(0)),"","_"),
                                  person_type_1L_chr,
                                  "_",
                                  year_1L_chr)
  profiled_area_df <- profiled_area_sf %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(!!rlang::sym(unit_col_name_1L_chr),
                  !!rlang::sym(cases_col_name_1L_chr))
  agent_coordinates_tb <- purrr::map2_dfr(profiled_area_df %>% dplyr::select(!!unit_col_name_1L_chr) %>% dplyr::pull(),
                                          profiled_area_df %>% dplyr::select(!!rlang::sym(cases_col_name_1L_chr)) %>% dplyr::pull(),
                                          ~ randomise_locations(profiled_sf = profiled_area_sf %>%
                                                                  dplyr::filter(!!rlang::sym(unit_col_name_1L_chr)==.x),
                                                                cases_int = .y,
                                                                crs_dbl = crs_dbl))
  return(agent_coordinates_tb)
}
make_closest_yrs_ls <- function(data_lookup_tb,
                                main_incld_feature_chr,
                                target_year_1L_chr,
                                target_area_1L_chr = character(0),
                                approximation_1L_chr = "abs"){
  if(!identical(target_area_1L_chr, character(0))){
    data_lookup_tb <- data_lookup_tb %>%
      dplyr::filter(area_type_chr == target_area_1L_chr)
  }
  available_yrs_ls <- purrr::map(main_incld_feature_chr,
                                 ~ data_lookup_tb %>%
                                   dplyr::filter(main_feature_chr == .x) %>%
                                   dplyr::pull(year_chr) %>%
                                   as.numeric())
  if(approximation_1L_chr == "abs"){
    closest_yrs_ls <- purrr::map(available_yrs_ls,
                                 ~ .x[which(abs(.x - as.numeric(target_year_1L_chr)) == min(abs(.x - as.numeric(target_year_1L_chr))))])
  }
  if(approximation_1L_chr == "previous"){
    closest_yrs_ls <- purrr::map(available_yrs_ls,
                                 ~ .x[which(as.numeric(target_year_1L_chr) - .x == min(max(as.numeric(target_year_1L_chr) - .x,0)))])
  }

  if(approximation_1L_chr == "next"){
    closest_yrs_ls <- purrr::map(available_yrs_ls,
                                 ~ .x[which(.x - as.numeric(target_year_1L_chr) == min(max(.x - as.numeric(target_year_1L_chr),0)))])
  }
  return(closest_yrs_ls)
}
make_cluster_bndys <- function(clusters_chr,
                               crs_nbr_dbl,
                               distance_in_km_1L_dbl,
                               land_boundary_sf,
                               vicinity_points_ls){
  cluster_bndys_ls <- purrr::map(1:length(clusters_chr),
                                 ~ manufacture.vicinity_points(vicinity_points_ls %>% #make_geomc_dist_bndys
                                                                 purrr::pluck(.x),
                                                               land_sf = land_boundary_sf,
                                                               metres_1L_dbl = distance_in_km_1L_dbl *1000,
                                                               crs_nbr_dbl = crs_nbr_dbl,
                                                               what_1L_chr == "geometric")) %>%
    stats::setNames(., vicinity_points_ls %>% names())
  return(cluster_bndys_ls)

}
make_cluster_isochrones <- function(vicinity_points_ls,
                                    index_val_1L_int,
                                    time_min_1L_dbl = 0,
                                    time_max_1L_dbl = 30,
                                    time_steps_1L_dbl = 5,
                                    travel_mode_1L_chr = "car"){
  #require(osrm) # Make a dependency
  cluster_services_chr <- vicinity_points_ls %>%
    purrr::pluck(index_val_1L_int) %>%
    dplyr::select(service_name_chr) %>%
    dplyr::pull()
  x_vicinity_points <- vicinity_points_ls %>%
    purrr::pluck(index_val_1L_int)
  cluster_isochrone_ls <- purrr::map(cluster_services_chr,
                                     ~ manufacture.vicinity_points(x_vicinity_points,#make_isochrs_for_1_srvc
                                                                   service_1L_chr = .x,
                                                                   time_min_1L_dbl = time_min_1L_dbl,
                                                                   time_max_1L_dbl = time_max_1L_dbl,
                                                                   time_steps_1L_dbl = time_steps_1L_dbl,
                                                                   travel_mode_1L_chr = travel_mode_1L_chr,
                                                                   what_1L_chr = "isochrones")) %>% #drive time
    stats::setNames(., cluster_services_chr)
  #detach("package:osrm", unload=TRUE)
  isochrone_bands_ls <- purrr::map(1:length(cluster_isochrone_ls),
                                   ~ make_isochrone_bands(index_val_1L_int = .x,
                                                          cluster_isochrone_ls = cluster_isochrone_ls,
                                                          travel_mode_1L_chr = travel_mode_1L_chr)) %>%
    stats::setNames(cluster_isochrone_ls %>% names())
  unioned_isochrones_ls <- purrr::map(1:(isochrone_bands_ls %>%
                                           purrr::pluck(1) %>%
                                           length()),
                                      ~ bind_isochrone_bands(isochrone_bands_ls =  isochrone_bands_ls,
                                                             index_1L_int = .x,
                                                             travel_mode_1L_chr = travel_mode_1L_chr)) %>%
    stats::setNames(paste0("tb_",
                           1:(isochrone_bands_ls %>%
                                purrr::pluck(1) %>%
                                length())))
  #sf::sf_use_s2(FALSE)
  temporal_bands_ls <- purrr::accumulate(2:length(unioned_isochrones_ls),
                                         .init = unioned_isochrones_ls[[1]],
                                         .simplify = F,
                                         ~ sf::st_union(.x,
                                                        unioned_isochrones_ls[[.y]]))  %>%
    stats::setNames(paste0("tb_",
                           1:(unioned_isochrones_ls %>%
                                length())))
  temporal_bands_ls <- purrr::map(1:length(temporal_bands_ls),
                                  ~ update_isochrone_tbl(index_val_1L_int = .x,
                                                             temporal_bands_ls = temporal_bands_ls,
                                                             travel_mode_1L_chr = travel_mode_1L_chr)) %>%
    stats::setNames(paste0("tb_",
                           1:(temporal_bands_ls  %>%
                                length())))
  discrete_temporal_bands_ls <- purrr::map(1:(length(unioned_isochrones_ls)-1),
                                           ~ sf::st_difference(unioned_isochrones_ls %>% purrr::pluck(.x+1),
                                                               temporal_bands_ls %>% purrr::pluck(.x)) %>%
                                             dplyr::select(id,isomin,isomax,
                                                           center_value, ### CHECK
                                                           !!rlang::sym(paste0(travel_mode_1L_chr,"_times"))# drive_times
                                             )) %>%
    stats::setNames(paste0("tb_",
                           2:(temporal_bands_ls  %>%
                                length())))  %>%
    append(list(tb_1 = unioned_isochrones_ls %>% purrr::pluck(1) %>%
                  dplyr::mutate(center_value = (isomin + isomax) / 2) %>%
                  dplyr::select(id,isomin,isomax,center_value,!!rlang::sym(paste0(travel_mode_1L_chr,"_times")) )),
           after = 0)
  return(discrete_temporal_bands_ls)
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
make_featured_var_pfx <- function(dynamic_var_rsl_1L_chr,
                                  reference_vals_chr,# = c("tot_pop","age_sex"),
                                  reference_var_rsl_1L_chr = NULL,
                                  data_year_1L_chr){
  if(!is.null(reference_var_rsl_1L_chr)){
    nse_names_ls <- make_nse_objs_ls(spatial_unit_1L_chr = reference_var_rsl_1L_chr,
                                     concept_1L_chr = reference_vals_chr[1],#"tot_pop",
                                     reference_var_nm_1L_chr = paste0("year_",
                                                                      data_year_1L_chr,
                                                                      "pr"),
                                     grouping_var_1L_chr = dynamic_var_rsl_1L_chr,
                                     data_year_1L_chr = data_year_1L_chr)
  }else{
    nse_names_ls <- make_nse_objs_ls(spatial_unit_1L_chr = dynamic_var_rsl_1L_chr,

                                     concept_1L_chr = reference_vals_chr[2],#"age_sex",
                                     grouping_var_1L_chr = dynamic_var_rsl_1L_chr,
                                     data_year_1L_chr = data_year_1L_chr)
  }
  prefix_1L_chr <- paste0(nse_names_ls$popl_inc_unit,"_")
  return(prefix_1L_chr)
}
make_filter_by_year_logic <- function(data_tb,
                                      years_chr){
  filter_by_year_lgl <- purrr::map2_lgl(data_tb$year_chr, data_tb$year_start_chr, ~ (.x %in% years_chr | .y %in% years_chr))
  return(filter_by_year_lgl)
}
make_intersecting_profiled_area <- function(attribute_rsl_1L_chr,
                                            attribute_sf,
                                            crs_nbr_dbl,
                                            data_type_chr,
                                            data_year_1L_chr,
                                            profiled_sf,
                                            featured_var_pfx_1L_chr = character(0),
                                            profiled_sf_col_1L_chr = NA_character_,
                                            profiled_sf_row_1L_chr = NA_character_
                                            ){
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
                                           data_year_1L_chr = data_year_1L_chr,
                                           featured_var_pfx_1L_chr = featured_var_pfx_1L_chr,
                                           feature_nm_1L_chr = attribute_rsl_1L_chr)
  profiled_sf <- make_intersecting_geometries(geometry_one_sf = profiled_sf,
                                              geometry_two_sf = attribute_sf,
                                              crs_nbr_dbl = crs_nbr_dbl)
  return(profiled_sf)
}
make_intersecting_geometries <- function(geometry_one_sf,
                                         geometry_two_sf,
                                         crs_nbr_dbl,
                                         validate_1L_lgl = T){
  intersection_sf <- sf::st_intersection(geometry_one_sf %>% sf::st_transform(crs_nbr_dbl[2]),
                              geometry_two_sf %>% sf::st_transform(crs_nbr_dbl[2])) %>%
    sf::st_transform(crs_nbr_dbl[1])

  if(validate_1L_lgl)
    intersection_sf <- intersection_sf %>% make_valid_new_sf()
  return(intersection_sf)
}
make_isochrone_bands <- function(index_val_1L_int,
                                 cluster_isochrone_ls,
                                 travel_mode_1L_chr){ #"car" # CHECK
  travel_time_bands <- cluster_isochrone_ls %>%
    purrr::pluck(index_val_1L_int) %>% dplyr::pull(!!rlang::sym(paste0(travel_mode_1L_chr,"_times"))# drive_times
    )
  time_band_sf_ls <- purrr::map(travel_time_bands,
                                ~ cluster_isochrone_ls %>%
                                  purrr::pluck(index_val_1L_int) %>%
                                  dplyr::filter(!!rlang::sym(paste0(travel_mode_1L_chr,"_times")) == .x)) %>% # drive_times
    stats::setNames(paste0("tb_",
                           stringr::str_replace_all(travel_time_bands,
                                                    " ",
                                                    "_")))
  return(time_band_sf_ls)
}
make_isochrones <- function(lat_1L_dbl,
                            lng_1L_dbl,
                            server_1L_chr = character(0),
                            time_min_1L_dbl,
                            time_max_1L_dbl,
                            time_steps_1L_dbl,
                            travel_mode_1L_chr = "car"){
  if(identical(server_1L_chr, character(0)))
    server_1L_chr <- getOption("osrm.server")
  if(identical(travel_mode_1L_chr, character(0)))
    travel_mode_1L_chr <- getOption("osrm.profile")
  step_1L_dbl <- (time_max_1L_dbl-time_min_1L_dbl)/time_steps_1L_dbl
  iso_sf <- osrm::osrmIsochrone(loc = c(lng_1L_dbl, lat_1L_dbl),
                                breaks = seq(from = time_min_1L_dbl,
                                             to = time_max_1L_dbl,
                                             by = step_1L_dbl),
                                osrm.profile = travel_mode_1L_chr,
                                osrm.server = server_1L_chr)
  iso_sf <- sf::st_as_sf(iso_sf) %>%
    dplyr::mutate(!!rlang::sym(paste0(travel_mode_1L_chr,"_times")) := paste0(isomin,##drive_times # time_min_1L_dbl,
                                                                              " to ",
                                                                              isomax,#time_max_1L_dbl,
                                                                              " mins")) %>%
    dplyr::arrange(id)
  return(iso_sf)
}
make_km_sqd_dbl <- function(data_sf){
  km_sqd_dbl <- data_sf %>%
    dplyr::mutate(FT_AREA_SQKM = sf::st_area(.) %>%
                    units::set_units(km^2)) %>%
    dplyr::summarise(TOT_AREA_SQKM = sum(FT_AREA_SQKM)) %>%
    dplyr::pull(TOT_AREA_SQKM)
  return(km_sqd_dbl)
}
make_nse_objs_ls <- function(concept_1L_chr,
                             spatial_unit_1L_chr,
                             reference_var_nm_1L_chr = NULL,
                             grouping_var_1L_chr = NULL,
                             data_year_1L_chr,
                             featured_var_pfx_1L_chr){
  if(concept_1L_chr == "age_sex"){
    popl_multiplier <- paste0("inc_",spatial_unit_1L_chr,"_prop")
    whl_pop_str_1 <- paste0("whl_",spatial_unit_1L_chr,"_",featured_var_pfx_1L_chr,"y",data_year_1L_chr,".Females.")
    whl_pop_str_2 <- paste0("whl_",spatial_unit_1L_chr,"_",featured_var_pfx_1L_chr,"y",data_year_1L_chr,".Males.")
    inc_str_to_delete <- paste0("whl_",spatial_unit_1L_chr,"_")
    grouping_age_sex_popl_1L_chr <- NA_character_
  }
  if(concept_1L_chr == "tot_pop"){
    popl_multiplier <- "pop_prop_multiplier_tot_pop"
    grouping_age_sex_popl_1L_chr <- paste0("grp_by_",grouping_var_1L_chr,"_inc_age_sex_")
    whl_pop_str_1 <- paste0(grouping_age_sex_popl_1L_chr,"y",data_year_1L_chr,".Females.")
    whl_pop_str_2 <- paste0(grouping_age_sex_popl_1L_chr,"y",data_year_1L_chr,".Males.")
    inc_str_to_delete <- grouping_age_sex_popl_1L_chr
    grouping_age_sex_popl_1L_chr <- paste0("grp_by_",grouping_var_1L_chr,"_inc_age_sex_")
  }
  nse_objs_ls <- list(area_whl_unit = paste0("whl_",spatial_unit_1L_chr,"_area"),
                      area_inc_unit = paste0("inc_",spatial_unit_1L_chr,"_area"),
                      prop_inc_unit = paste0("inc_",spatial_unit_1L_chr,"_prop"),
                      popl_inc_unit = paste0("inc_",spatial_unit_1L_chr,"_popl"),
                      popl_whl_unit = paste0("whl_",spatial_unit_1L_chr,"_",reference_var_nm_1L_chr),
                      popl_multiplier = popl_multiplier,
                      popl_whl_starts_with_1 = ifelse(is.null(whl_pop_str_1),
                                                      NA_character_,
                                                      whl_pop_str_1),
                      popl_whl_starts_with_2 = ifelse(is.null(whl_pop_str_2),
                                                      NA_character_,
                                                      whl_pop_str_2),
                      grouping_1_concept_tot = ifelse(is.null(grouping_var_1L_chr),
                                                      NA_character_,
                                                      paste0("grp_by_",
                                                             grouping_var_1L_chr,
                                                             "_inc_",
                                                             concept_1L_chr)),
                      grouping_1_age_sex_pop = grouping_age_sex_popl_1L_chr,
                      inc_str_to_delete = inc_str_to_delete)
  return(nse_objs_ls)
}
make_paths_to_fls_for_ingest <- function(data_type_chr, #get_r_import_path_chr
                                         name_chr,
                                         processed_fls_dir_1L_chr){
  if(data_type_chr=="Geometry")
    name_chr <- paste0(name_chr,"_sf")
  paths_chr <- paste0(processed_fls_dir_1L_chr,"/",name_chr,".RDS")
  return(paths_chr)
}
make_polygons_from_duplicates <- function(sf, # Not sure if needed / and or properly implemented
                                          uid_1L_chr){
  sf <- sf %>% dplyr::filter(sf::st_is_valid(sf))
  duplicates_chr <- sf %>% dplyr::filter(!!rlang::sym(uid_1L_chr) %>%
                                           duplicated()) %>%
    dplyr::pull(!!rlang::sym(uid_1L_chr)) %>%
    unique()
  geometry_one_sf <- sf %>% dplyr::filter(!(!!rlang::sym(uid_1L_chr) %in%
                                              duplicates_chr))
  geometry_two_sf <- sf %>% dplyr::filter(!!rlang::sym(uid_1L_chr) %in%
                                            duplicates_chr)
  polygons_sf <- purrr::map(duplicates_chr,
                            ~ sf::st_sf(geometry_two_sf %>%
                                          dplyr::filter(!!rlang::sym(uid_1L_chr) == .x) %>%
                                          sf::st_set_geometry(NULL) %>%
                                          dplyr::summarise_all(.funs = dplyr::first),
                                        geometry = geometry_two_sf %>%
                                          dplyr::filter(!!rlang::sym(uid_1L_chr) == .x) %>%
                                          sf::st_union() %>%
                                          sf::st_sfc())) %>%
    append(list(geometry_one_sf))  %>%
    purrr::reduce(~rbind(.x,.y))
  return(polygons_sf)
}
make_path_for_raw_outp_dir <- function(category_1L_chr,
                                       raw_fls_dir_1L_chr){
  paste0(raw_fls_dir_1L_chr,"/",category_1L_chr)
}
make_reconciled_intersecting_area <- function(profiled_sf,###### make mthd
                                              crs_nbr_dbl,
                                              data_year_1L_chr,
                                              dynamic_var_rsl_1L_chr,
                                              group_by_var_1L_chr,
                                              reference_grouping_1L_chr,
                                              reference_vals_chr,# = c("tot_pop","age_sex"),
                                              reference_var_rsl_1L_chr,
                                              spatial_atts_ls,
                                              profiled_sf_col_1L_chr = NA,
                                              profiled_sf_row_1L_chr = NA){
  if(!is.null(reference_var_rsl_1L_chr)){
    if(reference_grouping_1L_chr %in% names(spatial_atts_ls[[reference_var_rsl_1L_chr]])){
      spatial_atts_ls[[dynamic_var_rsl_1L_chr]] <- merge(spatial_atts_ls[[reference_var_rsl_1L_chr]],
                                                      sf::st_set_geometry(spatial_atts_ls[[dynamic_var_rsl_1L_chr]],NULL),
                                                      by = reference_grouping_1L_chr) %>%
        dplyr::distinct(.keep_all = T) %>%
        dplyr::select(-dplyr::ends_with(".x")) %>%
        dplyr::rename_at(.vars = dplyr::vars(dplyr::ends_with(".y")),
                         ~ stringi::stri_replace_last_regex(.x,"\\.y$",""))
      spatial_atts_ls[[dynamic_var_rsl_1L_chr]] <- rename_vars_based_on_res(sf = spatial_atts_ls[[dynamic_var_rsl_1L_chr]],
                                                                         data_type_chr = reference_vals_chr[1],
                                                                         data_year_1L_chr = data_year_1L_chr,
                                                                         feature_nm_1L_chr = reference_var_rsl_1L_chr) %>%
        add_km_sqd(feature_nm_1L_chr = reference_var_rsl_1L_chr)
    }
  }
  spatial_atts_ls[[dynamic_var_rsl_1L_chr]] <- spatial_atts_ls[[dynamic_var_rsl_1L_chr]] %>%
    add_km_sqd_by_group(group_by_var_1L_chr = reference_grouping_1L_chr,
                           feature_nm_1L_chr = dynamic_var_rsl_1L_chr)
  profiled_sf <- make_intersecting_profiled_area(profiled_sf = profiled_sf,
                                                 profiled_sf_col_1L_chr = profiled_sf_col_1L_chr,
                                                 profiled_sf_row_1L_chr = profiled_sf_row_1L_chr,
                                                 attribute_sf = spatial_atts_ls[[dynamic_var_rsl_1L_chr]],
                                                 attribute_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                                 data_type_chr = reference_vals_chr[2],
                                                 data_year_1L_chr = data_year_1L_chr,
                                                 crs_nbr_dbl = crs_nbr_dbl)
  if(!is.null(reference_var_rsl_1L_chr)){
    if(!reference_grouping_1L_chr %in% names(spatial_atts_ls[[reference_var_rsl_1L_chr]])){
      profiled_sf <- make_intersecting_profiled_area(profiled_sf = profiled_sf,
                                                     profiled_sf_col_1L_chr = profiled_sf_col_1L_chr,
                                                     profiled_sf_row_1L_chr = profiled_sf_row_1L_chr,
                                                     attribute_sf = spatial_atts_ls[[reference_var_rsl_1L_chr]] %>%
                                                       add_km_sqd(feature_nm_1L_chr = reference_var_rsl_1L_chr) ,
                                                     attribute_rsl_1L_chr = reference_var_rsl_1L_chr,
                                                     data_type_chr = reference_vals_chr[1])
    }
  }
  profiled_sf <- update_popl_counts(profiled_sf = profiled_sf,
                                           group_by_var_1L_chr = group_by_var_1L_chr,
                                           dynamic_var_nm_1L_chr = reference_grouping_1L_chr,
                                           data_year_1L_chr = data_year_1L_chr,
                                           dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                           reference_var_rsl_1L_chr = reference_var_rsl_1L_chr,
                                    reference_vals_chr = reference_vals_chr)

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
make_sf_rows_fn <- function(...){
  attribution_1L_chr <- "Based on: https://github.com/r-spatial/sf/issues/49"
  sf_list <- rlang::dots_values(...)[[1]]
  sfg_list_column <- lapply(sf_list, function(sf) sf$geometry[[1]]) %>% sf::st_sfc()
  df <- lapply(sf_list, function(sf) sf::st_set_geometry(sf, NULL)) %>% dplyr::bind_rows()
  sf_rows_fn <- sf::st_sf(data.frame(df, geom=sfg_list_column))
  return(sf_rows_fn)
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
  valid_sf <- valid_sf %>%
    dplyr::filter(sf::st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    sf::st_cast("MULTIPOLYGON") %>%
    dplyr::distinct(.keep_all = T)
  return(valid_sf)
}

# make_trvl_tm_isochrs <- function(appID, # No longer required????
#                                  apiKey, # https://stackoverflow.com/questions/40489162/draw-time-radius-around-lat-long-on-map
#                                  origin,
#                                  mode_of_transport_1L_chr = "driving",
#                                  travel_time_hours,
#                                  crs){
#   location <- origin
#   travel_time_secs <- travel_time_hours * 60 * 60
#   url <- "http://api.traveltimeapp.com/v4/time-map"
#   requestBody <- paste0('{
#                         "departure_searches" : [
#                         {"id" : "test",
#                         "coords": {"lat":', origin[1], ', "lng":', origin[2],' },
#                         "transportation" : {"type" : "', mode_of_transport_1L_chr,'" } ,
#                         "travel_time" : ', travel_time_secs, ',
#                         "departure_time" : "2017-05-03T08:00:00z"
#                         }
#                         ]
# }') # Check lat and lng are correct
#   res <- httr::POST(url = url,
#                     httr::add_headers('Content-Type' = 'application/json'),
#                     httr::add_headers('Accept' = 'application/json'),
#                     httr::add_headers('X-Application-Id' = appId),
#                     httr::add_headers('X-Api-Key' = apiKey),
#                     body = requestBody,
#                     encode = "json")
#
#   res <- jsonlite::fromJSON(as.character(res))
#   pl <- lapply(res$results$shapes[[1]]$shell, function(x){
#     googleway::encode_pl(lat = x[['lat']], lon = x[['lng']]) # Check lat and lng are correct
#   })
#   df <- data.frame(polyline = unlist(pl))
#   #df_marker <- data.frame(lat = location[1], lon = location[2])
#   polyline_vec <- df %>% dplyr::pull(polyline)
#   list_of_sfs <- purrr::map(polyline_vec,
#                             ~ transform_polyline_to_sf(polyline_xx = .x,
#                                                          mode_of_transport_1L_chr = mode_of_transport_1L_chr,
#                                                          travel_time_hours_1L_dbl = travel_time_hours_1L_dbl,
#                                                          crs_1L_dbl = crs))
#
#   new_sf <- purrr::reduce(list_of_sfs,
#                           rbind)
#   return(new_sf)
# }

# make_att_data_xx <- function(x_VicinityLookup, now manufacture mthd
#                               match_1L_chr,
#                               starter_sf){
#   data_lookup_tb <- x_VicinityLookup@vicinity_processed_r3
#   att_data_xx <- ingest(data_lookup_tb,
#                          col_nm_1L_chr = "name_chr",
#                          match_value_xx = match_1L_chr)
#   if(is.data.frame(att_data_xx)){
#     att_data_xx <- list(att_data_xx) %>%
#       stats::setNames(ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb,
#                                                match_1L_chrerence = match_1L_chr,
#                                                match_var_nm_1L_chr = "name_chr",
#                                                target_var_nm_1L_chr = "year_chr",
#                                                evaluate_1L_lgl = FALSE))
#   }
#   region_short_nm_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb,
#                                                      match_value_xx = match_1L_chr,
#                                                      match_var_nm_1L_chr = "name_chr",
#                                                      target_var_nm_1L_chr = "region_chr",
#                                                      evaluate_1L_lgl = FALSE)
#   region_short_long_chr <- c(region_short_nm_1L_chr,
#                              ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_abbreviations_r3,
#                                                       match_value_xx = region_short_nm_1L_chr,
#                                                       match_var_nm_1L_chr = "short_name_chr",
#                                                       target_var_nm_1L_chr = "long_name_chr",
#                                                       evaluate_1L_lgl = FALSE))
#   area_names_var_chr <- ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb,
#                                                  match_value_xx = match_1L_chr,
#                                                  match_var_nm_1L_chr = "name_chr",
#                                                  target_var_nm_1L_chr = "area_type_chr",
#                                                  evaluate_1L_lgl = FALSE) %>%
#     ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_templates_r3,
#                              match_value_xx = .,
#                              match_var_nm_1L_chr = "area_type_chr",
#                              target_var_nm_1L_chr = "subdivision_chr",
#                              evaluate_1L_lgl = FALSE)
#   area_names_var_chr <- area_names_var_chr[area_names_var_chr %in% names(starter_sf)]
#   boundary_year_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb,
#                                                    match_value_xx = match_1L_chr,
#                                                    match_var_nm_1L_chr = "name_chr",
#                                                    target_var_nm_1L_chr = "area_bndy_yr_chr",
#                                                    evaluate_1L_lgl = F)
#   area_names_var_chr <- x_VicinityLookup@vicinity_identifiers_r3 %>%
#     dplyr::filter(var_name_chr %in% area_names_var_chr) %>%
#     dplyr::filter(as.numeric(year_chr) == max(as.numeric(year_chr)[as.numeric(year_chr) <= as.numeric(boundary_year_1L_chr)])) %>%
#     dplyr::pull(var_name_chr)
#   att_data_xx <- manufacture(x_VicinityLookup,# updateAttrDataXx
#                               att_data_xx = att_data_xx,
#                               altv_names_sf= starter_sf,
#                               area_names_var_chr = area_names_var_chr,
#                               region_short_long_chr = region_short_long_chr,
#                               match_value_xx = match_1L_chr)
#   return(att_data_xx)
# }
# make_imports_chr <- function(x_VicinityLookup,#lookup_tbs_r4, # Now manufacture mthd
#                              data_type_1L_chr){
#   if(data_type_1L_chr == "Geometry"){
#     imports_chr <- x_VicinityLookup@vicinity_raw_r3 %>%
#       dplyr::filter(main_feature_chr == "Boundary") %>% dplyr::pull(name_chr)
#   }else{
#     imports_chr <- x_VicinityLookup@vicinity_raw_r3 %>%
#       dplyr::filter(data_type_1L_chr == "Attribute") %>% dplyr::pull(name_chr)
#     return(imports_chr)
#   }
# }
# make_attributes_ls <- function(input_ls, # get_sp_data or similar ???
#                                subdivision_1L_chr = NULL,
#                                match_year_1L_lgl = TRUE,
#                                exclude_dif_bndy_yr_1L_lgl = TRUE){
#   years_chr <- manufacture(input_ls$x_VicinityProfile,
#                            input_ls = input_ls,
#                            what_1L_chr = "years")#make_years_chr
#   attributes_to_import_chr = procure(input_ls$x_VicinityProfile, # Formally get_spatial_attr_names(
#                                      exclude_dif_bndy_yr_1L_lgl = exclude_dif_bndy_yr_1L_lgl,
#                                      highest_rsl_chr = input_ls$at_highest_res,
#                                      key_var_1L_chr = input_ls$key_var_1L_chr,
#                                      #subdivision_1L_chr = NULL,
#                                      match_year_1L_lgl = match_year_1L_lgl,
#                                      years_chr = years_chr,
#                                      what_1L_chr = "grouping")
#   attributes_ls <- manufacture(input_ls$x_VicinityProfile,
#                                attributes_to_import_chr = attributes_to_import_chr,
#                                key_var_1L_chr  = input_ls$key_var_1L_chr,
#                                specified_rsl_chr  = input_ls$at_specified_res,
#                                what_1L_chr = "attributes",
#                                years_chr  = years_chr)
#   return(attributes_ls)
# }
# make_merge_sf_chr <- function(x_VicinityLookup, Now manufacture method
#                               y_vicinity_raw,
#                               processed_fls_dir_1L_chr = NULL){
#   if(is.null(y_vicinity_raw %>% dplyr::pull(add_bndys_from_ls) %>% purrr::pluck(1))){
#     merge_sf_chr <- NA_character_
#   }else{
#     if(is.na(y_vicinity_raw %>% dplyr::pull(add_bndys_from_ls) %>% purrr::pluck(1)) %>% any()){
#       merge_sf_chr <- NA_character_
#     }else{
#       merge_sf_chr <- purrr::map_chr(y_vicinity_raw %>% pull(add_bndys_from_ls) %>% purrr::pluck(1),
#                                      ~ ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
#                                                                 match_value_xx = .x,
#                                                                 match_var_nm_1L_chr = "uid_chr",
#                                                                 target_var_nm_1L_chr = "name_chr",
#                                                                 evaluate_1L_lgl = FALSE) %>%
#                                        ready4::get_from_lup_obj(data_lookup_tb = x_VicinityLookup@vicinity_raw_r3,
#                                                                 match_value_xx = .,
#                                                                 match_var_nm_1L_chr = "name_chr",
#                                                                 target_var_nm_1L_chr = "source_reference_chr",
#                                                                 evaluate_1L_lgl = FALSE) %>%
#                                        ifelse(stringr::str_detect(.,"::"),
#                                               .,
#                                               paste0("readRDS(\"",processed_fls_dir_1L_chr,"/",.,".rds\")")))
#     }
#   }
#   return(merge_sf_chr)
# }
# make_paths_chr <- function(x_vicinity_raw, Now manufacture mthd
#                            dir_1L_chr,
#                            data_match_value_xx,
#                            match_var_nm_1L_chr,
#                            sub_dirs_chr){
#   paths_chr <- purrr::map_chr(sub_dirs_chr,
#                               ~ ready4::get_from_lup_obj(data_lookup_tb = x_vicinity_raw,
#                                                          match_value_xx = data_match_value_xx,
#                                                          match_var_nm_1L_chr = match_var_nm_1L_chr,
#                                                          target_var_nm_1L_chr = .x,
#                                                          evaluate_1L_lgl = FALSE))
#   paths_chr <- purrr::accumulate(paths_chr,
#                                  ~ paste0(.x,"/",.y)) %>%
#     paste0(dir_1L_chr,
#            "/",
#            .)
#   return(paths_chr)
# }
# make_drive_time_for_one_service <- function(cluster_tb, #make_1_clstr_1_srvc_trvl_tm # now manufacture
#                                             service_1L_chr,
#                                             time_min_1L_dbl,
#                                             time_max_1L_dbl,
#                                             time_steps_1L_dbl){
#   one_service_tb <- cluster_tb %>%
#     dplyr::filter(service_name_chr == service_1L_chr)
#   one_service_sf <- make_isochrones(lng_1L_dbl = one_service_tb %>% dplyr::select(lng_dbl) %>% dplyr::pull(),
#                                                lat_1L_dbl = one_service_tb %>% dplyr::select(lat_dbl) %>% dplyr::pull(),
#                                                time_min_1L_dbl = time_min_1L_dbl,
#                                                time_max_1L_dbl = time_max_1L_dbl,
#                                                time_steps_1L_dbl = time_steps_1L_dbl)
#   return(one_service_sf)
# }
# make_geomc_dist_bndys <- function(point_locations_tb, # manufacture.vicinity_points
#                                   land_sf,
#                                   distance_dbl,
#                                   crs_nbr_dbl){
#   distance_from_pts_sf <- sf::st_as_sf(point_locations_tb,
#                                        coords = c("lng_dbl", "lat_dbl"),
#                                        crs = crs_nbr_dbl[1]) %>% #4326)
#     sf::st_transform(crs_nbr_dbl[2]) ##3577
#   distance_from_pts_on_land_sf <- sf::st_buffer(distance_from_pts_sf,
#                                                 dist = distance_dbl) %>%
#     sf::st_union() %>%
#     sf::st_intersection(land_sf %>%
#                           sf::st_transform(crs_nbr_dbl[2])) %>% #3577
#     sf::st_transform(crs_nbr_dbl[1]) %>%
#     sf::st_sf()
#   return(distance_from_pts_on_land_sf)
# }
# make_distance_based_bands <- function(distance_in_km_1L_dbl, # Not a manufacuture mthd for vicinity_points
#                                       bands_1L_dbl,
#                                       service_cluster_tb,
#                                       profiled_sf,
#                                       crs_nbr_dbl){
#   distances_dbl <- seq(from = distance_in_km_1L_dbl/bands_1L_dbl,
#                        to = distance_in_km_1L_dbl,
#                        by = distance_in_km_1L_dbl/bands_1L_dbl)
#
#   service_clusters_chr <- service_cluster_tb %>% dplyr::pull(cluster_name_chr) %>% unique()
#   service_vicinity_points_ls <- purrr::map(service_clusters_chr,
#                                           ~ service_cluster_tb %>%
#                                             dplyr::filter(cluster_name_chr == .x)) %>%
#     stats::setNames(service_clusters_chr)
#   service_clusters_by_distance_ls <- purrr::map(distances_dbl,
#                                                   ~ make_cluster_bndys(distance_in_km_1L_dbl = .x,
#                                                                                        clusters_chr = service_clusters_chr,
#                                                                                        vicinity_points_ls = service_vicinity_points_ls,
#                                                                                        land_boundary_sf = profiled_sf,
#                                                                                        crs_nbr_dbl = crs_nbr_dbl)) %>%
#     stats::setNames(., paste0("km_",
#                               distances_dbl,
#                               "from_service"))
#   geometric_distance_by_cluster_circles <- purrr::map(1:length(service_clusters_chr),
#                                                       ~ reorder_clusters_by_distances(index_val_1L_int = .x,
#                                                                                          clusters_by_distance_ls = service_clusters_by_distance_ls,
#                                                                                          distances_dbl = distances_dbl)) %>%
#     stats::setNames(., service_vicinity_points_ls %>% names())
#   geometric_distance_by_cluster_bands <- purrr::map(geometric_distance_by_cluster_circles,
#                                                     ~ transform_circles_to_bands(geomc_dist_circles_ls = .x)) %>%
#     stats::setNames(., service_vicinity_points_ls %>% names())
#   geometric_distance_by_cluster_circles_merged_list <- purrr::map(geometric_distance_by_cluster_circles,
#                                                                   ~ do.call(rbind,.x)) %>%
#     stats::setNames(., service_vicinity_points_ls %>% names()) %>%
#     purrr::map(.,
#                ~ .x %>% dplyr::arrange(desc(distance_in_km_dbl)))
#   geometric_distance_by_cluster_bands_merged_list <- purrr::map(geometric_distance_by_cluster_bands,
#                                                                 ~ do.call(rbind,.x)) %>%
#     stats::setNames(., service_vicinity_points_ls %>% names()) %>%
#     purrr::map(.,
#                ~ .x %>% dplyr::arrange(desc(distance_in_km_dbl)) %>%
#                  transform_to_simpler_sf(crs = crs_nbr_dbl[1]))
#   return(geometric_distance_by_cluster_bands_merged_list)
# }
# make_profiled_area_objs <- function(x_VicinityProfile){ Now a manufacture mthd
#   group_by_var_1L_chr <- procure(x_VicinityProfile,#get_group_by_var_from_VicinityProfile
#                                  what_1L_chr = "grouping")
#   st_profiled_sf <- ingest(x_VicinityProfile, # get_starter_sf_for_profiled_area
#                            key_var_1L_chr = group_by_var_1L_chr)
#   subdivision_var_nm_1L_chr <- ifelse(x_VicinityProfile@use_coord_lup_lgl,
#                                       x_VicinityProfile@VicinityLookup@vicinity_identifiers_r3 %>%
#                                         ready4::get_from_lup_obj(match_var_nm_1L_chr = "spatial_unit_chr",
#                                                                  match_value_xx = x_VicinityProfile@region_type,
#                                                                  target_var_nm_1L_chr = "var_name_chr",
#                                                                  evaluate_1L_lgl = F),
#                                       ready4::get_from_lup_obj(data_lookup_tb = x_VicinityProfile@VicinityLookup@vicinity_templates_r3 %>%
#                                                                  dplyr::filter(country_chr == x_VicinityProfile@country_chr) %>%
#                                                                  dplyr::filter(area_bndy_yr_dbl == x_VicinityProfile@area_bndy_yr_dbl),
#                                                                match_var_nm_1L_chr = "area_type_chr",
#                                                                match_value_xx = x_VicinityProfile@area_type_chr,
#                                                                target_var_nm_1L_chr = "subdivision_chr",
#                                                                evaluate_1L_lgl = FALSE))
#   if(!x_VicinityProfile@use_coord_lup_lgl){
#     profiled_sf <- st_profiled_sf
#     profiled_area_bands_ls <- make_sf_ls(profiled_sf = profiled_sf,
#                                          group_by_var_1L_chr = group_by_var_1L_chr)
#     subdivisions_chr <- profiled_sf %>%
#       dplyr::pull(!!rlang::sym(subdivision_var_nm_1L_chr)) %>%
#       as.character() %>%
#       unique()
#   }else{
#     y_vicinity_points <- x_VicinityProfile@a_VicinityLookup@vicinity_points_r3 %>%
#       dplyr::filter(service_name_chr %in% x_VicinityProfile@features_chr)
#     if(!is.na(geom_dist_limit_km(x_VicinityProfile))){
#       profiled_sf <- manufacture.vicinity_points(y_vicinity_points,
#                                                  bands_1L_dbl = x_VicinityProfile@nbr_bands_dbl,#,
#                                                  crs_nbr_dbl = x_VicinityProfile@crs_dbl,
#                                                  land_sf =  st_profiled_sf,
#                                                  metres_1L_dbl = x_VicinityProfile@geomc_dist_limit_km_dbl *1000, # make_distance_based_bands
#                                                  type_1L_chr = "bands",
#                                                  what_1L_chr = "geometric"
#       )[[1]]
#       profiled_area_bands_ls <- make_sf_ls(profiled_sf = profiled_sf,
#                                            group_by_var_1L_chr = group_by_var_1L_chr)
#     }
#     if(!is.na(drive_time_limit_mins(x_VicinityProfile))){
#       profiled_area_bands_ls <- make_cluster_isochrones(vicinity_points_ls = list(y_vicinity_points),
#                                                             index_val_1L_int = 1,
#                                                             time_min_1L_dbl = 0,
#                                                             time_max_1L_dbl = drive_time_limit_mins(x_VicinityProfile),
#                                                             time_steps_1L_dbl = x_VicinityProfile@nbr_bands_dbl)
#       names(profiled_area_bands_ls) <- paste0("dt_band_",1:length(profiled_area_bands_ls))
#       profiled_sf <- do.call(rbind,profiled_area_bands_ls) %>%
#         sf::st_transform(x_VicinityProfile@crs_dbl[1]) %>%
#         transform_to_simpler_sf()
#     }
#     subdivisions_chr <- make_intersecting_geometries(geometry_one_sf = st_profiled_sf,
#                                                      geometry_two_sf = profiled_sf,
#                                                      crs_nbr_dbl = x_VicinityProfile@crs_dbl) %>%
#       dplyr::pull(!!rlang::sym(subdivision_var_nm_1L_chr)) %>%
#       as.vector()%>%
#       unique()
#   }
#   profiled_area_objs_ls <- list(subdivisions_chr = subdivisions_chr,
#                                 profiled_sf = profiled_sf,
#                                 profiled_area_bands_ls = profiled_area_bands_ls)
#   return(profiled_area_objs_ls)
# }
# make_env_param_tb <- function(n_its_int, # Now manufacture mthd
#                               env_str_param_tb,
#                               mape_str_param_tb,
#                               joint_dstr_1L_lgl){
#   param_val_mape <- reckon(x = mape_str_param_tb,
#                            n_its_int = n_its_int,
#                            joint_dstr_1L_lgl = joint_dstr_1L_lgl)
#   param_val_env <- reckon(x = env_str_param_tb,
#                           n_its_int = n_its_int)
#   env_param_tb <- dplyr::bind_rows(param_val_env,
#                                    param_val_mape)
# }
# make_spatial_atts_ls <- function(input_ls, # Now manufacture mthd Perhaps formerly make_sp_data_ls
#                                   subdivisions_chr){
#   lists_to_merge <- purrr::map(subdivisions_chr,
#                                ~ manufacture(input_ls$x_VicinityProfile,#make_attributes_ls
#                                              exclude_dif_bndy_yr_1L_lgl = TRUE,
#                                              input_ls = input_ls,
#                                              match_year_1L_lgl = FALSE,
#                                              subdivision_1L_chr = .x,
#                                              type_1L_chr = "outer"))
#   lists_to_merge <- purrr::transpose(lists_to_merge)
#   merged_list <- purrr::map(lists_to_merge[2:length(lists_to_merge)],
#                             ~ do.call(rbind,.x))
#   names_ppr <- purrr::map_chr(lists_to_merge[[1]],
#                               ~ ifelse(length(.x[1])==0,
#                                        NA_character_,
#                                        names(.x[1])))
#   ppr_idx_dbl <- purrr::map_dbl(lists_to_merge[[1]],
#                                 ~ ifelse(length(.x[1])==0,
#                                          NA_real_,
#                                          .x[1])) %>%
#     stats::setNames(names_ppr)
#   spatial_atts_ls <- append(merged_list,list(ppr_idx_dbl = ppr_idx_dbl),
#                              after = 0)
#   return(spatial_atts_ls)
# }
# make_years_chr <- function(input_ls){ # Now manufacture mthd
#   model_end_year <- calculate_end_date(input_ls = input_ls) %>% lubridate::year()
#   key_var_1L_chr <- input_ls$key_var_1L_chr
#   data_year_1L_chr <- input_ls$x_VicinityProfile@data_year_1L_chr
#   x_VicinityLookup <- input_ls$x_VicinityProfile@a_VicinityLookup
#   spatial_lookup_tb <- x_VicinityLookup@vicinity_processed_r3
#   year_opts <- spatial_lookup_tb %>%
#     dplyr::filter(main_feature_chr == key_var_1L_chr) %>%
#     dplyr::pull(year_end_chr)
#   year_opts <- year_opts[stringr::str_length(year_opts)==4]
#   year_opts_ref <- which((year_opts %>%
#                             as.numeric() %>%
#                             sort()) >= model_end_year) %>% min()
#   model_end_year <- year_opts %>%
#     as.numeric() %>%
#     sort() %>% purrr::pluck(year_opts_ref) %>%
#     as.character()
#   as.character(as.numeric(data_year_1L_chr):as.numeric(model_end_year))
# }
