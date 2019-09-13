#' @title make_sp_data_list
#' @description FUNCTION_DESCRIPTION
#' @param input_data PARAM_DESCRIPTION
#' @param sub_div_units_vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}},\code{\link[purrr]{transpose}},\code{\link[purrr]{prepend}}
#'  \code{\link[stats]{setNames}}
#' @rdname make_sp_data_list
#' @export
#' @importFrom purrr map transpose map_chr map_dbl prepend
#' @importFrom stats setNames
make_sp_data_list <- function(input_data,
                              sub_div_units_vec){
  lists_to_merge <- purrr::map(sub_div_units_vec,
                               ~ get_spatial_data_list(input_data = input_data,
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

#' @title make_profiled_area_objs
#' @description FUNCTION_DESCRIPTION
#' @param profiled_area_input PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[ready4s4]{lookup_tb}},\code{\link[ready4s4]{sp_starter_sf_lup}},\code{\link[ready4s4]{country}},\code{\link[ready4s4]{area_type}},\code{\link[ready4s4]{use_coord_lup}},\code{\link[ready4s4]{sp_site_coord_lup}},\code{\link[ready4s4]{features}},\code{\link[ready4s4]{geom_dist_limit_km}},\code{\link[ready4s4]{nbr_bands}},\code{\link[ready4s4]{crs_nbr}},\code{\link[ready4s4]{drive_time_limmit_mins}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[sf]{st_transform}},\code{\link[sf]{geos_binary_ops}}
#' @rdname make_profiled_area_objs
#' @export
#' @importFrom ready4utils data_get
#' @importFrom ready4s4 lookup_tb sp_starter_sf_lup country area_type use_coord_lup sp_site_coord_lup features geom_dist_limit_km nbr_bands crs_nbr drive_time_limmit_mins
#' @importFrom dplyr filter pull
#' @importFrom rlang sym
#' @importFrom sf st_transform st_intersection
make_profiled_area_objs <- function(profiled_area_input){
  group_by_var <- get_group_by_var_from_pai(profiled_area_input = profiled_area_input)
  st_profiled_sf <- get_starter_sf_for_profiled_area(profiled_area_input = profiled_area_input,
                                                     group_by_var = group_by_var)
  main_sub_div_var <- ifelse(ready4s4::use_coord_lup(profiled_area_input),
                             "STE_NAME16", ## UPDATE WITH LUP REFERENCE
                             ready4utils::data_get(data_lookup_tb = profiled_area_input %>%
                                                     ready4s4::lookup_tb() %>%
                                                     ready4s4::sp_starter_sf_lup() %>%
                                                     dplyr::filter(country == ready4s4::country(profiled_area_input)),
                                                   lookup_variable = "area_type",
                                                   lookup_reference = ready4s4::area_type(profiled_area_input),
                                                   target_variable = "sf_main_sub_div",
                                                   evaluate = FALSE))
  if(!ready4s4::use_coord_lup(profiled_area_input)){
    profiled_sf <- st_profiled_sf
    profiled_area_bands_list <- subset_sf_by_feature(profiled_sf = profiled_sf,
                                                     group_by_var = group_by_var)
    sub_div_units_vec <- profiled_sf %>%
      dplyr::pull(!!rlang::sym(main_sub_div_var)) %>%
      as.character() %>%
      unique()
  }else{
    cluster_tb = ready4s4::lookup_tb(profiled_area_input) %>%
      ready4s4::sp_site_coord_lup() %>%
      dplyr::filter(service_type %in% ready4s4::area_type(profiled_area_input))  %>%
      dplyr::filter(service_name %in% ready4s4::features(profiled_area_input))
    if(!is.na(ready4s4::geom_dist_limit_km(profiled_area_input))){
      profiled_sf <- gen_distance_based_bands(distance_km_outer = ready4s4::geom_dist_limit_km(profiled_area_input), # *1000
                                              nbr_distance_bands = ready4s4::nbr_bands(profiled_area_input),
                                              service_cluster_tb = cluster_tb,
                                              profiled_sf =  st_profiled_sf,
                                              crs_nbr = ready4s4::crs_nbr(profiled_area_input))[[1]]
      profiled_area_bands_list <- subset_sf_by_feature(profiled_sf = profiled_sf,
                                                       group_by_var = group_by_var)
    }
    if(!is.na(ready4s4::drive_time_limmit_mins(profiled_area_input))){
      profiled_area_bands_list <- cluster_isochrones(cluster_tbs_list = list(cluster_tb),
                                                     look_up_ref = 1,
                                                     time_min = 0,
                                                     time_max = ready4s4::drive_time_limmit_mins(profiled_area_input),
                                                     nbr_time_steps = ready4s4::nbr_bands(profiled_area_input))
      names(profiled_area_bands_list) <- paste0("dt_band_",1:length(profiled_area_bands_list))
      profiled_sf <- do.call(rbind,profiled_area_bands_list) %>%
        sf::st_transform(ready4s4::crs_nbr(profiled_area_input)[1]) %>%
        simplify_sf()
    }
    sub_div_units_vec <- intersect_lon_lat_sfs(sf_1 = st_profiled_sf,
                                               sf_2 = profiled_sf,
                                               crs_nbr_vec = ready4s4::crs_nbr(profiled_area_input)) %>%
      dplyr::pull(!!rlang::sym(main_sub_div_var)) %>%
      as.vector()%>%
      unique()
  }
  return(list(sub_div_units_vec = sub_div_units_vec, # previously state_territory
              profiled_sf = profiled_sf,
              profiled_area_bands_list = profiled_area_bands_list))
}

#' @title extend_sp_data_list
#' @description FUNCTION_DESCRIPTION
#' @param sp_data_list PARAM_DESCRIPTION
#' @param input_data PARAM_DESCRIPTION
#' @param profiled_area_bands_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4s4]{crs_nbr}},\code{\link[ready4s4]{geom_dist_limit_km}},\code{\link[ready4s4]{drive_time_limmit_mins}},\code{\link[ready4s4]{lookup_tb}},\code{\link[ready4s4]{sp_uid_lup}},\code{\link[ready4s4]{data_year}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[sf]{geos_measures}}
#' @rdname extend_sp_data_list
#' @export
#' @importFrom ready4s4 crs_nbr geom_dist_limit_km drive_time_limmit_mins lookup_tb sp_uid_lup data_year
#' @importFrom ready4utils data_get
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map map2
#' @importFrom sf st_area
extend_sp_data_list <- function(sp_data_list,
                                input_data,
                                profiled_area_bands_list){
  crs_nbr_vec <-  input_data$profiled_area_input %>% ready4s4::crs_nbr()
  at_highest_res = input_data$at_highest_res
  distance_km = ready4s4::geom_dist_limit_km(input_data$profiled_area_input)
  travel_time_mins = ready4s4::drive_time_limmit_mins(input_data$profiled_area_input)
  group_by_var <- get_group_by_var_from_pai(input_data$profiled_area_input)
  age_sex_pop_resolution <- names(sp_data_list)[which(at_highest_res == input_data$age_sex_pop_str) + 1]
  age_sex_counts_grouped_by <- ready4utils::data_get(data_lookup_tb = ready4s4::lookup_tb(input_data$profiled_area_input) %>%
                                                       ready4s4::sp_uid_lup() %>%
                                                       dplyr::filter(year %in% c(ready4s4::data_year(input_data$profiled_area_input)#,
                                                                                 #"All" # May need replacement
                                                       )),
                                                     lookup_variable = "spatial_unit",
                                                     lookup_reference = age_sex_pop_resolution,
                                                     target_variable = "var_name",
                                                     evaluate = FALSE)
  tot_pop_resolution <- NULL
  if(!is.null(input_data$tot_pop_str))
    tot_pop_resolution <- names(sp_data_list)[which(at_highest_res == input_data$tot_pop_str) + 1]
  # if(ready4s4::use_coord_lup(input_data$profiled_area_input))
  #   profiled_area_bands_list <- purrr::map(profiled_area_bands_list,
  #                                          ~ .x %>%
  #                                            sf::st_transform(ready4s4::crs_nbr(input_data$profiled_area_input)[1]))
  by_band_pop_counts_sf_ls <- purrr::map(profiled_area_bands_list,
                                         ~ intersect_sfs_update_counts(profiled_sf = .x,
                                                                       profiled_colref = NA,
                                                                       profiled_rowref = NA,
                                                                       sp_data_list = sp_data_list,
                                                                       tot_pop_resolution = tot_pop_resolution,
                                                                       age_sex_pop_resolution = age_sex_pop_resolution,
                                                                       group_by_var = group_by_var,
                                                                       age_sex_counts_grouped_by = age_sex_counts_grouped_by,
                                                                       data_year = ready4s4::data_year(input_data$profiled_area_input),
                                                                       crs_nbr_vec = crs_nbr_vec))
  by_band_pop_counts_sf_ls <- purrr::map2(by_band_pop_counts_sf_ls,
                                          names(by_band_pop_counts_sf_ls),
                                          ~ .x %>%
                                            dplyr::mutate(pop_sp_unit_id = paste0(.y,
                                                                                  "_",
                                                                                  tolower(age_sex_pop_resolution),
                                                                                  "_",
                                                                                  rownames(.x))) %>%
                                            dplyr::mutate(pop_sp_unit_area = sf::st_area(.)))
  profiled_sf <- do.call(rbind,by_band_pop_counts_sf_ls)
  popl_var_prefix <- get_popl_var_prefix(age_sex_pop_resolution = age_sex_pop_resolution,
                                         tot_pop_resolution = tot_pop_resolution,
                                         data_year = ready4s4::data_year(input_data$profiled_area_input))
  profiled_sf <- drop_grouped_popl_vars(profiled_sf = profiled_sf,
                                        popl_var_prefix = popl_var_prefix)
  ##
  # dyn_sf <- sp_data_list[[sp_data_list$ppr_ref]] %>% ## Should reference this from lookup
  #   dplyr::select(1)
  # profiled_sf <- simplify_sf(profiled_sf,
  #                          crs = sf::st_crs(dyn_sf)[[1]])
  # dyn_sf <- simplify_sf(dyn_sf)

  profiled_sf <- add_dynamic_sp_vars_to_sf(dynamic_sp_vars_sf = sp_data_list[[sp_data_list$ppr_ref]] %>%
                                             dplyr::select(1),
                                           pop_attr_sf = profiled_sf,
                                           age_sex_pop_resolution = "UNIT_ID",
                                           age_sex_var_name = "pop_sp_unit_id",
                                           popl_var_prefix = popl_var_prefix,
                                           data_year = input_data$profiled_area_input@data_year,
                                           crs_nbr_vec = crs_nbr_vec)
  extended_sp_data_list <- append(sp_data_list,
                                  list(profiled_sf = profiled_sf,
                                       popl_var_prefix = popl_var_prefix)) # Is pop_val_prefix needed in this list?
  return(extended_sp_data_list)
}

#' @title get_data_year_chr
#' @description FUNCTION_DESCRIPTION
#' @param data_ymdhms PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[lubridate]{year}}
#' @rdname get_data_year_chr
#' @export
#' @importFrom lubridate year
get_data_year_chr <- function(data_ymdhms){
  data_ymdhms %>%
    lubridate::year() %>%
    as.character()
}
#' @title get_model_end_ymdhs
#' @description FUNCTION_DESCRIPTION
#' @param input_data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[lubridate]{period}}
#' @rdname get_model_end_ymdhs
#' @export
#' @importFrom lubridate years weeks days
get_model_end_ymdhs <- function(input_data){
  input_data$model_start_ymdhms +
    lubridate::years(input_data$simulation_steps_ymwd[1]) * input_data$nbr_steps_start_to_end +
    months(input_data$simulation_steps_ymwd[2]) * input_data$nbr_steps_start_to_end +
    lubridate::weeks(input_data$simulation_steps_ymwd[3]) * input_data$nbr_steps_start_to_end +
    lubridate::days(input_data$simulation_steps_ymwd[4]) * input_data$nbr_steps_start_to_end
}

#' @title add_dynamic_sp_vars_to_sf
#' @description FUNCTION_DESCRIPTION
#' @param dynamic_sp_vars_sf PARAM_DESCRIPTION
#' @param pop_attr_sf PARAM_DESCRIPTION
#' @param age_sex_pop_resolution PARAM_DESCRIPTION
#' @param age_sex_var_name PARAM_DESCRIPTION
#' @param popl_var_prefix PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[rlang]{sym}}
#' @rdname add_dynamic_sp_vars_to_sf
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang sym
add_dynamic_sp_vars_to_sf <- function(dynamic_sp_vars_sf,
                                      pop_attr_sf,
                                      age_sex_pop_resolution,
                                      age_sex_var_name,
                                      popl_var_prefix,
                                      data_year,
                                      crs_nbr_vec){
  profiled_sf <- intersect_sfs_keep_counts(profiled_sf = dynamic_sp_vars_sf,
                                           profiled_colref = NA,
                                           profiled_rowref = NA,
                                           attribute_sf = pop_attr_sf,
                                           attribute_unit = age_sex_pop_resolution,
                                           data_type = "processed_age_sex",
                                           data_year = data_year,
                                           popl_var_prefix = popl_var_prefix,
                                           crs_nbr_vec = crs_nbr_vec)
  dyn_par_unit_id <- names(dynamic_sp_vars_sf)[1] # Should be read from lookup
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(age_sex_var_name) := paste0(dyn_par_unit_id,"_",!!rlang::sym(age_sex_var_name)))
  update_pop_count_by_areas(profiled_sf = profiled_sf,
                            group_by_var = group_by_var,
                            age_sex_var_name = age_sex_var_name,
                            data_year = data_year,
                            age_sex_pop_resolution = age_sex_pop_resolution,
                            tot_pop_resolution = NULL,
                            popl_var_prefix = popl_var_prefix)

}

#' @title simplify_sf
#' @description FUNCTION_DESCRIPTION
#' @param sf PARAM_DESCRIPTION
#' @param crs PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_crs}},\code{\link[sf]{st_geometry_type}},\code{\link[sf]{st_collection_extract}},\code{\link[sf]{st_transform}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#'  \code{\link[geojsonio]{geojson_json}},\code{\link[geojsonio]{geojson_sf}}
#'  \code{\link[rmapshaper]{ms_simplify}}
#' @rdname simplify_sf
#' @export
#' @importFrom sf st_crs st_geometry_type st_collection_extract st_transform
#' @importFrom dplyr filter select
#' @importFrom geojsonio geojson_json geojson_sf
#' @importFrom rmapshaper ms_simplify
simplify_sf <- function(sf,
                        crs = NULL){ ## NOTE: CURRENTLY CREATES POLYGON WITH NA VALUE FEATURES TO FILL GAPS LEFT BY REMOVED LINESTRINGS
  if(is.null(crs))
    crs <- sf::st_crs(sf)[[1]]
  sf_poly <- sf %>% dplyr::filter(sf::st_geometry_type(.) %in% c("POLYGON","MULTIPOLYGON"))
  sf_other <- sf %>%
    dplyr::filter(!sf::st_geometry_type(.) %in% c("POLYGON","MULTIPOLYGON"))
  if(nrow(sf_other)!=0){
    sf_other <- sf_other %>%
      sf::st_collection_extract()
    sf_poly <- rbind(sf_poly,sf_other)
  }
  sf_poly_json <- geojsonio::geojson_json(sf_poly, geometry = "polygon", type = "auto")
  simple_poly_json <- rmapshaper::ms_simplify(sf_poly_json)
  sf_poly <- geojsonio::geojson_sf(simple_poly_json) %>%
    dplyr::select(-rmapshaperid) %>%
    sf::st_transform(crs = crs)
  }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{dots_values}}
#'  \code{\link[sf]{sfc}},\code{\link[sf]{st_geometry}},\code{\link[sf]{sf}}
#'  \code{\link[dplyr]{bind}}
#' @rdname from_web_bind_rows_sf
#' @export
#' @importFrom rlang dots_values
#' @importFrom sf st_sfc st_set_geometry st_sf
#' @importFrom dplyr bind_rows
from_web_bind_rows_sf <- function(...){ #https://github.com/r-spatial/sf/issues/49
  sf_list <- rlang::dots_values(...)[[1]]
  sfg_list_column <- lapply(sf_list, function(sf) sf$geometry[[1]]) %>% sf::st_sfc()
  df <- lapply(sf_list, function(sf) sf::st_set_geometry(sf, NULL)) %>% dplyr::bind_rows()
  sf_appended <- sf::st_sf(data.frame(df, geom=sfg_list_column))
  return(sf_appended)
}
#' @title drop_grouped_popl_vars
#' @description FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param popl_var_prefix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}}
#' @rdname drop_grouped_popl_vars
#' @export
#' @importFrom dplyr select
drop_grouped_popl_vars <- function(profiled_sf,
                                   popl_var_prefix){
  var_names_vec <- profiled_sf %>% names()
  keep_vars_vec <- var_names_vec[!var_names_vec  %>% startsWith("whl_") & !var_names_vec  %>% startsWith("grp_by_") & !var_names_vec  %>% startsWith("dupl_")]
  keep_vars_vec <- keep_vars_vec[!keep_vars_vec %>% startsWith("inc_") | keep_vars_vec %>% startsWith(popl_var_prefix)]
  dplyr::select(profiled_sf,
                keep_vars_vec)

}
#' @title get_group_by_var
#' @description FUNCTION_DESCRIPTION
#' @param profile_unit PARAM_DESCRIPTION
#' @param data_unit PARAM_DESCRIPTION
#' @param group_at_profile_unit PARAM_DESCRIPTION, Default: TRUE
#' @param group_by_lookup_tb PARAM_DESCRIPTION
#' @param area_bound_year PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[dplyr]{filter}}
#' @rdname get_group_by_var
#' @export
#' @importFrom ready4utils data_get
#' @importFrom dplyr filter
get_group_by_var <- function(profile_unit,
                             data_unit,
                             group_at_profile_unit = TRUE,
                             group_by_lookup_tb,
                             area_bound_year){ ### REPLACE ?????
  group_by <- ifelse(group_at_profile_unit,
                     ready4utils::data_get(data_lookup_tb = group_by_lookup_tb %>% dplyr::filter(spatial_unit == profile_unit) %>%
                                             dplyr::filter(as.numeric(year)==area_bound_year),
                                          lookup_variable = "spatial_unit",
                                          lookup_reference = profile_unit,
                                          target_variable = "var_name",
                                          evaluate = FALSE),
                     ready4utils::data_get(data_lookup_tb = group_by_lookup_tb,
                                          lookup_variable = "spatial_unit",
                                          lookup_reference = data_unit,
                                          target_variable = "var_name",
                                          evaluate = FALSE))
  return(group_by)
}

#' @title get_group_by_var_from_pai
#' @description FUNCTION_DESCRIPTION
#' @param profiled_area_input PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4s4]{sp_uid_lup}},\code{\link[ready4s4]{lookup_tb}},\code{\link[ready4s4]{data_year}},\code{\link[ready4s4]{use_coord_lup}},\code{\link[ready4s4]{area_type}},\code{\link[ready4s4]{geom_dist_limit_km}}
#'  \code{\link[dplyr]{filter}}
#' @rdname get_group_by_var_from_pai
#' @export
#' @importFrom ready4s4 sp_uid_lup lookup_tb data_year use_coord_lup area_type geom_dist_limit_km
#' @importFrom dplyr filter
get_group_by_var_from_pai <- function(profiled_area_input){
  group_by_lookup_tb = ready4s4::sp_uid_lup(profiled_area_input %>% ready4s4::lookup_tb())
  #%>%
    #dplyr::filter(year %in% c(ready4s4::data_year(profiled_area_input),"All"))
  if(!ready4s4::use_coord_lup(profiled_area_input)){
    group_by_var <- get_group_by_var(profile_unit = profiled_area_input %>% ready4s4::area_type(),
                                     group_by_lookup_tb = group_by_lookup_tb,
                                     area_bound_year = ready4s4::area_bound_year(profiled_area_input))
  }else{
    # group_by_var <- "service_name"
    if(is.na(ready4s4::geom_dist_limit_km(profiled_area_input)))
      group_by_var <- "drive_times"
        # get_group_by_var(profile_unit = "DRIVE_TIME",
        #                                group_by_lookup_tb = group_by_lookup_tb) ## MAY NEED REPLACING
    else
      group_by_var <- "distance_km"
    get_group_by_var(profile_unit = "GEOMETRIC_DISTANCE",
                                   group_by_lookup_tb = group_by_lookup_tb) ## MAY NEED REPLACING
  }
  return(group_by_var)
}


#' @title get_starter_sf_for_profiled_area
#' @description FUNCTION_DESCRIPTION
#' @param profiled_area_input PARAM_DESCRIPTION
#' @param group_by_var PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4s4]{lookup_tb}},\code{\link[ready4s4]{sp_starter_sf_lup}},\code{\link[ready4s4]{country}},\code{\link[ready4s4]{area_type}},\code{\link[ready4s4]{use_coord_lup}},\code{\link[ready4s4]{crs_nbr}},\code{\link[ready4s4]{features}}
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[sf]{st_crs<-}}
#'  \code{\link[rlang]{sym}}
#' @rdname get_starter_sf_for_profiled_area
#' @export
#' @importFrom ready4s4 lookup_tb sp_starter_sf_lup country area_type use_coord_lup crs_nbr features
#' @importFrom dplyr filter
#' @importFrom ready4utils data_get
#' @importFrom sf st_crs<-
#' @importFrom rlang sym
get_starter_sf_for_profiled_area <- function(profiled_area_input,
                                             group_by_var){
  sp_data_starter_sf_lup <- profiled_area_input %>%
    ready4s4::lookup_tb() %>%
    ready4s4::sp_starter_sf_lup() %>%
    dplyr::filter(country == ready4s4::country(profiled_area_input))
  starter_sf_nm <- ready4utils::data_get(data_lookup_tb = sp_data_starter_sf_lup,
                                     lookup_variable = "area_type",
                                     lookup_reference = ifelse(ready4s4::area_type(profiled_area_input) %in% sp_data_starter_sf_lup$area_type,
                                                               ready4s4::area_type(profiled_area_input),
                                                               "STE"#"PNT"
                                                               ),
                                     target_variable = "starter_sf",
                                     evaluate = FALSE)
    starter_sf <-  ready4utils::data_get(data_lookup_tb = profiled_area_input %>%
                            ready4s4::lookup_tb() %>%
                            ready4s4::sp_data_pack_lup(),
                          lookup_variable = "name",
                          lookup_reference = starter_sf_nm %>% stringr::str_sub(end=-4),
                          target_variable = "source_reference",
                          evaluate = FALSE) %>%
    parse(file="",n=NULL,text = .) %>%
    eval()
  if(ready4s4::use_coord_lup(profiled_area_input))
    starter_sf <- starter_sf %>%
      sf::`st_crs<-`(ready4s4::crs_nbr(profiled_area_input)[1])
  else
    starter_sf <-  starter_sf %>%
      dplyr::filter(!!rlang::sym(group_by_var) %in% ready4s4::features(profiled_area_input))
    return(starter_sf)
}

#' @title subset_sf_by_feature
#' @description FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param group_by_var PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{pull}},\code{\link[dplyr]{filter}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[stats]{setNames}}
#' @rdname subset_sf_by_feature
#' @export
#' @importFrom purrr map
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @importFrom stats setNames
subset_sf_by_feature <- function(profiled_sf,
                                 group_by_var){
  purrr::map(profiled_sf %>%
               dplyr::pull(!!rlang::sym(group_by_var)) %>%
               unique(),
             ~ profiled_sf %>%
               dplyr::filter(!!rlang::sym(group_by_var) == .x)) %>%
    stats::setNames(profiled_sf %>%
                      dplyr::pull(!!rlang::sym(group_by_var)) %>%
                      unique())
}

#' sum_at_diff_funs
#' FUNCTION_DESCRIPTION
#' @param data_sf PARAM_DESCRIPTION
#' @param var_list PARAM_DESCRIPTION
#' @param funs_list PARAM_DESCRIPTION
#' @param group_by PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map2}},\code{\link[purrr]{reduce}}
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[sf]{st_join}}
#' @rdname sum_at_diff_funs
#' @export
#' @importFrom purrr map2 reduce
#' @importFrom dplyr group_by summarise_at select one_of rename
#' @importFrom rlang sym
#' @importFrom sf st_join
sum_at_diff_funs <- function(data_sf,
                             var_list,
                             funs_list,
                             group_by){
  ## https://github.com/tidyverse/dplyr/issues/3101
  purrr::map2(var_list,
              funs_list,
              ~ data_sf %>%
                dplyr::group_by(!!rlang::sym(group_by)) %>%
                dplyr::summarise_at(.x, .y)) %>%
    #purrr::reduce(dplyr::inner_join)
    purrr::reduce(sf::st_join) %>%
    dplyr::select(-dplyr::one_of(paste0(group_by,".y"))) %>%
    dplyr::rename(!!rlang::sym(group_by) := paste0(group_by,".x"))
}

#' @title simplify_geoms_in_lup
#' @description FUNCTION_DESCRIPTION
#' @param lup_r4 PARAM_DESCRIPTION
#' @param r_data_dir PARAM_DESCRIPTION
#' @param crs_nbr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4s4]{sp_data_pack_lup}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[purrr]{map}}
#' @rdname simplify_geoms_in_lup
#' @export
#' @importFrom ready4s4 sp_data_pack_lup
#' @importFrom dplyr filter pull
#' @importFrom purrr walk
simplify_geoms_in_lup <- function(lup_r4,
                                  r_data_dir,
                                  crs_nbr){
  lup_r4 %>%
    ready4s4::sp_data_pack_lup() %>%
    dplyr::filter(main_feature =="Boundary") %>%
    dplyr::pull(source_reference) %>%
    purrr::walk(~ readRDS(paste0(r_data_dir,"/",.x,".rds")) %>%
                  simplify_sf(crs = crs_nbr[1]) %>%
                  saveRDS(paste0(r_data_dir,"/",.x,".rds")))
}
