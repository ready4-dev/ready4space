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
#'  \code{\link[ready4s4]{geom_dist_limit_km}},\code{\link[ready4s4]{drive_time_limmit_mins}},\code{\link[ready4s4]{lookup_tb}},\code{\link[ready4s4]{sp_uid_lup}},\code{\link[ready4s4]{data_year}},\code{\link[ready4s4]{use_coord_lup}},\code{\link[ready4s4]{crs_nbr}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[ready4sd]{intersect_sfs_update_counts}},\code{\link[ready4sd]{get_popl_var_prefix}}
#'  \code{\link[sf]{st_transform}},\code{\link[sf]{geos_measures}}
#' @rdname extend_sp_data_list
#' @export
#' @importFrom ready4s4 geom_dist_limit_km drive_time_limmit_mins lookup_tb sp_uid_lup data_year use_coord_lup crs_nbr
#' @importFrom ready4utils data_get
#' @importFrom dplyr filter mutate
#' @importFrom purrr map map2
#' @importFrom ready4sd intersect_sfs_update_counts get_popl_var_prefix
#' @importFrom sf st_transform st_area
extend_sp_data_list <- function(sp_data_list,
                                input_data,
                                profiled_area_bands_list){
  at_highest_res = input_data$at_highest_res
  distance_km = ready4s4::geom_dist_limit_km(input_data$profiled_area_input)
  travel_time_mins = ready4s4::drive_time_limmit_mins(input_data$profiled_area_input)
  group_by_var <- get_group_by_var_from_pai(input_data$profiled_area_input)
  age_sex_pop_resolution <- names(sp_data_list)[which(at_highest_res == input_data$age_sex_pop_str) + 1]
  age_sex_counts_grouped_by <- ready4utils::data_get(data_lookup_tb = ready4s4::lookup_tb(input_data$profiled_area_input) %>%
                                                      ready4s4::sp_uid_lup() %>%
                                                      dplyr::filter(year %in% c(ready4s4::data_year(input_data$profiled_area_input),
                                                                                "All")),
                                                    lookup_variable = "spatial_unit",
                                                    lookup_reference = age_sex_pop_resolution,
                                                    target_variable = "var_name",
                                                    evaluate = FALSE)
  tot_pop_resolution <- NULL
  if(!is.null(input_data$tot_pop_str))
    tot_pop_resolution <- names(sp_data_list)[which(at_highest_res == input_data$tot_pop_str) + 1]
  if(ready4s4::use_coord_lup(input_data$profiled_area_input))
    profiled_area_bands_list <- purrr::map(profiled_area_bands_list,
                                           ~ .x %>%
                                             sf::st_transform(ready4s4::crs_nbr(input_data$profiled_area_input)[1]))
  by_band_pop_counts_sf_ls <- purrr::map(profiled_area_bands_list,
                                         ~ ready4sd::intersect_sfs_update_counts(profiled_sf = .x,
                                                                       profiled_colref = NA,
                                                                       profiled_rowref = NA,
                                                                       sp_data_list = sp_data_list,
                                                                       tot_pop_resolution = tot_pop_resolution,
                                                                       age_sex_pop_resolution = age_sex_pop_resolution,
                                                                       group_by_var = group_by_var,
                                                                       age_sex_counts_grouped_by = age_sex_counts_grouped_by,
                                                                       data_year = ready4s4::data_year(input_data$profiled_area_input)
                                         ))
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
  popl_var_prefix <- ready4sd::get_popl_var_prefix(age_sex_pop_resolution = age_sex_pop_resolution,
                                         tot_pop_resolution = tot_pop_resolution,
                                         data_year = ready4s4::data_year(input_data$profiled_area_input))
  extended_sp_data_list <- append(sp_data_list,
                                  list(profiled_sf = profiled_sf,
                                       popl_var_prefix = popl_var_prefix))
  return(extended_sp_data_list)
}
#' @title get_group_by_var
#' @description FUNCTION_DESCRIPTION
#' @param profile_unit PARAM_DESCRIPTION
#' @param data_unit PARAM_DESCRIPTION
#' @param group_at_profile_unit PARAM_DESCRIPTION, Default: TRUE
#' @param group_by_lookup_tb PARAM_DESCRIPTION
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
#' @rdname get_group_by_var
#' @export
#' @importFrom ready4utils data_get
get_group_by_var <- function(profile_unit,
                             data_unit,
                             group_at_profile_unit = TRUE,
                             group_by_lookup_tb){ ### REPLACE ?????
  group_by <- ifelse(group_at_profile_unit,
                     ready4utils::data_get(data_lookup_tb = group_by_lookup_tb,
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
get_group_by_var_from_pai <- function(profiled_area_input){
  group_by_lookup_tb = ready4s4::sp_uid_lup(profiled_area_input %>% ready4s4::lookup_tb()) %>%
    dplyr::filter(year %in% c(ready4s4::data_year(profiled_area_input),"All"))
  if(!ready4s4::use_coord_lup(profiled_area_input)){
    group_by_var <- get_group_by_var(profile_unit = profiled_area_input %>% ready4s4::area_type(),
                                     group_by_lookup_tb = group_by_lookup_tb)
  }else{
    if(is.na(ready4s4::geom_dist_limit_km(profiled_area_input)))
      group_by_var <- get_group_by_var(profile_unit = "DRIVE_TIME",
                                       group_by_lookup_tb = group_by_lookup_tb)
    else
      group_by_var <- get_group_by_var(profile_unit = "GEOMETRIC_DISTANCE",
                                       group_by_lookup_tb = group_by_lookup_tb)
  }
  return(group_by_var)
}
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
  main_sub_div_var <- ready4utils::data_get(data_lookup_tb = profiled_area_input %>%
                                             ready4s4::lookup_tb() %>%
                                             ready4s4::sp_starter_sf_lup() %>%
                                             dplyr::filter(country == ready4s4::country(profiled_area_input)),
                                           lookup_variable = "area_type",
                                           lookup_reference = ready4s4::area_type(profiled_area_input),
                                           target_variable = "sf_main_sub_div",
                                           evaluate = FALSE)
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
        sf::st_transform(ready4s4::crs_nbr(profiled_area_input)[1])
    }
    sub_div_units_vec <- sf::st_intersection(st_profiled_sf,
                                             profiled_sf) %>%
      dplyr::pull(!!rlang::sym(main_sub_div_var)) %>%
      as.vector()%>%
      unique()
  }
  return(list(sub_div_units_vec = sub_div_units_vec, # previously state_territory
              profiled_sf = profiled_sf,
              profiled_area_bands_list = profiled_area_bands_list))
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
  starter_sf <- ready4utils::data_get(data_lookup_tb = sp_data_starter_sf_lup,
                                     lookup_variable = "area_type",
                                     lookup_reference = ready4s4::area_type(profiled_area_input),
                                     target_variable = "starter_sf",
                                     evaluate = FALSE) %>% parse(file="",n=NULL,text = .) %>%
    eval()
  if(ready4s4::use_coord_lup(profiled_area_input))
    starter_sf <- starter_sf %>%
      sf::`st_crs<-`(ready4s4::crs_nbr(profiled_area_input)[1])
  else
    starter_sf <-  starter_sf %>%
      dplyr::filter(!!rlang::sym(group_by_var) %in% ready4s4::features(profiled_area_input))
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
