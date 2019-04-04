#' make_sim_data_env
#' Make a simulation data input for the environment.
#' @param profiled_area_type PARAM_DESCRIPTION
#' @param distance_km PARAM_DESCRIPTION
#' @param travel_time_mins PARAM_DESCRIPTION
#' @param profiled_area PARAM_DESCRIPTION
#' @param age_lower PARAM_DESCRIPTION
#' @param age_upper PARAM_DESCRIPTION
#' @param age_upper PARAM_DESCRIPTION
#' @param env_str_par_tb PARAM_DESCRIPTION
#' @param nbr_its PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @export

make_sim_data_env <- function(input_data#,
  # profiled_area_type,
  #                             profiled_area,
  #                             distance_km = NULL,
  #                             nbr_distance_steps = 5,
  #                             nbr_time_steps = 5,
  #                             travel_time_mins = NULL,
  #                             age_lower,
  #                             age_upper,
  #                             env_str_par_tb,
  #                             nbr_its,
  #                             deterministic,
  #                             group_at_profile_unit = TRUE,
  #                             data_ymdhms = lubridate::ymd_hms("2016-07-01 12:00:00"),
  #                             model_start_ymdhms = lubridate::ymd_hms("2019_07_01 12:00:00"),
  #                             simulation_steps_ymwd = c(1,0,0,0),
  #                             nbr_steps_start_to_end = 1,
  #                             at_highest_res_extra = NULL,
  #                             at_specified_res = list(a=c("SEIFA","SA2")),
  #                             age_sex_pop_str = "ERP by age and sex",
  #                             tot_pop_str = "ERP",
  #                             pop_projs_str = "Population projections",
  #                             country = "Australia",
  #                             crs_nbr = 4283,
  #lookup_tb_r4

                              #group_by_lookup_tb = group_by_var_lookup_tb
                              ){
  #group_by_lookup_tb = ready.s4::sp_data_uid_lup(lookup_tb_r4)

  ## 1. CONVERT DATES
  data_year <- ready.s4::data_year(input_data$profiled_area_input)#get_data_year_chr(input_data$data_ymdhm)
  model_end_ymdhms <- input_data$model_start_ymdhms +
    lubridate::years(input_data$simulation_steps_ymwd[1]) * input_data$nbr_steps_start_to_end +
    months(input_data$simulation_steps_ymwd[2]) * input_data$nbr_steps_start_to_end +
    lubridate::weeks(input_data$simulation_steps_ymwd[3]) * input_data$nbr_steps_start_to_end +
    lubridate::days(input_data$simulation_steps_ymwd[4]) * input_data$nbr_steps_start_to_end #lubridate::ymd_hms("2031_07_01 12:00:00")
  model_end_year <- model_end_ymdhms %>% lubridate::year() %>% as.character()
  ## 2. GET PARAMETER MATRICES
  par_str_list <- ready.sim::instantiate_env_struc_par_all(input_data$env_str_par_tb)
  env_param_tb  <- purrr::map_dfr(1:length(par_str_list),
                                  ~ ready.sim::genValueFromDist(par_str_list[[.x]], input_data$nbr_its))
  ## 3. DEFINE PROFILED AREA AND INCLUDED STATEs / TERRITORIES
  # group_by_lookup_tb <- ready.s4::sp_data_uid_lup(lookup_tb_r4) %>%
  #   dplyr::filter(year == data_year)
  profiled_area_objs_ls <- make_profiled_area_objs(profiled_area_input = input_data$profiled_area_input)
  ## 4. GET SPATIAL DATA FOR INCLUDED STATES / TERRITORIES
  # at_highest_res <- c(input_data$age_sex_pop_str,
  #                     input_data$tot_pop_str,
  #                     input_data$pop_projs_str) %>% purrr::compact()
  # if(!is.null(input_data$at_highest_res_extra))
  #   at_highest_res <- c(at_highest_res,input_data$at_highest_res_extra)
  sp_data_list <- make_sp_data_list(at_highest_res = input_data$at_highest_res,
                                    at_specified_res = input_data$at_specified_res,
                                    data_year = data_year,
                                    to_time = model_end_year,
                                    country = input_data$country,
                                    state_territory = profiled_area_objs_ls$state_territory, ### CHANGE VAR NAME
                                    pop_projs_str = input_data$pop_projs_str)

  ## 5. APPLY PROFILED AREA FILTER
  sp_data_list <- extend_sp_data_list(sp_data_list = sp_data_list,
                                      profiled_area_type = input_data$profiled_area_type,
                                      age_sex_pop_str = input_data$age_sex_pop_str,
                                      tot_pop_str = input_data$tot_pop_str,
                                      at_highest_res = input_data$at_highest_res,
                                      distance_km = input_data$distance_km,
                                      travel_time_mins = input_data$travel_time_mins,
                                      profiled_area_bands_list = profiled_area_objs_ls$profiled_area_bands_list,
                                      group_at_profile_unit = input_data$group_at_profile_unit,
                                      group_by_lookup_tb = group_by_lookup_tb,
                                      crs_nbr = input_data$crs_nbr,
                                      data_year = data_year)

  ## 6. REFORMAT LIST
  sp_data_list <- list(input_bl_profiled_sf = sp_data_list[names(sp_data_list)[!names(sp_data_list)
                                                                               %in% c("ppr_ref",
                                                                                      names(sp_data_list)[sp_data_list$ppr_ref],
                                                                                      "profiled_sf")]],
                       input_dynamic_sp_pars = sp_data_list[[sp_data_list$ppr_ref]],
                       profiled_sf = sp_data_list$profiled_sf,
                       popl_var_prefix = sp_data_list$popl_var_prefix)
  ## 7. CREATE SPATIO-TEMPORAL INPUT DATA OBJECT
  st_envir <- ready.sim::ready_env(st_data = sp_data_list,
                                   par_vals = env_param_tb)
  ## 8. CREATE SIMULATION DATA INPUT OBJECT
  sim_data <- ready.sim::ready_sim_data(st_envir = st_envir,
                                        pre_model_date = input_data$data_ymdhms,
                                        model_start_date = input_data$model_start_ymdhms,# %>%
                                          #lubridate::year() %>%
                                          #as.character(),
                                        model_end_date = model_end_ymdhms,# model_end_year,
                                        age_lower = input_data$age_lower,
                                        age_upper = input_data$age_upper,
                                        time_steps = input_data$simulation_steps_ymwd,
                                        nbr_steps = input_data$nbr_steps_start_to_end)
  return(sim_data)
}
get_data_year_chr <- function(data_ymdhms){
  data_ymdhms %>%
    lubridate::year() %>%
    as.character()
}
extend_sp_data_list <- function(sp_data_list,
                                profiled_area_type,
                                age_sex_pop_str,
                                tot_pop_str,
                                at_highest_res,
                                distance_km = NULL,
                                travel_time_mins = NULL,
                                profiled_area_bands_list,
                                group_at_profile_unit,
                                group_by_lookup_tb,
                                crs_nbr,
                                data_year
                                ){
  age_sex_pop_resolution <- names(sp_data_list)[which(at_highest_res == age_sex_pop_str) + 1]
  tot_pop_resolution <- NULL
  if(!is.null(tot_pop_str))
    tot_pop_resolution <- names(sp_data_list)[which(at_highest_res == tot_pop_str) + 1]
  group_by_var <- get_group_by_var(profile_unit = ifelse(profiled_area_type=="PHN",
                                                              "PHN",
                                                              ifelse(!is.null(travel_time_mins),"DRIVE_TIME","GEOMETRIC_DISTANCE")),
                                   data_unit = age_sex_pop_resolution,
                                   group_at_profile_unit = group_at_profile_unit,
                                   group_by_lookup_tb = group_by_lookup_tb)
  if(profiled_area_type != "PHN")
    profiled_area_bands_list <- purrr::map(profiled_area_bands_list,
                                           ~ .x %>%
                                             sf::st_transform(crs_nbr))
  by_band_pop_counts_sf_ls <- purrr::map(profiled_area_bands_list,
                                         ~ intersect_sfs_update_counts(profiled_sf = .x,
                                                                       profiled_colref = NA,
                                                                       profiled_rowref = NA,
                                                                       sp_data_list = sp_data_list,
                                                                       tot_pop_resolution = tot_pop_resolution,
                                                                       age_sex_pop_resolution = age_sex_pop_resolution,
                                                                       group_by_var = group_by_var,
                                                                       group_by_lookup_tb = group_by_lookup_tb,
                                                                       data_year = data_year))
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
                                         data_year = data_year)
  extended_sp_data_list <- append(sp_data_list,
                                  list(profiled_sf = profiled_sf,
                                       popl_var_prefix = popl_var_prefix))
  return(extended_sp_data_list)
}
get_group_by_var <- function(profile_unit,
                             data_unit,
                             group_at_profile_unit = TRUE,
                             group_by_lookup_tb){ ### REPLACE ?????
  group_by <- ifelse(group_at_profile_unit,
                     ready.data::data_get(data_lookup_tb = group_by_lookup_tb,
                                          lookup_variable = "spatial_unit",
                                          lookup_reference = profile_unit,
                                          target_variable = "var_name",
                                          evaluate = FALSE),
                     ready.data::data_get(data_lookup_tb = group_by_lookup_tb,
                                          lookup_variable = "spatial_unit",
                                          lookup_reference = data_unit,
                                          target_variable = "var_name",
                                          evaluate = FALSE))
  return(group_by)
}
get_group_by_var_from_pai <- function(profiled_area_input){
  group_by_lookup_tb = ready.s4::sp_uid_lup(profiled_area_input %>% ready.s4::lookup_tb()) %>%
    dplyr::filter(year %in% c(ready.s4::data_year(profiled_area_input),"All"))
  if(!ready.s4::use_coord_lup(profiled_area_input)){
    group_by_var <- get_group_by_var(profile_unit = profiled_area_input %>% ready.s4::area_type(),
                                     group_by_lookup_tb = group_by_lookup_tb)
  }else{
    if(is.na(ready.s4::geom_dist_limit_km(profiled_area_input)))
      group_by_var <- get_group_by_var(profile_unit = "DRIVE_TIME",
                                       group_by_lookup_tb = group_by_lookup_tb)
    else
      group_by_var <- get_group_by_var(profile_unit = "GEOMETRIC_DISTANCE",
                                       group_by_lookup_tb = group_by_lookup_tb)
  }
  return(group_by_var)
}
make_sp_data_list <- function(at_highest_res,
                              at_specified_res,
                              data_year,
                              to_time,
                              country,
                              state_territory,
                              pop_projs_str){
  if(!"Victoria" %in% state_territory){
    at_highest_res <- at_highest_res[at_highest_res != "Population projections"]
    to_time <- data_year
  }
  lists_to_merge <- purrr::map(state_territory,
                               ~ get_spatial_data_list(at_highest_res = at_highest_res,
                                                       data_year = data_year,
                                                       to_time = to_time,
                                                       at_specified_res = at_specified_res,
                                                       country = country,
                                                       state = .x,
                                                       require_year_match = FALSE,
                                                       excl_diff_bound_yr = TRUE,
                                                       pop_projs_str = pop_projs_str))
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

make_profiled_area_objs <- function(profiled_area_input){
  group_by_var <- get_group_by_var_from_pai(profiled_area_input = profiled_area_input)
  st_profiled_sf <- get_starter_sf_for_profiled_area(profiled_area_input = profiled_area_input,
                                                     group_by_var = group_by_var)
  main_sub_div_var <- ready.data::data_get(data_lookup_tb = profiled_area_input %>%
                                             ready.s4::lookup_tb() %>%
                                             ready.s4::sp_starter_sf_lup() %>%
                                             dplyr::filter(country == ready.s4::country(profiled_area_input)),
                                           lookup_variable = "area_type",
                                           lookup_reference = ready.s4::area_type(profiled_area_input),
                                           target_variable = "sf_main_sub_div",
                                           evaluate = FALSE)
  if(!ready.s4::use_coord_lup(profiled_area_input)){
    profiled_sf <- st_profiled_sf
    profiled_area_bands_list <- subset_sf_by_feature(profiled_sf = profiled_sf,
                                                     group_by_var = group_by_var)
    sub_div_units_vec <- profiled_sf %>%
      dplyr::pull(!!rlang::sym(main_sub_div_var)) %>%
      as.character() %>%
      unique()
  }else{
    cluster_tb = ready.s4::lookup_tb(profiled_area_input) %>%
      ready.s4::sp_site_coord_lup() %>%
      dplyr::filter(service_type %in% ready.s4::area_type(profiled_area_input))  %>%
      dplyr::filter(service_name %in% ready.s4::features(profiled_area_input))
    if(!is.na(ready.s4::geom_dist_limit_km(profiled_area_input))){
      profiled_sf <- gen_distance_based_bands(distance_km_outer = ready.s4::geom_dist_limit_km(profiled_area_input), # *1000
                                              nbr_distance_bands = ready.s4::nbr_bands(profiled_area_input),
                                              service_cluster_tb = cluster_tb,
                                              profiled_sf =  st_profiled_sf,
                                              crs_nbr = ready.s4::crs_nbr(profiled_area_input))[[1]]
      profiled_area_bands_list <- subset_sf_by_feature(profiled_sf = profiled_sf,
                                                       group_by_var = group_by_var)
    }
    if(!is.na(ready.s4::drive_time_limmit_mins(profiled_area_input))){
      profiled_area_bands_list <- cluster_isochrones(cluster_tbs_list = list(cluster_tb),
                                                     look_up_ref = 1,
                                                     time_min = 0,
                                                     time_max = ready.s4::drive_time_limmit_mins(profiled_area_input),
                                                     nbr_time_steps = ready.s4::nbr_bands(profiled_area_input))
      names(profiled_area_bands_list) <- paste0("dt_band_",1:length(profiled_area_bands_list))
      profiled_sf <- do.call(rbind,profiled_area_bands_list) %>%
        sf::st_transform(crs_nbr)
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

get_starter_sf_for_profiled_area <- function(profiled_area_input,
                                             group_by_var){
  sp_data_starter_sf_lup <- profiled_area_input %>%
    ready.s4::lookup_tb() %>%
    ready.s4::sp_starter_sf_lup() %>%
    dplyr::filter(country == ready.s4::country(profiled_area_input))
  starter_sf <- ready.data::data_get(data_lookup_tb = sp_data_starter_sf_lup,
                                     lookup_variable = "area_type",
                                     lookup_reference = ready.s4::area_type(profiled_area_input),
                                     target_variable = "starter_sf",
                                     evaluate = TRUE)
  if(ready.s4::use_coord_lup(profiled_area_input))
    starter_sf <- starter_sf %>%
      sf::`st_crs<-`(ready.s4::crs_nbr(profiled_area_input))
  else
    starter_sf <-  starter_sf %>%
      dplyr::filter(!!rlang::sym(group_by_var) %in% ready.s4::features(profiled_area_input))
}

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
#' @param data_tb PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{join}}
#'  \code{\link[rlang]{sym}}
#' @rdname sum_at_diff_funs
#' @export
#' @importFrom purrr map2 reduce
#' @importFrom dplyr group_by summarise_at inner_join
#' @importFrom rlang sym
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
