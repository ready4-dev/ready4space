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

make_sim_data_env <- function(input_data,
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
  lookup_tb_r4

                              #group_by_lookup_tb = group_by_var_lookup_tb
                              ){
  #group_by_lookup_tb = ready.s4::sp_data_uid_lup(lookup_tb_r4)

  ## 1. CONVERT DATES
  data_year <- get_data_year_chr(input_data$data_ymdhm)
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
  # profiled_area_objs_ls <- make_profiled_area_objs(profiled_area_type = input_data$profiled_area_type,
  #                                                  profiled_area = input_data$profiled_area,
  #                                                  crs_nbr = input_data$crs_nbr,
  #                                                  distance_km = input_data$distance_km,
  #                                                  nbr_distance_steps = input_data$nbr_distance_steps,
  #                                                  travel_time_mins = input_data$travel_time_mins,
  #                                                  nbr_time_steps = input_data$nbr_time_steps,
  #                                                  group_by_lookup_tb = group_by_lookup_tb)
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
                                    state_territory = profiled_area_objs_ls$state_territory,
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
                             group_by_lookup_tb){
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

make_profiled_area_objs <- function(profiled_area_type,
                                    profiled_area,
                                    crs_nbr,
                                    #headspace_tb = example_headspace_tb,#ymh.headspace::headspace_tb,#tibble::tibble(service_name = c("Glenroy", "Sunshine", "Craigieburn","Werribee"),
                                                    #              lat = c(-37.704890, -37.783314, -37.593766, -37.901473),
                                                    #              long = c(144.918099,144.831070,144.914055,144.662196)),
                                    distance_km = NULL,
                                    nbr_distance_steps,
                                    travel_time_mins = NULL,
                                    nbr_time_steps,
                                    data_year,
                                    lookup_tb_r4
                                    #group_by_lookup_tb
                                    ){
## BAND BY var_name ....
  group_by_lookup_tb = ready.s4::sp_data_uid_lup(lookup_tb_r4) %>%
    dplyr::filter(year == data_year)
  if(profiled_area_type=="PHN"){
    group_by_var <- get_group_by_var(profile_unit = "PHN",
                                     group_by_lookup_tb = group_by_lookup_tb)
    profiled_sf <- ready.aus.data::aus_boundary_phns_sf %>%
      dplyr::filter(!!rlang::sym(group_by_var) %in% profiled_area)
    profiled_area_bands_list <- subset_sf_by_feature(profiled_sf = profiled_sf,
                                                     group_by_var = group_by_var)
    state_territory <- profiled_sf %>%
      dplyr::pull("FIRST_STE1") %>%
      as.character() %>%
      unique()
  }else{
    aus_stt_sf <- ready.pp.phn::aus_stt_sf %>%
      sf::`st_crs<-`(crs_nbr)
    if(profiled_area_type == "Headspace"){
      cluster_tb =  lookup_tb_r4 %>% ready.s4::sp_site_coord_lup()  %>%
        #example_headspace_tb %>% #headspace_tb %>%
        dplyr::filter(service_name %in% profiled_area)
    }
    if(profiled_area_type == "Custom"){
      cluster_tb =  tibble::tibble(service_type = profiled_area$cluster_vec,
                                   cluster_name = profiled_area$cluster_vec,
                                   service_name = profiled_area$service_vec,
                                   lat = profiled_area$lat_vec,
                                   long = profiled_area$lon_vec)
    }
    if(!is.null(distance_km)){
      profiled_sf <- gen_distance_based_bands(distance_km_outer = distance_km, # *1000
                                                   nbr_distance_bands = nbr_distance_steps,
                                                   service_cluster_tb = cluster_tb,
                                                   aus_stt_sf = aus_stt_sf)[[1]]
      profiled_area_bands_list <- subset_sf_by_feature(profiled_sf = profiled_sf,
                                                       group_by_var = get_group_by_var(profile_unit = "GEOMETRIC_DISTANCE",
                                                                                       group_by_lookup_tb = group_by_lookup_tb))

      #names(profiled_area_bands_list) <- paste0("km_band_",1:length(profiled_area_bands_list))
    }
    if(!is.null(travel_time_mins)){
      profiled_area_bands_list <- cluster_isochrones(cluster_tbs_list = list(cluster_tb),
                                                  look_up_ref = 1,
                                                  time_min = 0,
                                                  time_max = travel_time_mins,
                                                  nbr_time_steps = nbr_time_steps)
      names(profiled_area_bands_list) <- paste0("dt_band_",1:length(profiled_area_bands_list))
      profiled_sf <- do.call(rbind,profiled_area_bands_list) %>%
        sf::st_transform(crs_nbr)
    }
    state_territory <- sf::st_intersection(aus_stt_sf,
                                           profiled_sf) %>%
      dplyr::pull(FIRST_STE1) %>%
      as.vector()%>%
      unique()
  }
  return(list(state_territory = state_territory,
              profiled_sf = profiled_sf,
              profiled_area_bands_list = profiled_area_bands_list))
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
