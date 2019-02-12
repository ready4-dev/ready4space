#' make_sim_data_env
#' Make a simulation data input for the environment.
#' @param profiled_area_type PARAM_DESCRIPTION
#' @param profiled_area PARAM_DESCRIPTION
#' @param age_lower PARAM_DESCRIPTION
#' @param age_upper PARAM_DESCRIPTION
#' @param age_upper PARAM_DESCRIPTION
#' @param env_str_par_tb PARAM_DESCRIPTION
#' @param nbr_its PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @export

make_sim_data_env <- function(profiled_area_type,
                              profiled_area,
                              age_lower,
                              age_upper,
                              project_for_year,
                              env_str_par_tb,
                              nbr_its,
                              deterministic){
  #
  ## 1. GET PARAMETER MATRICES
  par_str_list <- ready.sim::instantiate_env_struc_par_all(env_str_par_tb)
  env_param_tb  <- purrr::map_dfr(1:length(par_str_list),
                                  ~ genValueFromDist(par_str_list[[.x]], nbr_its))
  ## 2. DEFINE PROFILED AREA AND INCLUDED STATEs / TERRITORIES
  if(profiled_area_type=="PHN"){
    profiled_area_sf <- ready.aus.data::aus_boundary_phns_sf %>%
      dplyr::filter(PHN_NAME %in% profiled_area)
    state_territory <- profiled_area_sf %>%
      dplyr::pull("FIRST_STE1") %>% 
      as.character() %>%
      unique()
  }else{
    aus_stt_sf <- ready.aus.phn::aus_phn_nat_shp_bound_2017 %>%
      dplyr::group_by(FIRST_STE1) %>%
      dplyr::summarise(FIRST_STE_ = dplyr::first(FIRST_STE_),
                       SUM_AREASQ = sum(SUM_AREASQ))
    if(profiled_area == "Service cluster - Orygen headspaces"){
      orygen_headspace_cluster_tb =  tibble::tibble(lat = c(-37.704890, -37.783314, -37.593766, -37.901473),
                                                    long = c(144.918099,144.831070,144.914055,144.662196))
      profiled_area_sf <- ready.space::spatial_area_within_xkm_of_points(point_locations = orygen_headspace_cluster_tb,
                                                                         land_sf = aus_stt_sf ,
                                                                         distance = 10000)
    }
    state_territory <- sf::st_intersection(aus_stt_sf,
                                           profiled_area_sf) %>%
      dplyr::pull(FIRST_STE1) %>%
      as.vector()%>%
      unique()
  }
  ## 3. GET SPATIAL DATA FOR INCLUDED STATES / TERRITORIES
  at_highest_res = c("ERP by age and sex",
                     "ERP")
  to_time = "2016"
  if("Victoria" %in% state_territory){
    at_highest_res <- c(at_highest_res,"Population projections")
    to_time = "2031"
  }
  lists_to_merge <- purrr::map(state_territory,
                               ~ get_spatial_data_list(at_highest_res = at_highest_res,
                                                       at_time = "2016",
                                                       to_time = to_time,
                                                       at_specified_res = list(a=c("SEIFA","SA2")),
                                                       country = "Australia",
                                                       state = .x,
                                                       require_year_match = FALSE,
                                                       excl_diff_bound_yr = TRUE))
  lists_to_merge <- purrr::transpose(lists_to_merge)
  merged_list <- purrr::map(lists_to_merge[2:length(lists_to_merge)],
                            ~ do.call(rbind,.x))
  names_ppr <- purrr::map_chr(lists_to_merge[[1]],
                              ~ ifelse(length(.x[1])==0,NA_character_,names(.x[1]))) 
  ppr_ref <- purrr::map_dbl(lists_to_merge[[1]],
                            ~ ifelse(length(.x[1])==0,NA_real_,.x[1])) %>%
    stats::setNames(names_ppr)
  sp_data_list <- purrr::prepend(merged_list,list(ppr_ref))
  # sp_data_list <- get_spatial_data_list(at_highest_res = at_highest_res,
  #                                       at_time = "2016",
  #                                       to_time = to_time,
  #                                       at_specified_res = list(a=c("SEIFA","SA2")),
  #                                       country = "Australia",
  #                                       state = state_territory,
  #                                       require_year_match = FALSE,
  #                                       excl_diff_bound_yr = TRUE)
  ## 3. CHOOSE PROFILED AREA
  # phns_in_state <- ready.aus.data::aus_boundary_phns_sf %>%
  #   dplyr::filter(FIRST_STE1 == state_territory)
  # land_boundary_sf <- phns_in_state %>%
  #   sf::st_union()
  # if(profiled_area %in% (phns_in_state %>%
  #    dplyr::pull("PHN_NAME") %>% as.character())){
  #   profiled_area_sf <- phns_in_state %>%
  #     dplyr::filter(PHN_NAME == profiled_area)
  # }
  # if(profiled_area == "Service cluster - Orygen headspaces"){
  #   orygen_headspace_cluster_tb =  tibble::tibble(lat = c(-37.704890, -37.783314, -37.593766, -37.901473),
  #                                                 long = c(144.918099,144.831070,144.914055,144.662196))
  #   profiled_area_sf <- spatial_area_within_xkm_of_points(point_locations = orygen_headspace_cluster_tb,
  #                                                         land_sf = land_boundary_sf ,
  #                                                         distance = 10000)
  # }
  ## 4. APPLY PROFILED AREA FILTER
  sp_data_list[[2]] <- spatial_profile_by_resolution_and_update_counts(profiled_sf = profiled_area_sf,
                                                                       resolution_sf = sp_data_list[[2]],
                                                                       resolution_sa1s_sf = sp_data_list[[4]],
                                                                       resolution_sa2s_sf = sp_data_list[[2]],
                                                                       return_resolution = "SA2")
  
  ## 5. CREATE SPATIO-TEMPORAL INPUT DATA OBJECT
  st_envir <- ready.sim::ready_env(st_data = sp_data_list,
                                   par_vals = env_param_tb)
  ## 6. CREATE SIMULATION DATA INPUT OBJECT
  sim_data <- ready.sim::ready_sim_data(st_envir = st_envir,
                                        pre_model_date = "2016",
                                        model_start_date = "2019",
                                        model_end_date = to_time,
                                        age_lower = age_lower,
                                        age_upper = age_upper,
                                        time_steps = c(1,0,0,0),
                                        nbr_steps = project_for_year-2016)
  return(sim_data)
}
