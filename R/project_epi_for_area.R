#' project_epi_for_area
#' Project epidemiology for area at specified point in time
#' @param state_territory PARAM_DESCRIPTION, Default: 'Victoria'
#' @param profiled_area PARAM_DESCRIPTION, Default: 'Service cluster - Orygen headspaces'
#' @param age_lower PARAM_DESCRIPTION, Default: 12
#' @param age_upper PARAM_DESCRIPTION, Default: 18
#' @param project_for_year PARAM_DESCRIPTION, Default: 2023
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS


project_epi_for_area <- function(state_territory = "Victoria",
                                      profiled_area = "Service cluster - Orygen headspaces",
                                      age_lower = 12,
                                      age_upper = 18,
                                      project_for_year = 2023){
  #
  ## 1. GET PARAMETER MATRICES
  env_param_tb <- make_env_param_tb(nbr_its = 5,
                                    env_str_par_tb = ready.agents::par_str_environment_tb,
                                    mape_str_par_tb = ready.agents::params_struc_mape_tb,
                                    jt_dist = FALSE)
  ## Can use below to eliminate uncertainty from population predictions:
  env_param_tb <- env_param_tb %>%
    dplyr::mutate_if(is.numeric,dplyr::funs(ifelse(param_name=="pop_pe_sign",0,.)))
  class(env_param_tb) <- class(env_param_tb)[2:4]
  ## 2. GET SPATIAL DATA
  at_highest_res = c("ERP by age and sex",
                     "ERP")
  to_time = "2016"
  if(state_territory == "Victoria"){
    at_highest_res <- c(at_highest_res,"Population projections")
    to_time = "2031"
  }
  sp_data_list <- get_spatial_data_list(at_highest_res = at_highest_res,
                                        at_time = "2016",
                                        to_time = to_time,
                                        at_specified_res = list(a=c("SEIFA","SA2")),
                                        country = "Australia",
                                        state = state_territory,
                                        require_year_match = FALSE,
                                        excl_diff_bound_yr = TRUE)
  ## 3. CHOOSE PROFILED AREA
  phns_in_state <- ready.aus.data::aus_boundary_phns_sf %>%
    dplyr::filter(FIRST_STE1 == state_territory)
  land_boundary_sf <- phns_in_state %>%
    sf::st_union()
  if(profiled_area %in% (phns_in_state %>%
     dplyr::pull("PHN_NAME") %>% as.character())){
    profiled_area_sf <- phns_in_state %>%
      dplyr::filter(PHN_NAME == profiled_area)
  }
  if(profiled_area == "Service cluster - Orygen headspaces"){
    orygen_headspace_cluster_tb =  tibble::tibble(lat = c(-37.704890, -37.783314, -37.593766, -37.901473),
                                                  long = c(144.918099,144.831070,144.914055,144.662196))
    profiled_area_sf <- spatial_area_within_xkm_of_points(point_locations = orygen_headspace_cluster_tb,
                                                          land_sf = land_boundary_sf ,
                                                          distance = 10000)
  }
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
  ## 7. APPLY EPIDEMIOLOGICAL MODELS
  ##
  ## 8. SIMULATE
  sim_data <- ready.sim::runSimulation(sim_data)
  ready.sim::env_sf(ready.sim::st_envir(sim_data))
  ## 9 REPORT RESULTS

  ## 10. PLOT RESULTS

  ##
  return(sim_data)
}
