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

make_sim_data_env <- function(profiled_area_type,
                              profiled_area,
                              distance_km = NULL,
                              travel_time_mins = NULL,
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
    aus_stt_sf <- ready.pp.phn::aus_stt_sf %>%
      sf::`st_crs<-`(4283)
    if(profiled_area_type == "Headspace"){
      cluster_tb =  tibble::tibble(service_name = c("Glenroy", "Sunshine", "Craigieburn","Werribee"),
                                                    lat = c(-37.704890, -37.783314, -37.593766, -37.901473),
                                                    long = c(144.918099,144.831070,144.914055,144.662196)) %>%
        dplyr::filter(service_name %in% profiled_area)
    }
    if(profiled_area_type == "Custom"){
      cluster_tb =  tibble::tibble(service_name = profiled_area$service_vec,
                                   lat = profiled_area$lat_vec,
                                   long = profiled_area$lon_vec)
    }
      if(!is.null(distance_km)){
        profiled_area_sf <- ready.space::spatial_area_within_xkm_of_points(point_locations = cluster_tb,
                                                                           land_sf = aus_stt_sf,
                                                                           distance = distance_km*1000)
      }
    if(!is.null(travel_time_mins)){
      tt_from_cluster_isochrones <- ready.space::cluster_isochrones(cluster_tbs_list = list(cluster_tb),
                                                                    look_up_ref = 1)

      profiled_area_sf <- do.call(rbind,tt_from_cluster_isochrones) %>%
        sf::st_transform(4283)
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
  ## 4. APPLY PROFILED AREA FILTER
  profiled_pop_counts_sf <- spatial_profile_by_resolution_and_update_counts(profiled_sf = profiled_area_sf,
                                                                       resolution_sf = sp_data_list[[2]],
                                                                       resolution_sa1s_sf = sp_data_list[[4]],
                                                                       resolution_sa2s_sf = sp_data_list[[2]],
                                                                       return_resolution = "SA2")
  ## WIP START
  if(profiled_area_type=="PHN"){
    sp_data_list[[2]] <- profiled_pop_counts_sf
    }else{
    all_bands_pop_counts_sf <- profiled_pop_counts_sf
    centres_with_pop_whole_area_tb <- all_bands_pop_counts_sf
    sf::st_geometry(centres_with_pop_whole_area_tb) <- NULL
    ## https://github.com/tidyverse/dplyr/issues/3101
    centres_with_pop_whole_area_tb <- sum_at_diff_funs(data_tb = centres_with_pop_whole_area_tb,
                                                       var_list = list(c("id",
                                                                         "min",
                                                                         "max",
                                                                         "center",
                                                                         "AREASQKM16",
                                                                         names(all_bands_pop_counts_sf)[names(all_bands_pop_counts_sf) %>%
                                                                                                          startsWith("sa2_included")]),
                                                                       c("drive_times",
                                                                         "SA2_NAME16",
                                                                         "SA3_CODE16",
                                                                         names(all_bands_pop_counts_sf)[names(all_bands_pop_counts_sf) %>%
                                                                                                          endsWith("_CODE16")],
                                                                         names(all_bands_pop_counts_sf)[names(all_bands_pop_counts_sf) %>%
                                                                                                          endsWith("_NAME16")]
                                                                       )),
                                                       funs_list = tibble::lst(mean=mean,
                                                                               first=dplyr::first),
                                                       group_by = "SA2_MAIN16")
    centres_with_pop_whole_area_sf <- dplyr::inner_join(all_bands_pop_counts_sf %>%
                                                          dplyr::group_by(SA2_MAIN16) %>%
                                                          dplyr::select(SA2_MAIN16),
                                                        centres_with_pop_whole_area_tb) %>%
      sf::st_as_sf()
    pop_totals_tb <- centres_with_pop_whole_area_tb %>%
      dplyr::summarise_at(dplyr::vars(dplyr::starts_with("sa2_included")),
                          dplyr::funs(sum))
    
    if(!is.null(travel_time_mins)){
      by_band_pop_counts_sf_ls <- purrr::map(tt_from_cluster_isochrones,
                                             ~ spatial_profile_by_resolution_and_update_counts(profiled_sf = .x %>%
                                                                                                 sf::st_transform(4283),
                                                                                               resolution_sf = sp_data_list[[2]],
                                                                                               resolution_sa1s_sf = sp_data_list[[4]],
                                                                                               resolution_sa2s_sf = sp_data_list[[2]],
                                                                                               return_resolution = "SA2"))
      by_band_pop_counts_tb_ls <- purrr::map(by_band_pop_counts_sf_ls,
                                             ~ .x %>%
                                               sf::st_set_geometry(NULL) %>%
                                               sum_at_diff_funs(var_list = list(c("id","min","max", "center"),
                                                                                names(.x)[names(.x) %>% startsWith("sa2_included")]),
                                                                funs_list = tibble::lst(first = dplyr::first,
                                                                                        sum = sum),
                                                                group_by = "drive_times")
      )
      by_band_unioned_sf_ls <- purrr::map(by_band_pop_counts_sf_ls,
                                          ~ .x %>%
                                            sf::st_union() %>% 
                                            sf::st_sf())
      centres_with_pop_by_band_sf_list <- purrr::map2(by_band_unioned_sf_ls,
                                                      by_band_pop_counts_tb_ls,
                                                      ~ sf::st_bind_cols(.x,.y))
      centres_with_pop_by_band_sf <- do.call(rbind,centres_with_pop_by_band_sf_list)
      centres_with_pop_by_band_sf <- centres_with_pop_by_band_sf %>%
        dplyr::rename_at(dplyr::vars(dplyr::starts_with("sa2_included_")),
                         dplyr::funs(paste0("dtm_",stringr::str_sub(.,start=5) )))
      # pop_totals_tb_2 <- centres_with_pop_by_band_sf %>%
      #   dplyr::summarise_at(dplyr::vars(dplyr::starts_with("sa2_included")),
      #                       dplyr::funs(sum))
    }
    sp_data_list[[2]] <- centres_with_pop_by_band_sf#centres_with_pop_whole_area_sf
    #temp_copy <- sp_data_list[[2]] 
    # sp_data_list[[2]] <- sf::st_union(centres_with_pop_whole_area_sf,
    #                                   centres_with_pop_by_band_sf) # COULD WRITE CODE TO GET RID OF COLUMN DUPLICTES
    #sp_data_list[[2]] <- centres_with_pop_by_band_sf
    
    #append(sp_data_list,list(centres_with_pop_by_band_sf))
    ## WIP END
  }
  
  

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
sum_at_diff_funs <- function(data_tb,
                             var_list,
                             funs_list,
                             group_by){
  ## https://github.com/tidyverse/dplyr/issues/3101
  purrr::map2(var_list,
              funs_list,
              ~ data_tb %>%
                dplyr::group_by(!!rlang::sym(group_by)) %>%
                dplyr::summarise_at(.x, .y)) %>%
    purrr::reduce(dplyr::inner_join)
}
