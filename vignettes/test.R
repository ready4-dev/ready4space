# library(ready.sim)
# #library(ready.space)
# library(ready.plot)
library(magrittr)
devtools::load_all()
make_profiled_area_input_spine_exmpl <- function(profiled_area_type,
                                                 bands_based_on_drive_time = FALSE){
  profiled_area_input <- list(profiled_area_type = profiled_area_type,
                              geom_dist_limit_km = NA_real_,
                              drive_time_limit_mins  = NA_real_)
  if(profiled_area_type == "Custom"){
    custom_tb <- ymh.epsos::hyepp_coordinates_tb %>%
      dplyr::filter(cluster_name == "South East Melbourne")#"Eastern Melbourne" # profSfInput()
    profiled_area_input$profiled_area = list(cluster_vec = custom_tb %>% dplyr::pull(cluster_name),
                                             service_vec = custom_tb %>% dplyr::pull(service_name),
                                             lat_vec = custom_tb %>% dplyr::pull(lat),
                                             lon_vec = custom_tb %>% dplyr::pull(long))
  }
  if(profiled_area_type == "Headspace"){
    profiled_area_input$profiled_area = "Glenroy"
    # lookup_tb_r4 %>% ready.s4::sp_site_coord_lup()  %>%
    #   dplyr::filter(service_name %in% profiled_area) %>%
    #   ready.s4::`sp_site_coord_lup<-`(lookup_tb_r4,.)
  }
  if(profiled_area_type == "PHN"){
    profiled_area_input$profiled_area = "Gippsland"
  }else{
    if(bands_based_on_drive_time){
      profiled_area_input$drive_time_limit_mins  = 30
      profiled_area_input$geom_dist_limit_km = NA_real_
    } else{
      profiled_area_input$drive_time_limit_mins = NA_real_
      profiled_area_input$geom_dist_limit_km = 20
    }

  }
  return(profiled_area_input)
}
profiled_area_input <- vic.resilience::transform_profiled_area_inputs(profiled_area_input = make_profiled_area_input_spine_exmpl(profiled_area_type = "PHN",
                                                                                                                                  bands_based_on_drive_time = FALSE),
                                                   lookup_tb_r4 = vic.resilience::ready_lookup_tbs)
# ready.s4::ready_profiled_area(area_type = profiled_area_input$profiled_area_type,
#                               features = profiled_area_input$profiled_area,
#                               use_coord_lup = profiled_area_input$use_coords_lup_val,
#                               lookup_tb = profiled_area_input$lookup_tb_r4)
# profiled_area <- profiled_area_input$profiled_area
# lookup_tb_r4 <- profiled_area_input$lookup_tb_r4
# "ADHD"                 "Affective"            "Any_Common"           "Eating"
# "Generalized_Anxiety"  "Obsessive_Compulsive" "Personality"          "Social_Anxiety"
# "Substance_Use"
input_data <- list(
  ## DEMOGRAPHIC INPUTS
  age_lower = 12,
  age_upper = 25,
  sexes = c("Female","Male"),
  ## EPIDEMIOLOGICAL INPUTS
  disorder = "Any_Common",
  period = "Year",
  ## SPATIAL INPUTS
  at_highest_res = c("Population projections", "ERP", "ERP by age and sex"),
  at_specified_res = list(a=c("SEIFA","SA2")),
  age_sex_pop_str = "ERP by age and sex",
  #country = "Australia",
  #crs_nbr = 4283,
  #distance_km = NULL,#30,
  group_at_profile_unit = TRUE,
  #nbr_time_steps = 3,
  #nbr_distance_steps = 3,
  pop_projs_str = "Population projections",
  profiled_area_input = profiled_area_input,
  #profiled_area = profiled_area,#"Gippsland",#"Glenroy"
  #profiled_area_type = profiled_area_type, #"PHN",#"Headspace"#"Custom"#"PHN"#"Headspace"
  tot_pop_str = "ERP",
  #travel_time_mins = NULL,#60
  ## TEMPORAL INPUTS
  #data_ymdhms = lubridate::ymd_hms("2016-07-01 12:00:00"),
  model_start_ymdhms = lubridate::ymd_hms("2019_07_01 12:00:00"),
  nbr_steps_start_to_end = 7,
  simulation_steps_ymwd = c(1,0,0,0),
  ## UNCERTAINTY INPUTS
  deterministic = FALSE,
  nbr_its = 2,
  uncertainty_int = c(0.025,0.975),
  ## PARAMETER MATRIX
  env_str_par_tb = ready.sim::add_prev_to_env_str_par_tb(pref_source = ymh.epi.lit::pref_source,
                                                          prev_rates = ymh.epi.lit::prev_rates,
                                                          env_str_par_tb = ready.aus.prev::par_str_environment_tb,
                                                          Period = "Year",
                                                          disorder = "Any_Common",
                                                          age_range = c(12,25),
                                                          sexes = c("Female","Male"))
)
sim_data <- ready.space::make_sim_data_env(input_data = input_data)
## PLOT TEMPLATE
ggplot2::ggplot(ready.sim::st_data(ready.sim::st_envir(sim_data))$profiled_sf ) +
  ggplot2::geom_sf(ggplot2::aes(fill=grp_by_SA2_inc_age_sex_y2016.Males.15.19),colour=NA) +
  ggplot2::ggtitle("TITLE") +
  viridis::scale_fill_viridis("Persons") + #TRUE) +
  ggplot2::theme_bw()
ggplot2::ggplot(sp_data_sf) +
  ggplot2::geom_sf(ggplot2::aes(fill=pop_sp_unit_id),colour=NA)
# drop_grouped_popl_vars(profiled_sf = ready.sim::st_envir(sim_data) %>%
#                          ready.sim::st_data() %>%
#                          purrr::pluck("profiled_sf"),
#                        age_sex_prefix = ready.sim::st_envir(sim_data) %>%
#                          ready.sim::st_data() %>%
#                          purrr::pluck("popl_var_prefix"))
grouping_for_sim <- ifelse(!is.na(input_data$profiled_area_input %>%
                                    ready.s4::geom_dist_limit_km()),
                           "distance_km",
                           ifelse(!is.na(input_data$profiled_area_input %>%
                                           ready.s4::drive_time_limmit_mins()),
                                  "drive_times", "SA2_MAIN16"))
#ready.sim::st_data(ready.sim::st_envir(sim_data))$profiled_sf %>% names()
sim_results_ls <- ready.sim::runSimulation(x = sim_data,#simDataInput(),
                                           nbr_its = input_data$nbr_its, #nbrItsInput(),
                                           group_by = grouping_for_sim)
ready.plot::plot_pop(profiled_sf = sim_results_ls[[1]], #ready.sim::st_data(ready.sim::st_envir(sim_data))$profiled_sf
                     plot_variable = "tx_prev_adhd_all",#inc_SA1_popl_y2016.Males.15.19
                     population_string = "bbb",
                     year = "aaa")
ggplot2::ggplot(sim_results_ls[[1]]) +
  ggplot2::geom_sf(ggplot2::aes(fill=tx_prev_adhd_all),colour=NA) +
  ggplot2::ggtitle("TITLE") +
  viridis::scale_fill_viridis("Persons") + #TRUE) +
  ggplot2::theme_bw()
# sim_results = ready.sim::runSimulation(x = sim_data,
#                             nbr_its = nbr_its,
#                             group_by = "distance_km"
#                             )
##runSimulation :: DONT group_by: pop_sp_unit_id
## group_by <- "distance_km2"
###
###
# ready.data:::data_import_make_sp_lookup_tb()
# test1<-ready.data::data_import_show_menu_of_type_detail("Attribute")
# ready.data::data_import_show_menu_of_type_detail("Attribute")
# attributes_to_import <- ready.data::data_import_show_menu_of_type_names("Attribute")
# ready.data::data_import_selected_downloads(required_data = attributes_to_import,
#                                            destination_directory = "data-raw")
# import_items(included_items_names = attributes_to_import,
#              item_data_type = "Attribute")

## DATA INPUT: SPATIAL


