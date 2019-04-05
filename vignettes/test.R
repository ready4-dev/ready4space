# library(ready.sim)
# #library(ready.space)
# library(ready.plot)
library(magrittr)
devtools::load_all()
make_profiled_area_input_spine_exmpl <- function(profiled_area_type,
                                                 bands_based_on_drive_time = FALSE){
  profiled_area_input <- list(profiled_area_type = profiled_area_type,
                              geom_dist_limit_km = NA_real_,
                              drive_time_imit_mins  = NA_real_)
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
## BELONGS IN AUS SPECIFIC PACKAGE
transform_profiled_area_inputs <- function(profiled_area_input,
                                           lookup_tb_r4){
  country <- "Australia"
  crs_nbr <- c(4283,3577)
  data_ymdhms = lubridate::ymd_hms("2016-07-01 12:00:00")
  data_year <- "2016"
  nbr_bands <- 5
  if(profiled_area_input$profiled_area_type == "PHN")
    use_coords_lup_val <- FALSE
  else
    use_coords_lup_val <- TRUE
  if(profiled_area_input$profiled_area_type == "Custom"){
    cluster_tb =  tibble::tibble(service_type = "Custom",
                                 cluster_name = profiled_area_input$profiled_area$cluster_vec,
                                 service_name = profiled_area_input$profiled_area$service_vec,
                                 lat = profiled_area_input$profiled_area$lat_vec,
                                 long = profiled_area_input$profiled_area$lon_vec)
    lookup_tb_r4 <- ready.s4::`sp_site_coord_lup<-`(lookup_tb_r4,
                                                    dplyr::bind_rows(cluster_tb,ready.s4::sp_site_coord_lup(lookup_tb_r4)) %>%
                                                      ready.s3::rfwn_sp_site_coord_lup())
    profiled_area <- profiled_area_input$profiled_area$service_vec
  }else{
    profiled_area = profiled_area_input$profiled_area
  }
  ready.s4::ready_profiled_area(country = country,
                                area_type = profiled_area_input$profiled_area_type,
                                features = profiled_area,
                                use_coord_lup = use_coords_lup_val,
                                lookup_tb = lookup_tb_r4,
                                crs_nbr = crs_nbr,
                                geom_dist_limit_km = profiled_area_input$geom_dist_limit_km,
                                drive_time_limmit_mins = profiled_area_input$drive_time_limit_mins,
                                nbr_bands = nbr_bands,
                                data_year = data_year,
                                data_ymds = data_ymdhms)
}

###
#disorder = "MDD"
# profiled_area = "Gippsland",#"Glenroy"
# profiled_area_type = "PHN",#"Headspace"#"Custom"#"PHN"#"Headspace"
# profiled_area_type = "Custom" # "Headspace" # "PHN" ### SIMULATES UInput
profiled_area_input <- make_profiled_area_input_spine_exmpl(profiled_area_type = "Custom",
                                                            bands_based_on_drive_time = FALSE) ### SIMULATES UInput
profiled_area_input <- transform_profiled_area_inputs(profiled_area_input = profiled_area_input,
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
  age_lower = 13,
  age_upper = 17,
  sexes = c("Female","Male"),
  ## EPIDEMIOLOGICAL INPUTS
  disorder = "ADHD",
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
                                                          disorder = "ADHD",
                                                          age_range = c(13,17),
                                                          sexes = c("Female","Male"))
)


## 3. DEFINE PROFILED AREA AND INCLUDED STATEs / TERRITORIES
# group_by_lookup_tb <- ready.s4::sp_data_uid_lup(lookup_tb_r4) %>%
#   dplyr::filter(year == get_data_year_chr(input_data$data_ymdhm))
# profiled_area_objs_ls <- make_profiled_area_objs(profiled_area_type = input_data$profiled_area_type,
#                                                  profiled_area = input_data$profiled_area,
#                                                  crs_nbr = input_data$crs_nbr,
#                                                  distance_km = input_data$distance_km,
#                                                  nbr_distance_steps = input_data$nbr_distance_steps,
#                                                  travel_time_mins = input_data$travel_time_mins,
#                                                  nbr_time_steps = input_data$nbr_time_steps,
#                                                  lookup_tb_r4 = lookup_tb_r4,
#                                                  data_year = get_data_year_chr(input_data$data_ymdhm))

sim_data <- make_sim_data_env(input_data = input_data)
grouping_for_sim <- ifelse(!is.null(input_data$distance_km),
                           "distance_km",
                           ifelse(!is.null(input_data$travel_time_mins),
                                  "drive_times", "SA2_MAIN16"))
sim_results_ls <- ready.sim::runSimulation(x = sim_data,#simDataInput(),
                                           nbr_its = input_data$nbr_its, #nbrItsInput(),
                                           group_by = grouping_for_sim)
ready.plot::plot_pop(profiled_sf = sim_results_ls[[1]],
                     plot_variable = "tx_prev_adhd_all",
                     population_string = "bbb",
                     year = "aaa")
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


