# library(ready.sim)
# #library(ready.space)
# library(ready.plot)
library(magrittr)
devtools::load_all()
###
# age_range <- c(13,17) #ageRangeInput() #input$select_prof_sf
# profiled_area_type = "Custom"#"PHN"#"Headspace"
# custom_tb <- ymh.epsos::hyepp_coordinates_tb %>%
#   dplyr::filter(cluster_name == "South East Melbourne")#"Eastern Melbourne" # profSfInput()
#
# profiled_area = list(cluster_vec = custom_tb %>% dplyr::pull(cluster_name),
#                      service_vec = custom_tb %>% dplyr::pull(service_name),
#                      lat_vec = custom_tb %>% dplyr::pull(lat),
#                      lon_vec = custom_tb %>% dplyr::pull(long))

#disorder = "MDD"
age_range <- c(13,17) #ageRangeInput() #input$select_prof_sf
profiled_area_type = "Headspace"#"Custom"#"PHN"#"Headspace"
profiled_area = "Glenroy"
# custom_tb <- ymh.epsos::hyepp_coordinates_tb %>%
#   dplyr::filter(cluster_name == "South East Melbourne")#"Eastern Melbourne" # profSfInput()
# profiled_area = list(cluster_vec = custom_tb %>% dplyr::pull(cluster_name),
#                      service_vec = custom_tb %>% dplyr::pull(service_name),
#                      lat_vec = custom_tb %>% dplyr::pull(lat),
#                      lon_vec = custom_tb %>% dplyr::pull(long))
disorder = "ADHD"# "ADHD"                 "Affective"            "Any_Common"           "Eating"
# "Generalized_Anxiety"  "Obsessive_Compulsive" "Personality"          "Social_Anxiety"
# "Substance_Use"

sexes = c("Female","Male")
#project_for_year = "2023" ## Now a character.
data_ymdhms = lubridate::ymd_hms("2016-07-01 12:00:00")
model_start_ymdhms = lubridate::ymd_hms("2019_07_01 12:00:00")
nbr_steps_start_to_end = 7
simulation_steps_ymwd = c(1,0,0,0)
#model_end_ymdhms = lubridate::ymd_hms("2031_07_01 12:00:00")
nbr_its = 2
env_str_par_tb = ready.aus.prev::par_str_environment_tb
deterministic = FALSE
period = "Year"
uncertainty_int <- c(0.025,0.975)
## DATA INPUT: PREVALENCE
# pref_source <- ymh.epi.lit::pref_source %>%
#   dplyr::filter(Period == "Year") %>%
#   dplyr::mutate(nbr_refs = rowSums(!is.na(.[startsWith(names(.), "Female") |
#                                               startsWith(names(.), "Male")]))) %>%
#   dplyr::filter(nbr_refs == 28)
# disorder_options <- pref_source  %>%
#   dplyr::pull(Disorder)
# prev_rates <- ymh.epi.lit::prev_rates %>%
#   dplyr::filter(Period == "Year")
# disorder <- disorder_options[1]
# prev_rates_vec = ready.space::make_prev_struc_par_tb(disorder = disorder,
#                                                      period = "Year",
#                                                      ages = age_range[1]:age_range[2],
#                                                      sexes = c("Female","Male"),
#                                                      pref_source = pref_source ,
#                                                      prev_rates = prev_rates)
# prev_sum_tb <- ready.space::make_prev_rates_sum_tb(prev_rates_vec)
# env_str_par_tb <- env_str_par_tb %>%
#   dplyr::add_row(param_name = paste0("prev_",
#                                      tolower(disorder),
#                                      "_",
#                                      names(prev_rates_vec)),
#                  deter_val = prev_rates_vec,
#                  distribution = rep("normal", times = length(prev_rates_vec)),
#                  dist_param_1 = prev_rates_vec,
#                  dist_param_2 = rep(0, times = length(prev_rates_vec)),
#                  transformation = "max(x,0)")
##

distance_km = 30
nbr_distance_steps = 3
travel_time_mins = NULL#60
nbr_time_steps = 3
age_lower = age_range[1]
age_upper = age_range[2]
group_at_profile_unit = TRUE
at_highest_res_extra = NULL
at_specified_res = list(a=c("SEIFA","SA2"))
age_sex_pop_str = "ERP by age and sex"
tot_pop_str = "ERP"
pop_projs_str = "Population projections"
country = "Australia"
crs_nbr = 4283
m_results = ready.sim::runSimulation(x = sim_data,
                            nbr_its = nbr_its,
                            group_by = "distance_km"
                            )
##runSimulation :: DONT group_by: pop_sp_unit_id
## group_by <- "distance_km2"
###
###
ready.data:::data_import_make_sp_lookup_tb()
test1<-ready.data::data_import_show_menu_of_type_detail("Attribute")
ready.data::data_import_show_menu_of_type_detail("Attribute")
attributes_to_import <- ready.data::data_import_show_menu_of_type_names("Attribute")
ready.data::data_import_selected_downloads(required_data = attributes_to_import,
                                           destination_directory = "data-raw")
attribute_list <- ready.data::data_imenv_str_par_tb <- ready.sim::add_prev_to_env_str_par_tb(pref_source = ymh.epi.lit::pref_source,
                                                        prev_rates = ymh.epi.lit::prev_rates,
                                                        env_str_par_tb = ready.aus.prev::par_str_environment_tb,
                                                        Period = "Year",
                                                        disorder,
                                                        age_range,
                                                        sexes = c("Female","Male"))
## DATA INPUT: SPATIAL
sim_data <- make_sim_data_env(profiled_area_type = profiled_area_type,
                              profiled_area = profiled_area,
                              distance_km = distance_km,
                              nbr_distance_steps = nbr_distance_steps,
                              travel_time_mins = travel_time_mins,
                              nbr_time_steps = nbr_time_steps,
                              age_lower = age_lower,
                              age_upper = age_upper,
                              nbr_its = nbr_its,
                              env_str_par_tb = env_str_par_tb,
                              deterministic = deterministic,
                              group_at_profile_unit = group_at_profile_unit,
                              data_ymdhms = data_ymdhms,
                              model_start_ymdhms = model_start_ymdhms,
                              simulation_steps_ymwd = simulation_steps_ymwd,
                              nbr_steps_start_to_end = nbr_steps_start_to_end,
                              at_highest_res_extra = at_highest_res_extra,
                              at_specified_res = at_specified_res,
                              age_sex_pop_str = age_sex_pop_str,
                              tot_pop_str = tot_pop_str,
                              pop_projs_str = pop_projs_str,
                              country = country,
                              crs_nbr = crs_nbr)
siport_items(included_items_names = attributes_to_import,
                                                item_data_type = "Attribute")
