library(ready.sim)
devtools::load_all()
#library(ready.space)
library(ready.plot)
library(magrittr)
###
age_range <- c(13,17) #ageRangeInput() #input$select_prof_sf
profiled_area_type = "Custom"#"PHN"#"Headspace"
custom_tb <- ymh.epsos::hyepp_coordinates_tb %>%
  dplyr::filter(cluster_name == "South East Melbourne")#"Eastern Melbourne" # profSfInput()

profiled_area = list(cluster_vec = custom_tb %>% dplyr::pull(cluster_name),
                     service_vec = custom_tb %>% dplyr::pull(service_name),
                     lat_vec = custom_tb %>% dplyr::pull(lat),
                     lon_vec = custom_tb %>% dplyr::pull(long))

disorder = "MDD"
sexes = c("Female","Male")
#project_for_year = "2023" ## Now a character.
data_ymdhms = lubridate::ymd_hms("2016-07-01 12:00:00")
model_start_ymdhms = lubridate::ymd_hms("2019_07_01 12:00:00")
model_end_ymdhms = lubridate::ymd_hms("2031_07_01 12:00:00")
nbr_its = 2
env_str_par_tb = ready.aus.prev::par_str_environment_tb
deterministic = FALSE
period = "Year"
uncertainty_int <- c(0.025,0.975)
## DATA INPUT: PREVALENCE
prev_rates_vec = make_prev_struc_par_tb(disorder = disorder,
                                                     period = "Year",
                                                     ages = age_range[1]:age_range[2],
                                                     sexes = c("Female","Male"),
                                                     pref_source = ymh.epi.lit::pref_source,
                                                     prev_rates = ymh.epi.lit::prev_rates)
prev_sum_tb <- make_prev_rates_sum_tb(prev_rates_vec)
## DATA INPUT: SPATIAL
sim_data <- make_sim_data_env(profiled_area_type = profiled_area_type,
                                           profiled_area =  profiled_area,
                                           distance_km = NULL,#30,
                                           # nbr_distance_steps = 3,
                                           travel_time_mins = 60,
                                           nbr_time_steps = 3,
                                           age_lower = age_range[1],
                                           age_upper = age_range[2],
                                           nbr_its = nbr_its,
                                           env_str_par_tb = env_str_par_tb,
                                           deterministic = deterministic,
                                           group_at_profile_unit = TRUE,
                                           data_ymdhms = data_ymdhms,
                                           model_start_ymdhms = model_start_ymdhms,
                                           model_end_ymdhms = model_end_ymdhms,
                                           at_highest_res_extra = NULL,
                                           at_specified_res = list(a=c("SEIFA","SA2")),
                                           age_sex_pop_str = "ERP by age and sex",
                                           tot_pop_str = "ERP",
                                           pop_projs_str = "Population projections",
                                           country = "Australia",
                                           crs_nbr = 4283,
                                           var_name_lookup_tb = tibble::tibble(resolution = c("SA1","SA2","SA3", "SA4"),
                                                                               year = c("2016", "2016","2016","2016"),
                                                                               var_name = c("SA1_MAIN16","SA2_MAIN16","SA3_MAIN16","SA4_MAIN16")))
