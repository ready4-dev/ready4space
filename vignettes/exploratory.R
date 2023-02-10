devtools::load_all(".")
library(magrittr)
library(ready.sim)
## Step 0: User Input
state_territory = "Victoria"
profiled_area = "Service cluster - Orygen headspaces"
age_lower = 12
age_upper = 20
disorder = "Anxiety"
sexes = c("Female","Male")
model_end_y_int = 2023
n_its_int = 4
env_str_param_tb = ready.aus.prev::param_str_environment_tb
deterministic = FALSE
# if(state_territory != "Victoria")
#   model_end_y_int = 2016
## 1. Create Simulate Object
sim_data <- project_epi_for_area(state_territory = state_territory,
                                 profiled_area = profiled_area,
                          age_lower = age_lower,
                          age_upper = age_upper,
                          model_end_y_int = model_end_y_int,
                          n_its_int = n_its_int,
                          env_str_param_tb = env_str_param_tb,
                          deterministic = deterministic)
## 2. Simulate
# sim_data <- ready.sim::runSimulation(sim_data)
# ready.sim::env_sf(ready.sim::st_envir(sim_data))
## 3. Estimate Prevalence
estimate_prevalence(disorder = disorder,
                    period = "Year",
                    ages = age_lower:age_upper,
                    sexes = sexes,
                    pop_data = ready.sim::env_sf(ready.sim::st_envir(sim_data)) %>%
                      dplyr::group_by(SA2_MAIN16) %>%
                      dplyr::summarise_at(dplyr::vars(dplyr::starts_with("tx_")),
                                          dplyr::funs(mean)))
##

#
# rfwn.plot::plot_area_within_xkm_from_services(service_centre_locations_sf = orygen_headspace_cluster_boundary_10k_sf,
#                                                   distance = 10000,
#                                                   titlestring = "Orygen Headspace Centres")
# ready.plot::plot_seifa_by_SA2_SA1(profiled_sf = sp_data_list[[2]],
#                                      profiled_unit_name = "Area <10km from Orygen Headspaces",
#                                      resolution_unit_name = "sa2",
#                                      year = "2016")
## 5. SIMULATE EVOLUTION OF ENVIRONMENT OVER TIME
# sp_data_sf <- ready.sim::sim_environment(sp_data_list = sp_data_list,
#                               age0 = 12,
#                               age1 = 18,
#                               at_time = "2016",
#                               to_time = "2031",
#                               param_tb = env_param_tb,
#                               it_nbr = 1,
#                               #sp_data_sf = sp_data_sf,
#                               ymwd_step_to_bl = NULL,
#                               ymwd_step_from_tx = c(1,0,2,1),
#                               nbr_steps = 2)
# sp_data_sf <- ready.sim::sim_environment(sp_data_list = st_data(st_envir(sim_data)),
#                                          age0 = age_lower(sim_data),
#                                          age1 = age_upper(sim_data),
#                                          at_time = pre_model_date(sim_data),
#                                          to_time = model_end_date(sim_data),
#                                          param_tb = param_vals(st_envir(sim_data)),
#                                          it_nbr = 1,
#                                          #sp_data_sf = sp_data_sf,
#                                          ymwd_step_to_bl = NULL,#,
#                                          ymwd_step_from_tx = time_steps(sim_data),
#                                          nbr_steps = nbr_steps(sim_data))

# env_sf(st_envir(sim_data)) <- ready.sim::sim_environment(sp_data_list = st_data(st_envir(sim_data)),
#                                                          age0 = age_lower(sim_data),
#                                                          age1 = age_upper(sim_data),
#                                                          at_time = pre_model_date(sim_data),
#                                                          to_time = model_end_date(sim_data),
#                                                          param_tb = param_vals(st_envir(sim_data)),
#                                                          it_nbr = 1,
#                                                          #sp_data_sf = sp_data_sf,
#                                                          ymwd_step_to_bl = NULL,#,
#                                                          ymwd_step_from_tx = time_steps(sim_data),
#                                                          nbr_steps = nbr_steps(sim_data))
##
##
##
# vic_land_boundary_sf <- create_australia_land_boundary(state_territories = c("Victoria"))
# attributes_to_import <- c("aus_sa2_vic_att_erp_2016",
#                           "aus_sa2_nat_att_seifa_2016",
#                           "aus_sa1_nat_att_erp_2017",
#                           "aus_lga_vic_att_ppr_2016")
# attribute_list <- purrr::map(attributes_to_import,
#                              ~ ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
#                                                    lookup_reference = .,
#                                                    lookup_variable = "name",
#                                                    target_variable = "source_reference")) %>%
#   stats::setNames(attributes_to_import)
# boundaries_to_import <- c("aus_lga_nat_shp_bound_2016",
#                           "aus_sa1_nat_shp_bound_2016",
#                           "aus_sa2_nat_shp_bound_2016")
# boundary_list <- purrr::map(boundaries_to_import,
#                             ~ ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
#                                                    lookup_reference = .,
#                                                    lookup_variable = "name",
#                                                    target_variable = "source_reference"))
# boundary_list <- purrr::map(boundary_list,
#                             ~ .x %>% dplyr::filter(STE_NAME16=="Victoria")) %>%
#   stats::setNames(boundaries_to_import)
# ##
# vic_age_sex_seifa_sa2s_2006_2016_sf <- add_attr_recrly_to_sf(country_chr = "Australia",
#                                                             state = "Victoria",
#                                                             area_unit = "SA2",
#                                                             boundary_year = "2016",
#                                                             attribute_data = c("aus_pop_age_sex_sa2_2006_tb",
#                                                                                "aus_sa2_vic_att_erp_2016",
#                                                                                "aus_sa2_nat_att_seifa_2016"))
#
#
# vic_pop_growth_projs_sf <- add_attr_recrly_to_sf(country_chr = "Australia",
#                                                             state = "Victoria",
#                                                             area_unit = "LGA",
#                                                             boundary_year = "2016",
#                                                             attribute_data = c("aus_lga_vic_att_ppr_2016",
#                                                                                "aus_lga_vic_att_ppr_2021",
#                                                                                "aus_lga_vic_att_ppr_2026",
#                                                                                "aus_lga_vic_att_ppr_2031"))
# vic_age_sex_acgr_lga_2016_31_sf <- gen_demog_features(profiled_sf = vic_pop_growth_projs_sf,
#                                            years = c(2016,2019,2031,2025),
#                                            age0 = 12,
#                                            age1 = 18,
#                                            #age_by_year = FALSE,
#                                            #drop_projs = TRUE,
#                                            param_tb = test_param_val_master,
#                                            it_nbr = 1)
# ##
#
# vic_merged_attr_sf <- intersect_sf_drop_cols(main_sf = vic_age_sex_seifa_sa2s_2006_2016_sf,
#                        adjunct_sf = vic_age_sex_acgr_lga_2016_31_sf)
# vic_merged_attr_by_age_sf <- gen_demog_features(profiled_sf = vic_merged_attr_sf,
#                                                            years = c(2016,2019,2031,2025),
#                                                            age0 = 12,
#                                                            age1 = 18,
#                                                            acgr = FALSE,
#                                                            age_by_year = TRUE,
#                                                            drop_bands = TRUE,
#                                                            param_tb = test_param_val_master,
#                                                            it_nbr = 1)
# ###
# gen_age_sex_estimates_tx(profiled_sf = vic_merged_attr_by_age_sf,
#                          ymwd_step = c(22,5,2,1))
##
