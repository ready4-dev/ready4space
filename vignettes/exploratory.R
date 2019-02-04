devtools::load_all(".")
library(magrittr)
## CLASS EXPERIMENTS
# abc<-rfwn_dist()
# abc<-rfwn_dist_pert()
# is.rfwn_dist_pert(abc)
##
## 1. GET PARAMETER MATRICES
make_master_param_tb <- function(nbr_its,
                                 env_str_par_tb,
                                 mape_str_par_tb,
                                 jt_dist){
  par_val_mape <- ready.agents::gen_par_vals(x = mape_str_par_tb,
                                             nbr_its = nbr_its,
                                             jt_dist = jt_dist)
  par_val_env <- ready.agents::gen_par_vals(x = env_str_par_tb,
                                            nbr_its = nbr_its)
  dplyr::bind_rows(par_val_env,
                   par_val_mape)
}
test_par_val_master <- make_master_param_tb(nbr_its = 5,
                     env_str_par_tb = ready.agents::par_str_environment_tb,
                     mape_str_par_tb = ready.aus.data::params_struc_mape_tb,
                     jt_dist = FALSE)
# test_par_val_mape <- ready.agents::gen_par_vals(ready.aus.data::params_struc_mape_tb,
#                                                 nbr_its = 5,
#                                                 jt_dist = FALSE)
#
# test_par_val_env <- ready.agents::gen_par_vals(ready.agents::par_str_environment_tb,
#                                                5)
#
# test_par_val_master <- dplyr::bind_rows(test_par_val_env,
#                                         test_par_val_mape)
## Can use below to eliminate uncertainty from population predictions:
# test_par_val_master <- test_par_val_master %>%
#   dplyr::mutate_if(is.numeric,dplyr::funs(ifelse(param_name=="pop_pe_sign",0,.)))
## 2. GET SPATIAL DATA
sp_data_list <- get_spatial_data_list(at_highest_res = c("ERP by age and sex",
                                                         "ERP",
                                                         "Population projections"),
                                      at_time = "2016",
                                      to_time = "2031",
                                      at_specified_res = list(a=c("SEIFA","SA2")),
                                      country = "Australia",
                                      state = "Victoria",
                                      require_year_match = FALSE,
                                      excl_diff_bound_yr = TRUE)
## 3. APPLY PROFILED AREA FILTER
vic_st_boundary_sf <- ready.aus.data::aus_boundary_phns_sf %>%
  dplyr::filter(FIRST_STE1 == "Victoria") %>%
  sf::st_union()
orygen_headspace_cluster_long_lat_10k_tb =  tibble::tibble(lat = c(-37.704890, -37.783314, -37.593766, -37.901473),
                                                           long = c(144.918099,144.831070,144.914055,144.662196))
orygen_headspace_cluster_boundary_10k_sf <- rfwn.space.time::spatial_area_within_xkm_of_points(point_locations = orygen_headspace_cluster_long_lat_10k_tb,
                                                                                               land_sf = vic_st_boundary_sf,
                                                                                               distance = 10000)
rfwn.plot::plot_area_within_xkm_from_services(service_centre_locations_sf = orygen_headspace_cluster_boundary_10k_sf,
                                                  distance = 10000,
                                                  titlestring = "Orygen Headspace Centres")
sp_data_list_list_2 <- sp_data_list
sp_data_list_list_2[[2]] <- spatial_profile_by_resolution_and_update_counts(profiled_sf = orygen_headspace_cluster_boundary_10k_sf,
                                                                             resolution_sf = sp_data_list_list_2[[2]],
                                                                             resolution_sa1s_sf = sp_data_list_list_2[[4]],
                                                                             resolution_sa2s_sf = sp_data_list_list_2[[2]],
                                                                             return_resolution = "SA2")
ready.plot::plot_seifa_by_SA2_SA1(profiled_sf = sp_data_list_list_2[[2]],
                                     profiled_unit_name = "Area <10km from Orygen Headspaces",
                                     resolution_unit_name = "sa2",
                                     year = "2016")

## 4. SIMULATE EVOLUTION OF ENVIRONMENT OVER TIME
sp_data_sf <- ready.sim::sim_environment(sp_data_list = sp_data_list_list_2,
                              age0 = 12,
                              age1 = 18,
                              at_time = "2016",
                              to_time = "2031",
                              param_tb = test_par_val_master,
                              it_nbr = 1,
                              #sp_data_sf = sp_data_sf,
                              ymwd_step_to_bl = NULL,
                              ymwd_step_from_tx = c(1,0,2,1),
                              nbr_steps = 2)

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
# vic_age_sex_seifa_sa2s_2006_2016_sf <- recur_add_attr_to_sf(country = "Australia",
#                                                             state = "Victoria",
#                                                             area_unit = "SA2",
#                                                             boundary_year = "2016",
#                                                             attribute_data = c("aus_pop_age_sex_sa2_2006_tb",
#                                                                                "aus_sa2_vic_att_erp_2016",
#                                                                                "aus_sa2_nat_att_seifa_2016"))
#
#
# vic_pop_growth_projs_sf <- recur_add_attr_to_sf(country = "Australia",
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
#                                            param_tb = test_par_val_master,
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
#                                                            param_tb = test_par_val_master,
#                                                            it_nbr = 1)
# ###
# gen_age_sex_estimates_tx(profiled_sf = vic_merged_attr_by_age_sf,
#                          ymwd_step = c(22,5,2,1))
##
