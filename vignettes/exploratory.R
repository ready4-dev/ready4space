library(magrittr)
vic_land_boundary_sf <- ready.space::create_australia_land_boundary(aus_boundary_sf =
                                                                      ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                                                           lookup_reference = "aus_boundary_phns_sf",
                                                                                           lookup_variable = "name",
                                                                                           target_variable = "source_reference") %>%
                                                                      dplyr::filter(FIRST_STE1=="Victoria"))
attributes_to_import <- c("aus_sa2_vic_att_erp_2016",
                          "aus_sa2_nat_att_seifa_2016",
                          "aus_sa1_nat_att_erp_2017",
                          "aus_lga_vic_att_ppr_2016")
attribute_list <- purrr::map(attributes_to_import,
                             ~ ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                   lookup_reference = .,
                                                   lookup_variable = "name",
                                                   target_variable = "source_reference")) %>%
  stats::setNames(attributes_to_import)
boundaries_to_import <- c("aus_lga_nat_shp_bound_2016",
                          "aus_sa1_nat_shp_bound_2016",
                          "aus_sa2_nat_shp_bound_2016")
boundary_list <- purrr::map(boundaries_to_import,
                            ~ ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                   lookup_reference = .,
                                                   lookup_variable = "name",
                                                   target_variable = "source_reference"))
boundary_list <- purrr::map(boundary_list,
                            ~ .x %>% dplyr::filter(STE_NAME16=="Victoria")) %>%
  stats::setNames(boundaries_to_import)
## OLD
vic_age_sex_seifa_sa2s_sf <- ready.space::spatial_merge_areas_attributes(area_unit = "SA2",
                                                                         area_sf = boundary_list %>%
                                                                           purrr::pluck("aus_sa2_nat_shp_bound_2016"),
                                                                         child_youth_pop_t0_tb = ready.data::data_get(data_lookup_tb = ready.space::aus_spatial_lookup_tb,
                                                                                                                      lookup_reference = "aus_pop_age_sex_sa2_2006_tb",
                                                                                                                      lookup_variable = "name",
                                                                                                                      target_variable = "source_reference"),
                                                                         child_youth_pop_t1_tb = attribute_list %>%
                                                                           purrr::pluck("aus_sa2_vic_att_erp_2016"),
                                                                         seifa_deciles_by_unit = attribute_list %>%
                                                                           purrr::pluck("aus_sa2_nat_att_seifa_2016"))
### NEW
vic_age_sex_seifa_sa2s_2006_2016_sf <- recur_add_attr_to_sf(country = "Australia",
                                                            state = "Victoria",
                                                            area_unit = "SA2",
                                                            boundary_year = "2016",
                                                            attribute_data = c("aus_pop_age_sex_sa2_2006_tb",
                                                                               "aus_sa2_vic_att_erp_2016",
                                                                               "aus_sa2_nat_att_seifa_2016"))
## NEED TO CHECK LGA AS WELL
test_par_val_mape <- ready.agents::gen_par_vals(ready.aus.data::params_struc_mape_tb,
                                                nbr_its = 5,
                                                jt_dist = FALSE)

test_par_val_env <- ready.agents::gen_par_vals(ready.agents::par_str_environment_tb,
                                               5)

test_par_val_master <- dplyr::bind_rows(test_par_val_env,
                                        test_par_val_mape)
## Can use below to eliminate uncertainty from population predictions:
# test_par_val_master <- test_par_val_master %>%
#   dplyr::mutate_if(is.numeric,dplyr::funs(ifelse(param_name=="pop_pe_sign",0,.)))
vic_pop_growth_projs_sf <- recur_add_attr_to_sf(country = "Australia",
                                                            state = "Victoria",
                                                            area_unit = "LGA",
                                                            boundary_year = "2016",
                                                            attribute_data = c("aus_lga_vic_att_ppr_2016",
                                                                               "aus_lga_vic_att_ppr_2021",
                                                                               "aus_lga_vic_att_ppr_2026",
                                                                               "aus_lga_vic_att_ppr_2031"))
growth_rates <- demographic_by_yearly_age_sex(profiled_sf = vic_pop_growth_projs_sf,
                                           years = c(2016,2019,2031,2025),
                                           age0 = 12,
                                           age1 = 18,
                                           gen_projections = TRUE,
                                           drop_projs = TRUE,
                                           param_tb = test_par_val_master,
                                           it_nbr = 1)
#vic_lga_y_16_31 <- spatial_population_growth(population_tib = vic_pop_growth_projs_sf,
#                          t0 = "2016",
#                          t1 = "2031")
####
####
spatial_vic_pop_growth_lga(vic_pop_growth_by_age_lga_t0 = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                                                                lookup_reference = "aus_lga_vic_att_ppr_2016",
                                                                                                lookup_variable = "name",
                                                                                                target_variable = "source_reference"),
                                            vic_pop_growth_by_age_lga_t1 = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                                                                lookup_reference = "aus_lga_vic_att_ppr_2031",
                                                                                                lookup_variable = "name",
                                                                                                target_variable = "source_reference"),
                                            t0 ="2016",
                                            t1 ="2031")
#vic_pop_growth_by_age_lga_2016_2031_sf <- dplyr::in
