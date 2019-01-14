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

