vic_boundary_phns_sf <- ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                             lookup_reference = "aus_boundary_phns_sf") %>%
  dplyr::filter(FIRST_STE1=="Victoria") %>%
  sf::st_as_sf()
vic_boundary_sa2s_sf <- ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                             lookup_reference = "aus_boundary_sa2s_sf") %>%
  dplyr::filter(STE_NAME16=="Victoria")
vic_boundary_sa1s_sf <- ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                             lookup_reference = "aus_boundary_sa1s_sf") %>%
  dplyr::filter(STE_NAME16=="Victoria")
vic_boundary_state_sf <- vic_boundary_phns_sf %>%
  dplyr::group_by(FIRST_STE1) %>%
  dplyr::summarise(AREASQ = sum(SUM_AREASQ),
                   geometry = sf::st_union(geometry))
## Attribute Data
res_sa2s_sf <- ready.data::spatial_merge_areas_attributes(area_unit="SA2",
                                                                 area_sf = vic_boundary_sa2s_sf,
                                                                 child_youth_pop_t0_tb = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                                                                              lookup_reference = "aus_pop_age_sex_sa2_2006_tb"),
                                                                 child_youth_pop_t1_tb = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                                                                              lookup_reference = "aus_pop_age_sex_sa2_2016_tb"),
                                                                 seifa_deciles_by_unit = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                                                                              lookup_reference = "aus_seifa_deciles_by_sa2_2016_tb"))
res_sa1s_sf <- merge(vic_boundary_sa1s_sf,
                                  ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                       lookup_reference = "aus_pop_proj_sa1_2011_2016_tb"),
                                  by.x = c("SA1_7DIG16"),
                                  by.y = c("SA1"),
                                  all = FALSE) %>%
  sf::st_as_sf()
## MERGE
spatial_profile_by_sa2(profiled_sf = vic_boundary_phns_sf,
                                        col_ref = "FIRST_PHN_",
                                        res_sa1s_sf = resolution_sa1s_sf,
                                        res_sa2s_sf = resolution_sa2s_sf)

### BETTER MERGE OPTION (FIXED)
filter_by_phn <-function(phn_sf = vic_boundary_phns_sf,
                         phn_ref){
  filtered_phn_sf <- seplyr::filter_se(phn_sf,
                                       "FIRST_PHN_==phn_ref")
  return(filtered_phn_sf)
}
