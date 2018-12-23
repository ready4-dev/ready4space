library(magrittr)
old_spatial_lookup_tb <- tibble::tibble(variable_name = c("aus_age_sex_seifa_lgas_sf",
                                                          "aus_age_sex_seifa_sa2s_sf",
                                                          "aus_boundary_lgas_sf",
                                                          "aus_boundary_phns_sf",
                                                          "aus_boundary_sa1s_sf",
                                                          "aus_boundary_sa2s_sf",
                                                          "aus_boundary_postal_sf",
                                                          "aus_state_suburbs_sf",
                                                          "aus_correspondences_lga_2011_2016_tb",
                                                          "aus_pop_age_sex_lga_2011_tb",
                                                          "aus_pop_age_sex_lga_2016_tb",
                                                          "aus_pop_age_sex_sa2_2006_tb",
                                                          "aus_pop_age_sex_sa2_2016_tb",
                                                          "aus_pop_proj_sa1_2011_2016_tb",
                                                          "aus_seifa_deciles_by_lga_2011_tb",
                                                          "aus_seifa_deciles_by_lga_2016_tb",
                                                          "aus_seifa_deciles_by_sa2_2016_tb",
                                                          "vic_boundary_state_electorates_sf",
                                                          "vic_pop_growth_by_age_lga_2016_tb",
                                                          "vic_pop_growth_by_age_lga_2021_tb",
                                                          "vic_pop_growth_by_age_lga_2026_tb",
                                                          "vic_pop_growth_by_age_lga_2031_tb"),
                                        source_reference = c("ready.aus.data::aus_age_sex_seifa_lgas_sf",
                                                             "ready.aus.data::aus_age_sex_seifa_sa2s_sf",
                                                             "ready.aus.data::aus_boundary_lgas_sf",
                                                             "ready.aus.data::aus_boundary_phns_sf",
                                                             "ready.aus.data::aus_boundary_sa1s_sf",
                                                             "ready.aus.data::aus_boundary_sa2s_sf",
                                                             "ready.aus.data::aus_boundary_postal_sf",
                                                             "ready.aus.data::aus_state_suburbs_sf",
                                                             "ready.aus.data::aus_correspondences_lga_2011_2016_tb",
                                                             "ready.aus.data::aus_pop_age_sex_lga_2011_tb",
                                                             "ready.aus.data::aus_pop_age_sex_lga_2016_tb",
                                                             "ready.aus.data::aus_pop_age_sex_sa2_2006_tb",
                                                             "ready.aus.data::aus_pop_age_sex_sa2_2016_tb",
                                                             "ready.aus.data::aus_pop_proj_sa1_2011_2016_tb",
                                                             "ready.aus.data::aus_seifa_deciles_by_lga_2011_tb",
                                                             "ready.aus.data::aus_seifa_deciles_by_lga_2016_tb",
                                                             "ready.aus.data::aus_seifa_deciles_by_sa2_2016_tb",
                                                             "ready.aus.data::vic_boundary_state_electorates_sf",
                                                             "ready.aus.data::vic_pop_growth_by_age_lga_2016_tb",
                                                             "ready.aus.data::vic_pop_growth_by_age_lga_2021_tb",
                                                             "ready.aus.data::vic_pop_growth_by_age_lga_2026_tb",
                                                             "ready.aus.data::vic_pop_growth_by_age_lga_2031_tb"))

old_spatial_lookup_tb <- old_spatial_lookup_tb %>%
  dplyr::rename(name = variable_name)
sp_saved_data_lookup_tb <- ready.data::data_import_show_menu_detail() %>%
  dplyr::select(1:7)
sp_saved_data_lookup_tb <- rbind(sp_saved_data_lookup_tb,
                                 sp_saved_data_lookup_tb %>%
                                   dplyr::filter(name=="aus_lga_nat_att_erp_2011") %>%
                                   dplyr::mutate(name="aus_lga_nat_att_erp_2011_with_2016_bound",
                                                 main_feature = "ERP by age and sex for 2016 boundaries")) %>%
  dplyr::arrange(name)
sp_saved_data_lookup_tb <- sp_saved_data_lookup_tb %>%
  dplyr::mutate(source_reference = paste0("ready.aus.data::",name))
aus_spatial_lookup_tb <- dplyr::bind_rows(sp_saved_data_lookup_tb,
                                          old_spatial_lookup_tb)

usethis::use_data(aus_spatial_lookup_tb,
                   overwrite = TRUE)
usethis::use_data(aus_spatial_lookup_tb,
                   overwrite = TRUE,
                   internal = TRUE)
