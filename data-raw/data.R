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
aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
  dplyr::mutate(main_feature = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "ERP by age and sex",main_feature),
                area_type = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "SA2",area_type))
###
aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
  dplyr::mutate(country = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "Australia", country),
                region = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "National", region),
                data_type = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "Attribute", data_type),
                year = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "2006", year))
###
aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
  dplyr::mutate(name = ifelse(name =="aus_lga_vic_att_ppr_2016", "aus_lga_vic_att_ppr_2016_31",name))
aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
  dplyr::mutate(year = ifelse(name =="aus_lga_vic_att_ppr_2016_31", "2016_31",year))
aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
  dplyr::mutate(country = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "Australia",country),
                area_type = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "LGA",area_type),
                region = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "VIC",region),
                data_type = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "Attribute",data_type),
                main_feature = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "Population projections",main_feature),
                year = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "2016",year),
                name = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "aus_lga_vic_att_ppr_2016",name)) %>%
  dplyr::mutate(country = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "Australia",country),
                area_type = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "LGA",area_type),
                region = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "VIC",region),
                data_type = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "Attribute",data_type),
                main_feature = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "Population projections",main_feature),
                year = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "2021",year),
                name = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "aus_lga_vic_att_ppr_2021",name)) %>%
  dplyr::mutate(country = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "Australia",country),
                area_type = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "LGA",area_type),
                region = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "VIC",region),
                data_type = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "Attribute",data_type),
                main_feature = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "Population projections",main_feature),
                year = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "2026",year),
                name = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "aus_lga_vic_att_ppr_2026",name)) %>%
  dplyr::mutate(country = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "Australia",country),
                area_type = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "LGA",area_type),
                region = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "VIC",region),
                data_type = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "Attribute",data_type),
                main_feature = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "Population projections",main_feature),
                year = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "2031",year),
                name = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "aus_lga_vic_att_ppr_2031",name))
##
aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
  dplyr::mutate(additional_detail = ifelse(stringr::str_detect(main_feature, " 10 yr from 2006")," 10 yr from 2006",NA_character_)) %>%
  dplyr::mutate(additional_detail = ifelse(stringr::str_detect(main_feature, " for 2016 boundaries")," for 2016 boundaries",additional_detail)) %>%
  dplyr::mutate(main_feature = ifelse(stringr::str_detect(main_feature, "ERP by age and sex"),"ERP by age and sex", main_feature))
##
aus_data_resolution_tb <- tibble::tibble(area_type = c("SA1","SA2","SA3","SA4","LGA", "PHN","POA","SSC","GCCSA","ST","CED","SED"),
                                         boundary_year = c(2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2018,2018),
                                         area_count = c(57523,2310,358,107,547,31,2670,15304,8,8,150,425),
                                         complete = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE))
aus_state_short_tb <- tibble::tibble(state_territory = c("Australian Capital Territory", "New South Wales", "Northern Territory", "Queensland",
                                                         "South Australia", "Tasmania", "Victoria", "Western Australia"),
                                     short_name = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"))

par_str_environment_tb = ready.agents::par_str_environment_tb
params_struc_mape_tb = ready.aus.data::params_struc_mape_tb
#aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
#  dplyr::add_row(name = "aus_lga_vic_att_ppr_2021",
#                 country = "Australia",
#                 area_type = "LGA",
#                 region = "VIC",
#                 data_type = "Attribute",
#                 main_feature = "Population projections",
#                 year = "2021",
#                 source_reference = "ready.aus.data::aus_lga_vic_att_ppr_2016[[2]]") %>%
#  dplyr::add_row(name = "aus_lga_vic_att_ppr_2026",
#                 country = "Australia",
#                 area_type = "LGA",
#                 region = "VIC",
#                 data_type = "Attribute",
#                 main_feature = "Population projections",
#                 year = "2026",
#                 source_reference = "ready.aus.data::aus_lga_vic_att_ppr_2016[[3]]") %>%
#  dplyr::add_row(name = "aus_lga_vic_att_ppr_2031",
#                 country = "Australia",
#                 area_type = "LGA",
#                 region = "VIC",
#                 data_type = "Attribute",
#                 main_feature = "Population projections",
#                 year = "2031",
#                 source_reference = "ready.aus.data::aus_lga_vic_att_ppr_2016[[4]]") %>%
#  dplyr::mutate(source_reference = ifelse(name=="aus_lga_vic_att_ppr_2016",
#                                          "ready.aus.data::aus_lga_vic_att_ppr_2016[[1]]",
#                                          source_reference)) %>%
#  dplyr::arrange(name)
##
##
##
# safety_pref_source <- ymh.epi.lit::pref_source
# safety_prev_rates <- ymh.epi.lit::prev_rates
# usethis::use_data(safety_pref_source,
#                   overwrite = TRUE)
# usethis::use_data(safety_prev_rates,
#                   overwrite = TRUE)
group_by_var_lookup_tb <- tibble::tibble(resolution = c("SA1","SA2","SA3", "SA4","PHN","DRIVE_TIME", "GEOMETRIC_DISTANCE"),
                                    year = c("2016", "2016","2016","2016", "2016","2016","2016"),
                                    var_name = c("SA1_MAIN16","SA2_MAIN16","SA3_MAIN16","SA4_MAIN16", "PHN_NAME", "drive_times", "distance_km"))

aus_boundary_phns_sf <- ready.aus.data::aus_boundary_phns_sf
# aus_spatial_lookup_tb <- ready.space::aus_spatial_lookup_tb
# aus_data_resolution_tb <- ready.space:aus_data_resolution_tb
# aus_state_short_tb <- ready.space::aus_state_short_tb
# usethis::use_data(aus_spatial_lookup_tb,
#                    overwrite = TRUE)
example_headspace_tb <- ymh.headspace::headspace_tb %>%
  dplyr::mutate(cluster_name = "Headspace") %>%
  dplyr::rename(lat = long,
                long = lat)
usethis::use_data(aus_spatial_lookup_tb,
                  aus_data_resolution_tb,
                  aus_state_short_tb,
                  group_by_var_lookup_tb,
                  example_headspace_tb,
                  overwrite = TRUE,
                  internal = TRUE)
# usethis::use_data(aus_data_resolution_tb,
#                   overwrite = TRUE,
#                   internal = TRUE)
# usethis::use_data(aus_state_short_tb,
#                   overwrite = TRUE,
#                   internal = TRUE)
# usethis::use_data(group_by_var_lookup_tb,
#                   overwrite = TRUE,
                  # internal = TRUE)
# usethis::use_data(par_str_environment_tb,
#                   overwrite = TRUE,
#                   internal = TRUE)
# usethis::use_data(params_struc_mape_tb,
#                   overwrite = TRUE,
#                   internal = TRUE)
# usethis::use_data(aus_boundary_phns_sf,
#                   overwrite = TRUE,
#                   internal = TRUE)

