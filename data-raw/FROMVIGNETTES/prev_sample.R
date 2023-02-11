# vic_lga_y_16_31 <- ready.space::spatial_vic_pop_growth_lga(
#   vic_pop_growth_by_age_lga_t0 =
#     ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
#                          match_value_xx = "vic_pop_growth_by_age_lga_2016_tb"),
#   vic_pop_growth_by_age_lga_t1 =
#     ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
#                          match_value_xx = "vic_pop_growth_by_age_lga_2031_tb"),
#   t0 ="2016",
#   t1 ="2031")
#
#
# ## Get yearly age / sex population projections for included years
#
# vic_lga_age_12_25_y_16_19_31_25 <- ready.space::demographic_by_yearly_age_sex(profiled_sf = vic_lga_y_16_31,
#                                                                                   years = c(2016,2019, 2031, 2025),
#                                                                                   age0 = 12,
#                                                                                   age1 = 18)

## Apply prevalence estimates
# prev_rates <- ymh.epi::prev_rates
# pref_source <- ymh.epi::pref_source



