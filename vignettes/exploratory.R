devtools::load_all(".")
## PARAMETER MATRICES
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
## TEMP PLACE FOR FUNCTIONS
get_resolution_hierarchy <- function(data_year,
                                     resolution_tb = aus_data_resolution_tb,
                                     whole_area = TRUE){
  resolution_hierarchy <- resolution_tb  %>%
    dplyr::filter(boundary_year == data_year)
  if(whole_area){
    resolution_hierarchy <- resolution_hierarchy %>%
      dplyr::filter(complete==TRUE)
  }
  resolution_hierarchy %>%
    dplyr::arrange(dplyr::desc(area_count)) %>%
    dplyr::pull(area_type)
}
get_highest_res <- function(options_vec,
                year){
  if(!is.na(options_vec[1])){
    res_hierarchy <- get_resolution_hierarchy(as.numeric(at_time))
    res_hierarchy[min(which(res_hierarchy %in% options_vec))]
  }else
    NA
}
get_spatial_data_names <- function(at_highest_res,
                             at_time,
                             to_time = NULL,
                             at_specified_res = NULL,
                             country = "Australia",
                             state = NULL,
                             require_year_match = TRUE,
                             excl_diff_bound_yr = TRUE){ #### NEED TO WORK ON SECOND HALF
  if(excl_diff_bound_yr){
    spatial_lookup_tb <- aus_spatial_lookup_tb %>%
      dplyr::filter(is.na(additional_detail) | additional_detail != " for 2016 boundaries")
  }else
    spatial_lookup_tb <- aus_spatial_lookup_tb

  year_vec <- c(at_time,
                paste0(at_time,
                       "_",
                       stringr::str_sub(to_time,3,4)))
  lookup_tb_list <- purrr::map(at_highest_res,
                               ~ spatial_lookup_tb %>%
                                 dplyr::filter(main_feature == .x) %>%
                                 dplyr::filter(year %in% year_vec))
  data_res_vec <- purrr::map_chr(lookup_tb_list,
                                 ~ .x %>%
                                   dplyr::pull(area_type) %>%
                                   unique() %>%
                                   get_highest_res(year = at_time))
  data_unavail_for_year <-  is.na(data_res_vec)
  if(require_year_match & sum(data_unavail_for_year) > 0)
    stop("Data not available for specified year for all data requested")
  matched_year_vec <- at_highest_res[!data_unavail_for_year]
  matched_yr_lookup_tb_list <- lookup_tb_list[!data_unavail_for_year]
  matched_yr_data_res_vec <- data_res_vec[!data_unavail_for_year]
  non_matched_year_vec <- at_highest_res[is.na(data_res_vec)]
  matched_yr_lookup_tb_list <- purrr::map2(matched_yr_lookup_tb_list,
                                           matched_yr_data_res_vec,
                                           ~ .x %>%
                                             dplyr::filter(area_type == .y))
  if(!is.null(state)){
    region_lookup <- purrr::map_chr(state,
                                    ~ ready.data::data_get(data_lookup_tb = aus_state_short_tb,
                                                           lookup_reference = .,
                                                           lookup_variable = "state_territory",
                                                           target_variable = "short_name",
                                                           evaluate = FALSE))
    matched_yr_lookup_tb_list <- purrr::map2(matched_yr_lookup_tb_list,
                                             region_lookup,
                                             ~  .x %>% dplyr::filter(region %in% .y))
  }
  matched_yr_lookup_tb_list <- purrr::map(matched_yr_lookup_tb_list,
                                          ~ .x %>%
                                            dplyr::filter(year == ifelse(year_vec[2] %in% (.x %>% dplyr::pull(year)),year_vec[2],year_vec[1]))
  )
  names_of_data_vec <- purrr::map_chr(matched_yr_lookup_tb_list,
                                      ~ .x %>%
                                        dplyr::pull(name))

  if(!identical(non_matched_year_vec,character(0))){
    avail_years <-   purrr::map(non_matched_year_vec,
                                ~ spatial_lookup_tb %>%
                                  dplyr::filter(main_feature == .x) %>%
                                  dplyr::pull(year) %>%
                                  as.numeric())
    closest_years <- purrr::map(avail_years,
                                ~ .x[which(abs(.x - as.numeric(at_time)) == min(abs(.x - as.numeric(at_time))))])

    extra_names <- purrr::map2_chr(non_matched_year_vec,closest_years,
                                   ~     ready.data::data_get(data_lookup_tb = spatial_lookup_tb %>%
                                                                dplyr::filter(year == .y),
                                                              lookup_reference = .x,
                                                              lookup_variable = "main_feature",
                                                              target_variable = "name",
                                                              evaluate = FALSE))
  }
  names_of_data_vec <- c(names_of_data_vec,extra_names)
  extra_names <- purrr::map_chr(at_specified_res,
                                ~ spatial_lookup_tb %>%
                                  dplyr::filter(year %in% year_vec) %>%
                                  dplyr::filter(area_type == .x[2]) %>%
                                  ready.data::data_get(lookup_reference = .x[1],
                                                       lookup_variable = "main_feature",
                                                       target_variable = "name",
                                                       evaluate = FALSE)) %>%
    unname()
  c(names_of_data_vec,extra_names)
}
##
get_spatial_data <- function(at_highest_res,
                             at_time,
                             to_time = NULL,
                             at_specified_res = NULL,
                             country = "Australia",
                             state = NULL,
                             require_year_match = TRUE,
                             excl_diff_bound_yr = TRUE){

  attributes_to_import <- get_spatial_data_names(at_highest_res = at_highest_res,
                                       at_time = at_time,
                                       to_time = to_time,
                                       at_specified_res = at_specified_res,
                                       country = country,
                                       state = state,
                                       require_year_match =require_year_match,
                                       excl_diff_bound_yr = excl_diff_bound_yr)

  attribute_list <- purrr::map(attributes_to_import,
                               ~ ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                      lookup_reference = .,
                                                      lookup_variable = "name",
                                                      target_variable = "source_reference")) %>%
    stats::setNames(attributes_to_import)
  boundary_res <- stringr::str_sub(attributes_to_import,5,7) %>% unique() %>% toupper()
  #boundary_year <-
  boundary_list <- purrr::map(boundary_res,
                                     ~ ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb %>%
                                                 dplyr::filter(main_feature == "Boundary"),
                                               lookup_reference = .,
                                               lookup_variable = "area_type",
                                               target_variable = "source_reference"))

  boundary_list <- purrr::map(boundary_list,
                              ~ .x %>% dplyr::filter(STE_NAME16 %in% state)) %>%
    stats::setNames(boundary_res)
  ##
  vic_age_sex_seifa_sa2s_2006_2016_sf <- recur_add_attr_to_sf(country = "Australia",
                                                              state = "Victoria",
                                                              area_unit = "SA2",
                                                              boundary_year = "2016",
                                                              attribute_data = c("aus_pop_age_sex_sa2_2006_tb",
                                                                                 "aus_sa2_vic_att_erp_2016",
                                                                                 "aus_sa2_nat_att_seifa_2016"))


  vic_pop_growth_projs_sf <- recur_add_attr_to_sf(country = "Australia",
                                                  state = "Victoria",
                                                  area_unit = "LGA",
                                                  boundary_year = "2016",
                                                  attribute_data = c("aus_lga_vic_att_ppr_2016",
                                                                     "aus_lga_vic_att_ppr_2021",
                                                                     "aus_lga_vic_att_ppr_2026",
                                                                     "aus_lga_vic_att_ppr_2031"))
  vic_age_sex_acgr_lga_2016_31_sf <- gen_demog_features(profiled_sf = vic_pop_growth_projs_sf,
                                                        years = c(2016,2019,2031,2025),
                                                        age0 = 12,
                                                        age1 = 18,
                                                        #age_by_year = FALSE,
                                                        #drop_projs = TRUE,
                                                        param_tb = test_par_val_master,
                                                        it_nbr = 1)
  ##

  vic_merged_attr_sf <- intersect_sf_drop_cols(main_sf = vic_age_sex_seifa_sa2s_2006_2016_sf,
                                               adjunct_sf = vic_age_sex_acgr_lga_2016_31_sf)

  vic_merged_attr_by_age_sf <- gen_demog_features(profiled_sf = vic_merged_attr_sf,
                                                  years = c(2016,2019,2031,2025),
                                                  age0 = 12,
                                                  age1 = 18,
                                                  acgr = FALSE,
                                                  age_by_year = TRUE,
                                                  drop_bands = TRUE,
                                                  param_tb = test_par_val_master,
                                                  it_nbr = 1)
  ###

  gen_age_sex_estimates_tx(profiled_sf = vic_merged_attr_by_age_sf,
                           ymwd_step = c(22,5,2,1))

}
attributes_to_import <- get_spatial_data_names(at_highest_res = c("ERP by age and sex",
                                          "ERP",
                                          "Population projections"),
                       at_time <- "2016",
                       to_time <- "2031",
                       at_specified_res = list(a=c("SEIFA","SA2")),
                       state = "Victoria",
                       require_year_match = FALSE

                       )


##
vic_land_boundary_sf <- create_australia_land_boundary(state_territories = c("Victoria"))
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
##
vic_age_sex_seifa_sa2s_2006_2016_sf <- recur_add_attr_to_sf(country = "Australia",
                                                            state = "Victoria",
                                                            area_unit = "SA2",
                                                            boundary_year = "2016",
                                                            attribute_data = c("aus_pop_age_sex_sa2_2006_tb",
                                                                               "aus_sa2_vic_att_erp_2016",
                                                                               "aus_sa2_nat_att_seifa_2016"))


vic_pop_growth_projs_sf <- recur_add_attr_to_sf(country = "Australia",
                                                            state = "Victoria",
                                                            area_unit = "LGA",
                                                            boundary_year = "2016",
                                                            attribute_data = c("aus_lga_vic_att_ppr_2016",
                                                                               "aus_lga_vic_att_ppr_2021",
                                                                               "aus_lga_vic_att_ppr_2026",
                                                                               "aus_lga_vic_att_ppr_2031"))
vic_age_sex_acgr_lga_2016_31_sf <- gen_demog_features(profiled_sf = vic_pop_growth_projs_sf,
                                           years = c(2016,2019,2031,2025),
                                           age0 = 12,
                                           age1 = 18,
                                           #age_by_year = FALSE,
                                           #drop_projs = TRUE,
                                           param_tb = test_par_val_master,
                                           it_nbr = 1)
##

vic_merged_attr_sf <- intersect_sf_drop_cols(main_sf = vic_age_sex_seifa_sa2s_2006_2016_sf,
                       adjunct_sf = vic_age_sex_acgr_lga_2016_31_sf)

vic_merged_attr_by_age_sf <- gen_demog_features(profiled_sf = vic_merged_attr_sf,
                                                           years = c(2016,2019,2031,2025),
                                                           age0 = 12,
                                                           age1 = 18,
                                                           acgr = FALSE,
                                                           age_by_year = TRUE,
                                                           drop_bands = TRUE,
                                                           param_tb = test_par_val_master,
                                                           it_nbr = 1)
###

gen_age_sex_estimates_tx(profiled_sf = vic_merged_attr_by_age_sf,
                         ymwd_step = c(22,5,2,1))
##
#vic_pop_growth_by_age_lga_2016_2031_sf <- dplyr::in
