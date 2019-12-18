## Script to make classes and save updated prototype table.
##
## 1. Pre-requisites
## The following files are required to be in the package's R folder:
## imp_pipe.R
## 2. Load package functions
devtools::load_all()
##
## 3. Run script to create the object with the metadata about the classes we will be creating.
source("data-raw/MAKE_CLASSES_S3.R")
source("data-raw/MAKE_CLASSES_S4.R")
##
## 4. Specify details about this package and the prefix we will use on all classes we create in this package.
name_prefix <- "ready4_"
dev_pckg_namespace <- "ready4space"
## 5. Load a prototype lookup table that contains all of the potential parent / prototype classes that we will be using.
data("class_pt_lup", package = "ready4use")
##
## 6. Remake the classes we previously created, this time using the new, preferred make_and_update method, which appends the metadata on the new classes to our instance of the ready4_class_pt_lup class.
class_pt_lup <- ready4class::make_and_update(s3_classes_to_make_tb,
                                             dev_pckg_namespace = dev_pckg_namespace,
                                             name_prefix = name_prefix,
                                             output_dir = "R",
                                             file_exists_logic = "overwrite",
                                             init_class_pt_lup = class_pt_lup)
class_pt_lup <- ready4class::make_and_update(s4_classes_to_make_tb[1:4,], ## Hadd to run each line sequentially - debugging required.
                                             dev_pckg_namespace = dev_pckg_namespace,
                                             name_prefix = name_prefix,
                                             output_dir = "R",
                                             #delete_files_pattern_chr = NA_character_,
                                             file_exists_logic = "overwrite",
                                             init_class_pt_lup = class_pt_lup,
                                             ignore_ns_chr = c("ready4s4"),
                                             class_in_cache_logic_chr = "overwrite")
## 7. Save a copy of a class prototype object with details about the newly created classes.
usethis::use_data(class_pt_lup,overwrite = T)
## 8. Document.
devtools::document()
##
prototype_tb <- class_pt_lup
# ###
# row_idx_num <- 5
# output_sub_folder = NULL
# file_exists_logic = "overwrite"
# name_stub = s4_classes_to_make_tb[[row_idx_num,2]]
# #name_prefix = name_prefix
# output_folder = "R"
# class_desc = s4_classes_to_make_tb[[row_idx_num,10]]
# parent =  if(is.na(s4_classes_to_make_tb[[row_idx_num,11]])){
#   NULL}else{
#     s4_classes_to_make_tb[[row_idx_num,11]]}
# include_classes = NULL#s4_classes_to_make_tb[[2,14]][[1]]
# parent_ns_chr = ""
# class_slots = s4_classes_to_make_tb[[row_idx_num,12]][[1]]
# type = s4_classes_to_make_tb[[row_idx_num,3]][[1]]
# meaningful_names = s4_classes_to_make_tb[[row_idx_num,13]] #[[1]] NEED TO UPDATE READER FUNCTION
# values = s4_classes_to_make_tb[[row_idx_num,6]][[1]]
# allowed_values = s4_classes_to_make_tb[[row_idx_num,7]][[1]]
# prototype_tb = class_pt_lup
# ignore_ns_chr = c(dev_pckg_namespace,"ready4s4")
# required_pckg_chr_vec = NA_character_# c("ready4s4")
# names_include = NULL
# not_same_length = NULL
# print_set_class = TRUE
# print_helper = T
# print_accessors = T
# print_validator = TRUE
# print_meaningful_names = TRUE
# delete_files_pattern_chr = "^generic.*R"
# ready4class::make_ready_s4(name_stub = name_stub,
#                           name_prefix = name_prefix,
#                           output_folder = output_folder,
#                           output_sub_folder = NULL,
#                           class_desc = class_desc,
#                           parent = parent,
#                           class_slots = class_slots,
#                           type = type,
#                           meaningful_names = meaningful_names,
#                           values = values,
#                           allowed_values = allowed_values,
#                           include_classes = include_classes,
#                           prototype_tb = prototype_tb,
#                           ignore_ns_chr = ignore_ns_chr,
#                           required_pckg_chr_vec = required_pckg_chr_vec,
#                           #ontaining_namespace = containing_namespace,
#                           names_include = names_include,
#                           not_same_length = not_same_length,
#                           print_set_class = print_set_class,
#                           print_helper = print_helper,
#                           print_accessors = print_accessors,
#                           print_validator = print_validator,
#                           print_meaningful_names = print_meaningful_names,
#                           #parent_ns_chr = "",
#                           class_in_cache_logic_chr = "overwrite")
# old_spatial_lookup_tb <- tibble::tibble(variable_name = c("aus_age_sex_seifa_lgas_sf",
#                                                           "aus_age_sex_seifa_sa2s_sf",
#                                                           "aus_boundary_lgas_sf",
#                                                           "aus_boundary_phns_sf",
#                                                           "aus_boundary_sa1s_sf",
#                                                           "aus_boundary_sa2s_sf",
#                                                           "aus_boundary_postal_sf",
#                                                           "aus_state_suburbs_sf",
#                                                           "aus_correspondences_lga_2011_2016_tb",
#                                                           "aus_pop_age_sex_lga_2011_tb",
#                                                           "aus_pop_age_sex_lga_2016_tb",
#                                                           "aus_pop_age_sex_sa2_2006_tb",
#                                                           "aus_pop_age_sex_sa2_2016_tb",
#                                                           "aus_pop_proj_sa1_2011_2016_tb",
#                                                           "aus_seifa_deciles_by_lga_2011_tb",
#                                                           "aus_seifa_deciles_by_lga_2016_tb",
#                                                           "aus_seifa_deciles_by_sa2_2016_tb",
#                                                           "vic_boundary_state_electorates_sf",
#                                                           "vic_pop_growth_by_age_lga_2016_tb",
#                                                           "vic_pop_growth_by_age_lga_2021_tb",
#                                                           "vic_pop_growth_by_age_lga_2026_tb",
#                                                           "vic_pop_growth_by_age_lga_2031_tb"),
#                                         source_reference = c("ready.aus.data::aus_age_sex_seifa_lgas_sf",
#                                                              "ready.aus.data::aus_age_sex_seifa_sa2s_sf",
#                                                              "ready.aus.data::aus_boundary_lgas_sf",
#                                                              "ready.aus.data::aus_boundary_phns_sf",
#                                                              "ready.aus.data::aus_boundary_sa1s_sf",
#                                                              "ready.aus.data::aus_boundary_sa2s_sf",
#                                                              "ready.aus.data::aus_boundary_postal_sf",
#                                                              "ready.aus.data::aus_state_suburbs_sf",
#                                                              "ready.aus.data::aus_correspondences_lga_2011_2016_tb",
#                                                              "ready.aus.data::aus_pop_age_sex_lga_2011_tb",
#                                                              "ready.aus.data::aus_pop_age_sex_lga_2016_tb",
#                                                              "ready.aus.data::aus_pop_age_sex_sa2_2006_tb",
#                                                              "ready.aus.data::aus_pop_age_sex_sa2_2016_tb",
#                                                              "ready.aus.data::aus_pop_proj_sa1_2011_2016_tb",
#                                                              "ready.aus.data::aus_seifa_deciles_by_lga_2011_tb",
#                                                              "ready.aus.data::aus_seifa_deciles_by_lga_2016_tb",
#                                                              "ready.aus.data::aus_seifa_deciles_by_sa2_2016_tb",
#                                                              "ready.aus.data::vic_boundary_state_electorates_sf",
#                                                              "ready.aus.data::vic_pop_growth_by_age_lga_2016_tb",
#                                                              "ready.aus.data::vic_pop_growth_by_age_lga_2021_tb",
#                                                              "ready.aus.data::vic_pop_growth_by_age_lga_2026_tb",
#                                                              "ready.aus.data::vic_pop_growth_by_age_lga_2031_tb"))
#
# old_spatial_lookup_tb <- old_spatial_lookup_tb %>%
#   dplyr::rename(name = variable_name)
# sp_saved_data_lookup_tb <- ready.utils::data_import_show_menu_detail() %>%
#   dplyr::select(1:7)
# sp_saved_data_lookup_tb <- rbind(sp_saved_data_lookup_tb,
#                                  sp_saved_data_lookup_tb %>%
#                                    dplyr::filter(name=="aus_lga_nat_att_erp_2011") %>%
#                                    dplyr::mutate(name="aus_lga_nat_att_erp_2011_with_2016_bound",
#                                                  main_feature = "ERP by age and sex for 2016 boundaries")) %>%
#   dplyr::arrange(name)
# sp_saved_data_lookup_tb <- sp_saved_data_lookup_tb %>%
#   dplyr::mutate(source_reference = paste0("ready.aus.data::",name))
# aus_spatial_lookup_tb <- dplyr::bind_rows(sp_saved_data_lookup_tb,
#                                           old_spatial_lookup_tb)
# aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
#   dplyr::mutate(main_feature = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "ERP by age and sex",main_feature),
#                 area_type = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "SA2",area_type))
# ###
# aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
#   dplyr::mutate(country = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "Australia", country),
#                 region = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "National", region),
#                 data_type = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "Attribute", data_type),
#                 year = ifelse(name=="aus_pop_age_sex_sa2_2006_tb", "2006", year))
# ###
# aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
#   dplyr::mutate(name = ifelse(name =="aus_lga_vic_att_ppr_2016", "aus_lga_vic_att_ppr_2016_31",name))
# aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
#   dplyr::mutate(year = ifelse(name =="aus_lga_vic_att_ppr_2016_31", "2016_31",year))
# aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
#   dplyr::mutate(country = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "Australia",country),
#                 area_type = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "LGA",area_type),
#                 region = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "VIC",region),
#                 data_type = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "Attribute",data_type),
#                 main_feature = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "Population projections",main_feature),
#                 year = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "2016",year),
#                 name = ifelse(name=="vic_pop_growth_by_age_lga_2016_tb", "aus_lga_vic_att_ppr_2016",name)) %>%
#   dplyr::mutate(country = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "Australia",country),
#                 area_type = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "LGA",area_type),
#                 region = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "VIC",region),
#                 data_type = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "Attribute",data_type),
#                 main_feature = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "Population projections",main_feature),
#                 year = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "2021",year),
#                 name = ifelse(name=="vic_pop_growth_by_age_lga_2021_tb", "aus_lga_vic_att_ppr_2021",name)) %>%
#   dplyr::mutate(country = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "Australia",country),
#                 area_type = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "LGA",area_type),
#                 region = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "VIC",region),
#                 data_type = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "Attribute",data_type),
#                 main_feature = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "Population projections",main_feature),
#                 year = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "2026",year),
#                 name = ifelse(name=="vic_pop_growth_by_age_lga_2026_tb", "aus_lga_vic_att_ppr_2026",name)) %>%
#   dplyr::mutate(country = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "Australia",country),
#                 area_type = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "LGA",area_type),
#                 region = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "VIC",region),
#                 data_type = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "Attribute",data_type),
#                 main_feature = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "Population projections",main_feature),
#                 year = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "2031",year),
#                 name = ifelse(name=="vic_pop_growth_by_age_lga_2031_tb", "aus_lga_vic_att_ppr_2031",name))
# ##
# aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
#   dplyr::mutate(additional_detail = ifelse(stringr::str_detect(main_feature, " 10 yr from 2006")," 10 yr from 2006",NA_character_)) %>%
#   dplyr::mutate(additional_detail = ifelse(stringr::str_detect(main_feature, " for 2016 boundaries")," for 2016 boundaries",additional_detail)) %>%
#   dplyr::mutate(main_feature = ifelse(stringr::str_detect(main_feature, "ERP by age and sex"),"ERP by age and sex", main_feature))
# ##
# aus_data_resolution_tb <- tibble::tibble(area_type = c("SA1","SA2","SA3","SA4","LGA", "PHN","POA","SSC","GCCSA","ST","CED","SED"),
#                                          boundary_year = c(2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2018,2018),
#                                          area_count = c(57523,2310,358,107,547,31,2670,15304,8,8,150,425),
#                                          complete = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE))
# aus_state_short_tb <- tibble::tibble(state_territory = c("Australian Capital Territory", "New South Wales", "Northern Territory", "Queensland",
#                                                          "South Australia", "Tasmania", "Victoria", "Western Australia"),
#                                      short_name = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"))
#
# par_str_environment_tb = ready.agents::par_str_environment_tb
# params_struc_mape_tb = ready.aus.data::params_struc_mape_tb
# #aus_spatial_lookup_tb <- aus_spatial_lookup_tb %>%
# #  dplyr::add_row(name = "aus_lga_vic_att_ppr_2021",
# #                 country = "Australia",
# #                 area_type = "LGA",
# #                 region = "VIC",
# #                 data_type = "Attribute",
# #                 main_feature = "Population projections",
# #                 year = "2021",
# #                 source_reference = "ready.aus.data::aus_lga_vic_att_ppr_2016[[2]]") %>%
# #  dplyr::add_row(name = "aus_lga_vic_att_ppr_2026",
# #                 country = "Australia",
# #                 area_type = "LGA",
# #                 region = "VIC",
# #                 data_type = "Attribute",
# #                 main_feature = "Population projections",
# #                 year = "2026",
# #                 source_reference = "ready.aus.data::aus_lga_vic_att_ppr_2016[[3]]") %>%
# #  dplyr::add_row(name = "aus_lga_vic_att_ppr_2031",
# #                 country = "Australia",
# #                 area_type = "LGA",
# #                 region = "VIC",
# #                 data_type = "Attribute",
# #                 main_feature = "Population projections",
# #                 year = "2031",
# #                 source_reference = "ready.aus.data::aus_lga_vic_att_ppr_2016[[4]]") %>%
# #  dplyr::mutate(source_reference = ifelse(name=="aus_lga_vic_att_ppr_2016",
# #                                          "ready.aus.data::aus_lga_vic_att_ppr_2016[[1]]",
# #                                          source_reference)) %>%
# #  dplyr::arrange(name)
# ##
# ##
# ##
# # safety_pref_source <- ymh.epi.lit::pref_source
# # safety_prev_rates <- ymh.epi.lit::prev_rates
# # usethis::use_data(safety_pref_source,
# #                   overwrite = TRUE)
# # usethis::use_data(safety_prev_rates,
# #                   overwrite = TRUE)
# group_by_var_lookup_tb <- tibble::tibble(resolution = c("SA1","SA2","SA3", "SA4","PHN","DRIVE_TIME", "GEOMETRIC_DISTANCE","UNIT_ID"),
#                                     year = c("2016", "2016","2016","2016", "2016","2016","2016","All"),
#                                     var_name = c("SA1_MAIN16","SA2_MAIN16","SA3_MAIN16","SA4_MAIN16", "PHN_NAME", "drive_times", "distance_km","pop_sp_unit_id"))
#
# aus_boundary_phns_sf <- ready.aus.data::aus_boundary_phns_sf
# # aus_spatial_lookup_tb <- ready.space::aus_spatial_lookup_tb
# # aus_data_resolution_tb <- ready.space:aus_data_resolution_tb
# # aus_state_short_tb <- ready.space::aus_state_short_tb
# # usethis::use_data(aus_spatial_lookup_tb,
# #                    overwrite = TRUE)
# example_headspace_tb <- ymh.headspace::headspace_tb %>%
#   dplyr::mutate(cluster_name = "Headspace") %>%
#   dplyr::rename(lat = long,
#                 long = lat)
# usethis::use_data(aus_spatial_lookup_tb,
#                   aus_data_resolution_tb,
#                   aus_state_short_tb,
#                   group_by_var_lookup_tb,
#                   example_headspace_tb,
#                   overwrite = TRUE,
#                   internal = TRUE)
# # usethis::use_data(aus_data_resolution_tb,
# #                   overwrite = TRUE,
# #                   internal = TRUE)
# # usethis::use_data(aus_state_short_tb,
# #                   overwrite = TRUE,
# #                   internal = TRUE)
# # usethis::use_data(group_by_var_lookup_tb,
# #                   overwrite = TRUE,
#                   # internal = TRUE)
# # usethis::use_data(par_str_environment_tb,
# #                   overwrite = TRUE,
# #                   internal = TRUE)
# # usethis::use_data(params_struc_mape_tb,
# #                   overwrite = TRUE,
# #                   internal = TRUE)
# # usethis::use_data(aus_boundary_phns_sf,
# #                   overwrite = TRUE,
# #                   internal = TRUE)
#
