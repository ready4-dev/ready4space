## Script to make classes and save updated prototype table.
##
## NOTE: AFTER RUNNING THE BELOW SCRIPT I IMPLEMENTED SOME MANUAL CHANGES TO THE ready4_sp_local* CLASSES TO
## ALLOW FOR MULTIPLE INHERITENCE. THIS IS A TEMPORARY FIX - ONCE READY4CLASS IS UPDATED TO MANAGE MULTIPLE
## INHERITENCE, I WILL RERUN SCRIPT AND NO MANUAL EDIT WILL BE REQUIRED.
##
## Script to make classes and save updated prototype table.
## This script creates the data files embedded with this package.
# 1. Load magrittr package to that the pipe operator ("%>%") can be used in this script.
library(magrittr)
#
# if(!dir.exists("man/figures"))
#   dir.create("man/figures")
# 2. Create "fns", "gnrcs" and "mthds" sub-directories.
ready4fun::write_fn_type_dirs()
#
# 3. MANUAL STEP. Write all your functions to R files in the new "fns" directory.
#
# 4. Set-up package structure
ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Standardised Modeller Tools For Retrieving, Managing and Synthesising Spatial Data In Projects Developed With The Ready4 Suite",
                            pkg_desc_1L_chr = "ready4space provides a set of classes and methods for spatial data management tasks throughout the ready4 suite of tools for mental health data synthesis and modelling projects.
  This development version of the ready4space package has been made available as part of the process of testing and documenting the package. The tools contained in this development release automate a number of tasks which MODIFY THE DIRECTORY STRUCTURE OF YOUR LOCAL MACHINE.
  Therefore you should only trial this software if you feel confident that you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                            authors_prsns = c(utils::person(
                              given = "Matthew",family = "Hamilton", email =
                                "matthew.hamilton@orygen.org.au",role = c("aut",
                                                                          "cre"),comment = c(ORCID = "0000-0001-7407-9194")
                            ),
                            utils::person("Glen", "Wiesner", email = "Glen.Wiesner@vu.edu.au",
                                          role = c("aut"), comment = c(ORCID = "0000-0002-0071-130X")),
                            #person("Alexandra", "Parker", email =  "Alex.Parker@vu.edu.au", role = c("rev"), comment = c(ORCID ="0000-0002-2398-6306")),
                            #person("Cathrine", "Mihalopoulos",email = "cathy.mihalopoulos@deakin.edu.au", role = c("rev"), comment = c(ORCID = "0000-0002-7127-9462")),
                            #person("Jonathan", "Karnon", email ="Jonathan.Karnon@flinders.edu.au", role = c("rev"), comment =c(ORCID = "0000-0003-3220-2099")),
                            #person("Petra","Plencnerova", email = "Petra.Plencnerova@vu.edu.au", role =c("rev"), comment = c(ORCID = "0000-0001-9698-9084")),
                            utils::person("Orygen", role = c("cph", "fnd")),
                            utils::person("VicHealth",role = c("fnd")),
                            utils::person("Victoria University", role =c("fnd"))
                            ),
                            urls_chr = c("https://ready4-dev.github.io/ready4space/",
                                         "https://github.com/ready4-dev/ready4space",
                                         "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::write_pkg_setup_fls(incr_ver_1L_lgl = F,
                                 delete_contents_of_R_dir = T,
                                 copyright_holders_chr = "Orygen",
                                 check_type_1L_chr = "gh",
                                 path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4space-logo/default.png",
                                 github_repo = "ready4-dev/ready4space",
                                 lifecycle_stage_1L_chr = "experimental",
                                 badges_lup = ready4fun::badges_lup,
                                 addl_badges_ls = list(ready4 = "development"))
##
## 3. Run scripts to create the MAKE CLASS TABLE object with the metadata about the classes we will be creating.
source("data-raw/MAKE_CLASSES_S3.R")
source("data-raw/MAKE_CLASSES_S4.R")
classes_to_make_tb <- dplyr::bind_rows(s3_classes_to_make_tb, s4_classes_to_make_tb)
##
## 3. Merge the two MAKE CLASS TABLE objects and pass the merged object to the method to create the new classes and make an updated PROTOTYPE LOOKUP object.
# prototype_lup <- dplyr::bind_rows(s3_classes_to_make_tb,
#                                  s4_classes_to_make_tb)
name_pfx_1L_chr <- "ready4_"

pkg_dss_tb <- ready4fun::write_abbr_lup(short_name_chr = c(paste0(name_pfx_1L_chr,classes_to_make_tb$name_stub_chr)),
                                        long_name_chr = c(classes_to_make_tb$class_desc_chr),
                                        # no_plural_chr = c(),
                                        #custom_plural_ls = list(utility = "utilities"),
                                        url_1L_chr = NA_character_,
                                        seed_lup = map2aqol::abbreviations_lup) # CHANGE
utils::data("abbreviations_lup")
pkg_dss_tb <- classes_to_make_tb %>%
  ready4class::write_classes_and_make_lup(dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(),
                                          name_pfx_1L_chr = name_pfx_1L_chr,
                                          output_dir_1L_chr = "R",
                                          file_exists_cdn_1L_chr = "overwrite",
                                          abbreviations_lup = abbreviations_lup,
                                          init_class_pt_lup = ready4use::prototype_lup)  %>% # UPDATE TO READY4U WHEN CLASSES ARE ADDED
  ready4fun::write_and_doc_ds(db_1L_chr = "prototype_lup",
                              title_1L_chr = "Class prototype lookup table",
                              desc_1L_chr = "Metadata on classes used in ready4 suite",
                              pkg_dss_tb = pkg_dss_tb)
# 5. Create function types and generics look-up tables
# 5.1 Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("fn_type_lup_tb",package = "ready4u")
# fn_type_lup_tb %>%
#   ready4fun::write_dmtd_fn_type_lup(url_1L_chr = NA_character_,
#                                     abbreviations_lup = abbreviations_lup,
#                                     pkg_dss_tb = pkg_dss_tb)
# data("fn_type_lup_tb")
# utils::data("fn_type_lup_tb",package = "ready4use")
pkg_dss_tb <- fn_type_lup_tb %>%
  ready4fun::add_rows_to_fn_type_lup(fn_type_nm_chr = ready4fun::get_new_fn_types(abbreviations_lup = abbreviations_lup,
                                                                                  fn_type_lup_tb = fn_type_lup_tb),
                                     fn_type_desc_chr = c("Adds a column to a data-frame type object for paths to data.",
                                                          "Binds rows to data-frame type objects.",
                                                          "Performs a validity check.",
                                                          "Downloads data.",
                                                          "Generates values for a specified varaible or variables.",
                                                          "Generates values for a specified parameter or parameter set.",
                                                          "Retrieves existing data from an object.",
                                                          "Imports data into R.",
                                                          "Gets the intersection between two or more data objects.",
                                                          "Creates a data pack object.",
                                                          "Creates multiple data pack objects.",
                                                          "Makes a manifest of data to import",
                                                          "Makes a tibble containing the set intersection between two data objects.",
                                                          "Orders a tibble object.",
                                                          "Changes ordering of an object.",
                                                          "Modifies the shape of a data object.",
                                                          "Samples values from specified distributions.",
                                                          "Writes a copy of a file in its raw (non-R) format.",
                                                          "Simplifies an object.",
                                                          "Subsets and object.",
                                                          "Unions objects."),
                                     is_generic_lgl = F,
                                     is_method_lgl = F) %>% # Add to ready4fun template.
  dplyr::arrange(fn_type_nm_chr) %>%
  ready4fun::write_dmtd_fn_type_lup(url_1L_chr = NA_character_,
                                    abbreviations_lup = abbreviations_lup,
                                    pkg_dss_tb = pkg_dss_tb)
utils::data("fn_type_lup_tb")

#
# 6. Create a table of all functions to document
fns_dmt_tb <- ready4fun::make_dmt_for_all_fns(paths_ls = ready4fun::make_fn_nms()[1],
                                              undocumented_fns_dir_chr = ready4fun::make_undmtd_fns_dir_chr()[1],
                                              custom_dmt_ls = list(details_ls = NULL,
                                                                   inc_for_main_user_lgl_ls = list(force_true_chr = c("add_aqol6d_items_to_tbs_ls","add_aqol_scores_tbs_ls",
                                                                                                                      "add_aqol6dU_to_aqol6d_items_tb","add_aqol6dU_to_tbs_ls",
                                                                                                                      "add_cors_and_uts_to_tbs_ls_ls","add_dim_disv_to_aqol6d_items_tb",
                                                                                                                      "add_dim_scores_to_aqol6d_items_tb", "add_unwtd_dim_tots",
                                                                                                                      "add_itm_disv_to_aqol6d_itms_tb","add_labels_to_aqol6d_tb",
                                                                                                                      "add_uids_to_tbs_ls","calculate_aqol6d_dim_1_disv",
                                                                                                                      "calculate_aqol6d_dim_2_disv","calculate_aqol6d_dim_3_disv",
                                                                                                                      "calculate_aqol6d_dim_4_disv","calculate_aqol6d_dim_5_disv",
                                                                                                                      "calculate_aqol6d_dim_6_disv","calculate_adult_aqol6dU",
                                                                                                                      "extract_guide_box_lgd","force_min_max_and_int_cnstrs",
                                                                                                                      "force_vec_to_sum_to_int","impute_adult_aqol6d_items_tb",
                                                                                                                      #"make_aqol_items_props_tbs_ls",
                                                                                                                      "make_aqol6d_fns_ls",
                                                                                                                      "make_aqol6d_items_tb", "make_correlated_data_tb",
                                                                                                                      "make_corstars_tbl_xx","make_dim_sclg_cons_dbl",
                                                                                                                      "make_domain_items_ls","make_item_wrst_wghts_ls_ls",
                                                                                                                      "make_pdef_cor_mat_mat", "make_synth_series_tbs_ls",
                                                                                                                      "make_vec_with_sum_of_int", "randomise_changes_in_fct_levs",
                                                                                                                      "reorder_tbs_for_target_cors","replace_with_missing_vals",
                                                                                                                      "scramble_xx","transform_raw_aqol_tb_to_aqol6d_tb",
                                                                                                                      "write_results_to_csv"),
                                                                                                   force_false_chr = NA_character_),
                                                                   args_ls_ls = NULL),
                                              fn_type_lup_tb = fn_type_lup_tb,
                                              abbreviations_lup = abbreviations_lup)
pkg_dss_tb <- fns_dmt_tb %>%
  ready4fun::write_and_doc_ds(db_1L_chr = "fns_dmt_tb",
                              title_1L_chr = "map2aqol function documentation table",
                              desc_1L_chr = "Meta-data on each map2aqol function used to create package documentation",
                              url_1L_chr = "https://ready4-dev.github.io/map2aqol/",
                              abbreviations_lup = abbreviations_lup,
                              pkg_dss_tb = pkg_dss_tb)
##
ready4fun::write_and_doc_fn_fls(fns_dmt_tb,
                                r_dir_1L_chr = "R",
                                dev_pkgs_chr = c("ready4fun","ready4use"),
                                update_pkgdown_1L_lgl = T,
                                path_to_dvpr_dmt_dir_1L_chr = "../../../../../Documentation/Code/Developer",
                                path_to_user_dmt_dir_1L_chr = "../../../../../Documentation/Code/User")
