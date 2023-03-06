library(ready4fun)
library(ready4use)
# library(ready4show)
# library(youthvars)
# library(scorz)
# library(specific)
library(sf)
ready4fun::write_fn_type_dirs()
#
# MANUAL STEP. Write all your functions to R files in the new "fns" directory.
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Model Spatial Attributes With Ready4",
                                 pkg_desc_1L_chr = "Tools for developing geospatial models for use with the ready4 youth mental health systems model (https://ready4-dev.github.io/ready4/).
                            This development version of the vicinity package has been made available as part of the process of testing and documenting the package. It is currently highly unstable and is not yet recommended for use.
                            If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                                 authors_prsn = c(#utils::person(given = "Caroline",family = "Gao",email = "caroline.gao@orygen.org.au", role = c("aut"),comment = c(ORCID = "0000-0002-0987-2759")),
                                                  utils::person(given = "Matthew",family = "Hamilton",email = "matthew.hamilton@orygen.org.au", role = c("aut", "cre"),comment = c(ORCID = "0000-0001-7407-9194")),
                                                  utils::person("Orygen", role = c("cph", "fnd")),
                                                  utils::person("Headspace", role = c( "fnd")),
                                                  utils::person("National Health and Medical Research Council", role = c( "fnd"))),
                                 urls_chr = c("https://ready4-dev.github.io/vicinity/",
                                              "https://github.com/ready4-dev/vicinity",
                                              "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(suggests_chr = c("rmarkdown"),
                                                                       imports_chr = c("knitrBootstrap")#, depends_chr = c("osrm") # required?
                                                                       ),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           copyright_holders_chr = "Orygen",
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(),##
                           dev_pkgs_chr = c("ready4",
                                            "ready4use"#,"ready4show",
                                            ),
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/vicinity-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "modelling",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7623630.svg)](https://doi.org/10.5281/zenodo.7623630)"
                           )
y <- ready4class::ready4class_constructor() %>%
  dplyr::bind_rows(tibble::tribble(
    ~ make_s3_lgl, ~ name_stub_chr, ~ pt_ls, ~ pt_chkr_pfx_ls, ~ pt_ns_ls, ~ vals_ls, ~ allowed_vals_ls, ~ min_max_vals_ls, ~ start_end_vals_ls, ~ class_desc_chr, ~ parent_class_chr, ~ slots_ls, ~ meaningful_nms_ls, ~inc_clss_ls,
    TRUE, "parameters",#"param_str_envir"
    list("tibble"), list("is_"), list("tibble"), list(param_name_chr = "character(0)",
                                                                               deterministic_val_dbl = "numeric(0)",#deter_val
                                                                               distribution_chr = "character(0)",#distribution
                                                                               dstr_param_1_dbl = "numeric(0)",#dstr_param_1
                                                                               dstr_param_2_dbl = "numeric(0)",#dstr_param_2
                                                                               dstr_param_3_dbl = "numeric(0)",#dstr_param_3
                                                                               dstr_param_4_dbl = "numeric(0)",#dstr_param_4
                                                                               transformation_chr = "character(0)",#transformation
                                                                               use_in_chr = "character(0)",#use_in
                                                                               source_chr = "character(0)"), #source
    NULL, NULL, NULL, "ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.", NA_character_, NULL, NULL, NULL,
    TRUE, "values",#"param_val_envir"
    list("tibble"), list("is_"), list("tibble"),list(param_name_chr = "character(0)",
                                                     iteration_1_dbl = "numeric(0)"), #v_it_1
    NULL, NULL, NULL, "ready4 S3 class for tibble object that stores simulation parameter values for each iteration.", NA_character_, NULL, NULL, NULL,
    TRUE, "abbreviations",#"sp_abbreviations_lup"
    list("tibble"), list("is_"), list("tibble"),list(long_name_chr = "character(0)",#long_name
                                                     short_name_chr = "character(0)",#short_name
                                                     type_chr = "character(0)"
                                                     ), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table for spatial data abbreviations.", NA_character_, NULL, NULL, NULL,
    TRUE, "processed",#"sp_data_pack_lup"
    list("tibble"), list("is_"), list("tibble"),list(name_chr = "character(0)",#name
                                                                               country_chr = "character(0)",#country
                                                                               area_type_chr = "character(0)",#area_type
                                                                               area_bndy_yr_chr = "character(0)",# area_bound_yr
                                                                               region_chr = "character(0)",#region
                                                                               data_type_chr = "character(0)",#data_type
                                                                               main_feature_chr = "character(0)",#main_feature
                                                                               year_chr = "character(0)",#year # IS IT NUMERIC?
                                                                               year_start_chr = "character(0)",#year_start # IS IT NUMERIC?
                                                                               year_end_chr = "character(0)",#year_end
                                                                               source_reference_chr = "character(0)",#
                                                                               additional_detail_chr = "character(0)"#
                                                     ), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).", NA_character_, NULL, NULL, NULL,
    TRUE, "raw", #"sp_import_lup"
    list("tibble"), list("is_"), list("tibble"),list(name_chr = "character(0)",#name
                                                                            country_chr = "character(0)",#country
                                                                            area_type_chr = "character(0)",#area_type
                                                                            area_bndy_yr_chr = "character(0)",#area_bound_yr
                                                                            region_chr = "character(0)",#region
                                                                            data_type_chr = "character(0)",#data_type
                                                                            main_feature_chr = "character(0)",#main_feature
                                                                            year_chr = "character(0)",#year
                                                                            year_start_chr = "character(0)",#year_start
                                                                            year_end_chr = "character(0)",#year_end
                                                                            uid_chr = "character(0)",#
                                                                            add_bndys_from_ls = "list()"#
                                                     ), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.","ready4use_imports", NULL, NULL, NULL,
    TRUE, "resolutions",#"sp_resolution_lup"
    list("tibble"), list("is_"), list("tibble"),list(parent_area_chr = "character(0)",# parent_area
                                                                                boundary_year_dbl = "numeric(0)",# boundary_year
                                                                                area_type_chr = "character(0)",# area_type
                                                                                area_count_dbl = "numeric(0)",# area_count
                                                                                complete_lgl = "logical(0)",# complete
                                                                                summed_area_dbl = "numeric(0)",# summed_area
                                                                                mean_size_dbl = "numeric(0)"# mean_size
                                                     ), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.", NA_character_, NULL, NULL, NULL,
    TRUE, "points",#"sp_site_coord_lup"
    list("tibble"), list("is_"), list("tibble"),list(service_type_chr = "character(0)",# service_type
                                                                                cluster_name_chr = "character(0)",# cluster_name
                                                                                service_name_chr = "character(0)",# service_name
                                                                                lat_dbl = "numeric(0)",# lat
                                                                                lng_dbl = "numeric(0)"# long
                                                     ), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.", NA_character_, NULL, NULL, NULL,
    TRUE, "templates",#"sp_starter_sf_lup"
    list("tibble"), list("is_"),
    list("tibble"),list(country_chr = "character(0)",#country
                        area_type_chr = "character(0)",# area_type
                        area_bndy_yr_chr = "character(0)",#area_bound_yr
                        starter_sf_nm_chr = "character(0)",#starter_sf ### NOT YET UPDATED
                        subdivision_chr = "character(0)"#sf_main_sub_div =
                        ), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.", NA_character_, NULL, NULL, NULL,
    TRUE, "identifiers",#"sp_uid_lup"
    list("tibble"), list("is_"), list("tibble"),list(spatial_unit_chr = "character(0)",#spatial_unit
                                                                         year_chr = "character(0)",#year
                                                                         var_name_chr = "character(0)"# var_name
                                                     ), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.", NA_character_, NULL, NULL, NULL
    ))
y <- ready4class::ready4class_constructor() %>%
  dplyr::bind_rows(
    y,
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = TRUE,
                                                 name_stub_chr = "mapes",
                                                 pt_ls = list(list("tibble")),
                                                 pt_chkr_pfx_ls = list(list("is_")),
                                                 pt_ns_ls = list(list("tibble")),
                                                 vals_ls = list(list(param_name_chr = "character(0)",
                                                                     var_nm_chr = "character(0)",
                                                                     mape_05_yr_mde_dbl = "numeric(0)",
                                                                     mape_10_yr_mde_dbl = "numeric(0)",
                                                                     mape_15_yr_mde_dbl = "numeric(0)",
                                                                     mape_05_yr_min_dbl = "numeric(0)",
                                                                     mape_10_yr_min_dbl = "numeric(0)",
                                                                     mape_15_yr_min_dbl = "numeric(0)",
                                                                     mape_05_yr_max_dbl = "numeric(0)",
                                                                     mape_10_yr_max_dbl = "numeric(0)",
                                                                     mape_15_yr_max_dbl = "numeric(0)",
                                                                     mape_05_yr_shp_dbl = "numeric(0)",
                                                                     mape_10_yr_shp_dbl = "numeric(0)",
                                                                     mape_15_yr_shp_dbl = "numeric(0)")),
                                                 class_desc_chr = "ready4 S3 class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors."),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "Lookup", # lookup
                                            slots_ls = c("vicinity_abbreviations_r3",#"sp_abbreviations_lup",
                                                         "vicinity_raw_r3",#"sp_import_lup",
                                                         "vicinity_processed_r3",#"sp_data_pack_lup",
                                                         "vicinity_resolutions_r3",#""sp_resolution_lup",
                                                         "vicinity_points_r3",#""sp_site_coord_lup",
                                                         "vicinity_templates_r3",#"sp_starter_sf_lup",
                                                         "vicinity_identifiers_r3"#"sp_uid_lup"
                                                         ) %>% list() %>% list(),
                                            pt_ls = c("vicinity_abbreviations",#"ready4_sp_abbreviations_lup",
                                                      "vicinity_raw",#"vicinity_raw",
                                                      "vicinity_processed",#"ready4_sp_data_pack_lup",
                                                      "vicinity_resolutions",#"vicinity_resolutions",
                                                      "vicinity_points",#"vicinity_points",
                                                      "vicinity_templates",#"vicinity_templates",
                                                      "vicinity_identifiers"#"vicinity_identifiers"
                                                      ) %>% list() %>% list(),
                                            class_desc_chr = "Look up tables for spatiotemporal data",
                                            parent_class_chr = NA_character_),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "Macro", # macro
                                            class_desc_chr = "Macro level context",
                                            pt_ls = list(c("VicinityLookup",#r$eady4_lookup, # From position 4
                                                           "character","character","numeric",
                                                           # MOVED TO FIRST
                                                           "numeric","POSIXt","POSIXt")) %>% list(),
                                            slots_ls = list(c("a_VicinityLookup",#"lookup_r3",#lookup_tb # Moved from position 4
                                                              "global_region_chr",#global_region
                                                              "country_chr",#country
                                                              "country_bndy_yr_dbl",#country_bound_year
                                                              # MOVED TO FIRST
                                                              "crs_dbl",#crs_nbr
                                                              "temporal_min_dtm",# temporal_min
                                                              "temporal_max_dtm"# temporal_max
                                                              )) %>% list(),
                                            parent_class_chr = NA_character_,
                                            inc_clss_ls = list("VicinityLookup") %>% list()),#?
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "MesoRegion", # meso_region
                                            class_desc_chr = "Meso level context - region",
                                            pt_ls = list(c("character","character","numeric")) %>% list(),
                                            slots_ls = list(c("region_type_chr",#"region_type"
                                                              "region_chr",#"region"
                                                              "region_bndy_yr_dbl"#""region_bound_year"
                                                              )) %>% list(),
                                            parent_class_chr = "VicinityMacro"), #"r$eady4_macro"
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "MesoArea",# meso_area
                                            class_desc_chr = "Meso level context - area", #
                                            pt_ls = list(c("character","character","numeric")) %>% list(),
                                            slots_ls = list(c("area_type_chr",# area_type
                                                              "area_chr",# area
                                                              "area_bndy_yr_dbl"# area_bound_year
                                                              )) %>% list(),
                                            parent_class_chr = "VicinityMesoRegion"),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "Micro",# micro
                                            class_desc_chr = "Micro level context",
                                            pt_ls = list(c("numeric","numeric","character")) %>% list(),
                                            slots_ls = list(c("geom_dist_km_cuts_dbl",# geom_dist_km_cuts
                                                              "travel_time_mins_cuts_dbl", # travel_time_mins_cuts
                                                              "travel_mode_chr" # travel_mode
                                                              )) %>% list(),
                                            parent_class_chr  = "VicinityMesoArea"),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "Profile",# profiled_area
                                            class_desc_chr = "Information to create a profiled area object",
                                            pt_ls = list(c("character","logical", "numeric", "numeric", "numeric", "character","POSIXt")) %>% list(),
                                            slots_ls = list(c("features_chr",# features
                                                              "use_coord_lup_lgl",# use_coord_lup
                                                              "geomc_dist_limit_km_dbl", # geom_dist_limit_km
                                                              "drive_time_limit_mins_dbl", # drive_time_limit_mins
                                                              "nbr_bands_dbl", # nbr_bands
                                                              "data_year_1L_chr",# data_yea
                                                              "data_ymds_dtm" # data_ymd
                                                              )) %>% list(),
                                            parent_class_chr = "VicinityMicro"),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "SpaceTime",#"env"
                                            slots_ls = c("data_ls",#""st_data"
                                                         "env_sf", #
                                                         "param_vals_tb"#"param_vals
                                                         ) %>% list() %>% list(),
                                            pt_ls = c("list","sf","tbl_df") %>% list() %>% list(),
                                            class_desc_chr = "Spatiotemporal environment",
                                            parent_class_chr = NA_character_),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                 name_stub_chr = "Arguments",
                                                 slots_ls = list("a_VicinityLookup",
                                                                 "crs_nbr_dbl") %>% list(), # Change #
                                                 pt_ls = list("VicinityLookup","numeric") %>% list(),
                                                 class_desc_chr= "Function arguments for constructing a spatial object.",
                                                 parent_class_chr = "Ready4useProcessed",
                                                 inc_clss_ls = list("VicinityLookup") %>% list()),
    # %>%
      ##
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "Local",# sp_local
                                            slots_ls = c("a_VicinityLookup"#"lup_tbs_r4"
                                                         ) %>% list() %>% list(),
                                            pt_ls = c("VicinityLookup"#"r$eady4_lookup" #
                                                      ) %>% list() %>% list(),
                                            class_desc_chr = "Object defining data to be saved in local directory.",
                                            parent_class_chr = "Ready4useFiles",
                                            inc_clss_ls = list("VicinityLookup") %>% list()),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "LocalRaw",#"spRaw"
                                            slots_ls = c("a_VicinityLookup"#"lup_tbs_r4"
                                                         ) %>% list() %>% list(),
                                            pt_ls = c("VicinityLookup"#"r$ady4_lookup"
                                                      ) %>% list() %>% list(),
                                            class_desc_chr = "Object defining data to be saved in local directory in a raw (unprocessed) format.",
                                            parent_class_chr = "Ready4useRaw",
                                            inc_clss_ls = list("VicinityLookup") %>% list()),
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "LocalProcessed", #spProcessed
                                            slots_ls = c("a_VicinityLookup"#"lup_tbs_r4"
                                            ) %>% list() %>% list(),
                                            pt_ls = c("VicinityLookup"#"r$eady4_lookup"
                                                      ) %>% list() %>% list(),
                                            class_desc_chr = "Object defining data to be saved in local directory in a processed (R) format.",
                                            parent_class_chr = "Ready4useProcessed",
                                            inc_clss_ls = list("VicinityLookup") %>% list())
    )

z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y#, pkg_ds_ls_ls = datasets_ls
                                             ) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
#usethis::use_package("sf")
ready4::write_extra_pkgs_to_actions()
ready4::write_citation_cff(packageDescription("vicinity"),
                           citation_chr = readLines("inst/CITATION"))
# usethis::use_dev_package("youthvars",
#                          type = "Imports",#D?
#                          remote = "ready4-dev/youthvars")
# usethis::use_dev_package("scorz",
#                          type = "Imports",
#                          remote = "ready4-dev/scorz")
devtools::build_vignettes()
# usethis::use_dev_package("specific",
#                          type = "Imports",
#                          remote = "ready4-dev/specific")
# usethis::use_package("readr")
# MANUAL DELETION OF TRAILING INCLUDE
# usethis::use_dev_package("ready4",
#                          type = "Depends",
#                          remote = "ready4-dev/ready4")
# usethis::use_dev_package("specific",
#                          type = "Depends",
#                          remote = "ready4-dev/scorz")
# usethis::use_package("rgl")
# piggyback::pb_new_release("ready4-dev/TTU",
#                           tag = paste0("v",desc::desc_get_version()),
#                           body = "Version implemented following significant redevelopment of package dependencies.",
#                           prerelease = F)
# devtools::build_vignettes()
