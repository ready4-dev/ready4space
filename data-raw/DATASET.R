library(ready4fun)
library(ready4use)
library(ready4show)
library(youthvars)
library(scorz)
library(specific)
ready4fun::write_fn_type_dirs()
# MANUAL STEP. Write all your functions to R files in the new "fns" directory.
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Model Spatial Attributes With Ready4",
                                 pkg_desc_1L_chr = "Tools for developing geospatial models for use with the ready4 youth mental health systems model (https://ready4-dev.github.io/ready4/).
                            This development version of the ready4space package has been made available as part of the process of testing and documenting the package. It is currently highly unstable and is not yet recommended for use.
                            If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                                 authors_prsn = c(utils::person(given = "Caroline",family = "Gao",email = "caroline.gao@orygen.org.au", role = c("aut"),comment = c(ORCID = "0000-0002-0987-2759")),
                                                  utils::person(given = "Matthew",family = "Hamilton",email = "matthew.hamilton@orygen.org.au", role = c("aut", "cre"),comment = c(ORCID = "0000-0001-7407-9194")),
                                                  utils::person("Orygen", role = c("cph", "fnd")),
                                                  utils::person("Headspace", role = c( "fnd")),
                                                  utils::person("National Health and Medical Research Council", role = c( "fnd"))),
                                 urls_chr = c("https://ready4-dev.github.io/ready4space/",
                                              "https://github.com/ready4-dev/ready4space",
                                              "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(suggests_chr = c("rmarkdown"),
                                                                       imports_chr = c("knitrBootstrap")),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           copyright_holders_chr = "Orygen",
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(),##
                           dev_pkgs_chr = c("ready4",
                                            "ready4use"#,"ready4show",
                                            ),
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4space-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "modelling"#, zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5646593.svg)](https://doi.org/10.5281/zenodo.5646593)"
                           )

###
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
    NULL, NULL, NULL, "ready4 S3 class for tibble object that stores simulation parameter values for each iteration.", NA_character_, NULL, NULL, NULL#,
    # TRUE, "abbreviations",#"sp_abbreviations_lup"
    # list("tibble"), list("is_"), list("tibble"),list(long_name = "character(0)",
    #                                                                                short_name = "character(0)"), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table for spatial data abbreviations.", NA_character_, NULL, NULL, NULL,
    # TRUE, "processed",#"sp_data_pack_lup"
    # list("tibble"), list("is_"), list("tibble"),list(name = "character(0)",
    #                                                                            country = "character(0)",
    #                                                                            area_type = "character(0)",
    #                                                                            area_bound_yr = "character(0)",
    #                                                                            region = "character(0)",
    #                                                                            data_type = "character(0)",
    #                                                                            main_feature = "character(0)",
    #                                                                            year = "character(0)",
    #                                                                            year_start = "character(0)",
    #                                                                            year_end = "character(0)",
    #                                                                            source_reference = "character(0)",
    #                                                                            additional_detail = "character(0)"), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).", NA_character_, NULL, NULL, NULL,
    # TRUE, "raw", #"sp_import_lup"
    # list("tibble"), list("is_"), list("tibble"),list(name = "character(0)",
    #                                                                         country = "character(0)",
    #                                                                         area_type = "character(0)",
    #                                                                         area_bound_yr = "character(0)",
    #                                                                         region = "character(0)",
    #                                                                         data_type = "character(0)",
    #                                                                         main_feature = "character(0)",
    #                                                                         year = "character(0)",
    #                                                                         year_start = "character(0)",
    #                                                                         year_end = "character(0)",
    #                                                                         uid = "character(0)",
    #                                                                         add_boundaries = "list()"), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.","ready4_imports", NULL, NULL, NULL,
    # TRUE, "resolutions",#"sp_resolution_lup"
    # list("tibble"), list("is_"), list("tibble"),list(parent_area = "character(0)",
    #                                                                             boundary_year = "numeric(0)",
    #                                                                             area_type = "character(0)",
    #                                                                             area_count = "numeric(0)",
    #                                                                             complete = "logical(0)",
    #                                                                             summed_area = "numeric(0)",
    #                                                                             mean_size = "numeric(0)"), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.", NA_character_, NULL, NULL, NULL,
    # TRUE, "points",#"sp_site_coord_lup"
    # list("tibble"), list("is_"), list("tibble"),list(service_type = "character(0)",
    #                                                                             cluster_name = "character(0)",
    #                                                                             service_name = "character(0)",
    #                                                                             lat = "numeric(0)",
    #                                                                             long = "numeric(0)"), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.", NA_character_, NULL, NULL, NULL,
    # TRUE, "template",#"sp_starter_sf_lup"
    # list("tibble"), list("is_"),
    # list("tibble"),list(country = "character(0)",
    #                                                                             area_type = "character(0)",
    #                                                                             area_bound_yr = "character(0)",
    #                                                                             starter_sf = "character(0)",
    #                                                                             subdivision_chr = #sf_main_sub_div =
    #                       "character(0)"), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.", NA_character_, NULL, NULL, NULL,
    # TRUE, "identifiers",#"sp_uid_lup"
    # list("tibble"), list("is_"), list("tibble"),list(spatial_unit = "character(0)",
    #                                                                      year = "character(0)",
    #                                                                      var_name = "character(0)"), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.", NA_character_, NULL, NULL, NULL
    ))

###

y <- ready4class::ready4class_constructor() %>%
  dplyr::bind_rows(
    y,
    ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "env",
                                            slots_ls = c("data_ls",#""st_data"
                                                         "env_sf",
                                                         "param_vals_tb"#"param_vals
                                                         ) %>% list() %>% list(),
                                            pt_ls = c("list","sf","tbl_df") %>% list() %>% list(),
                                            class_desc_chr = "Spatiotemporal environment",
                                            parent_class_chr = NA_character_)

    )

z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y#, pkg_ds_ls_ls = datasets_ls
                                             ) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
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
