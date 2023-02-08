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
y <- ready4class::ready4class_constructor() %>%
  dplyr::bind_rows(
    ready4class::make_pt_ready4_constructor(make_s3_lgl = FALSE,
                                              name_stub_chr = "Lookup", # rename to sp_lups #capitalised
                                              slots_ls = c("sp_abbreviations_lup","sp_import_lup","sp_data_pack_lup","sp_resolution_lup","sp_site_coord_lup","sp_starter_sf_lup","sp_uid_lup") %>% list() %>% list(),
                                              pt_ls = c("ready4_sp_abbreviations_lup","ready4_sp_import_lup","ready4_sp_data_pack_lup","ready4_sp_resolution_lup","ready4_sp_site_coord_lup","ready4_sp_starter_sf_lup","ready4_sp_uid_lup") %>% list() %>% list(),
                                              class_desc_chr = "Lookup Tables For Use in Geopspatial Modelling.",
                                              parent_class_chr = NA_character_),
    ready4class::make_pt_ready4_constructor(make_s3_lgl = FALSE,
                                            name_stub_chr = "Macro", #capitalised
                                            class_desc_chr = "Macro level context",
                                            pt_ls = list(c("character","character","numeric","ready4_lookup","numeric","POSIXt","POSIXt")) %>% list(),
                                            slots_ls = list(c("global_region","country","country_bound_year","lookup_tb","crs_nbr","temporal_min", "temporal_max")) %>% list(),
                                            parent_class_chr = NA_character_,
                                            inc_clss_ls = list("Ready4spaceLookup"))
    )

z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y#, pkg_ds_ls_ls = datasets_ls
                                             ) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
usethis::use_dev_package("youthvars",
                         type = "Imports",#D?
                         remote = "ready4-dev/youthvars")
usethis::use_dev_package("scorz",
                         type = "Imports",
                         remote = "ready4-dev/scorz")
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
