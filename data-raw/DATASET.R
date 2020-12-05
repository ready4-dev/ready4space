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
# ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Standardised Developer Tools For Retrieving and Managing Data In Projects Developed With The Ready4 Suite" %>% tools::toTitleCase(),
#                             pkg_desc_1L_chr = "ready4use provides a set of classes and methods for general data management tasks throughout the ready4 suite of tools for mental health data synthesis and modelling projects.
#   This development version of the ready4use package has been made available as part of the process of testing and documenting the package. The tools contained in this development release automate a number of tasks which MODIFY THE DIRECTORY STRUCTURE OF YOUR LOCAL MACHINE.
#   Therefore you should only trial this software if you feel confident that you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
#                             authors_prsns = c(utils::person(
#                               given = "Matthew",family = "Hamilton", email =
#                                 "matthew.hamilton@orygen.org.au",role = c("aut",
#                                                                           "cre"),comment = c(ORCID = "0000-0001-7407-9194")
#                             ),
#                             utils::person("Glen", "Wiesner", email = "Glen.Wiesner@vu.edu.au",
#                                           role = c("aut"), comment = c(ORCID = "0000-0002-0071-130X")),
#                             #person("Alexandra", "Parker", email =  "Alex.Parker@vu.edu.au", role = c("rev"), comment = c(ORCID ="0000-0002-2398-6306")),
#                             #person("Cathrine", "Mihalopoulos",email = "cathy.mihalopoulos@deakin.edu.au", role = c("rev"), comment = c(ORCID = "0000-0002-7127-9462")),
#                             #person("Jonathan", "Karnon", email ="Jonathan.Karnon@flinders.edu.au", role = c("rev"), comment =c(ORCID = "0000-0003-3220-2099")),
#                             #person("Petra","Plencnerova", email = "Petra.Plencnerova@vu.edu.au", role =c("rev"), comment = c(ORCID = "0000-0001-9698-9084")),
#                             utils::person("Orygen", role = c("cph", "fnd")),
#                             utils::person("VicHealth",role = c("fnd")),
#                             utils::person("Victoria University", role =c("fnd"))
#                             ),
#                             urls_chr = c("https://ready4-dev.github.io/ready4space/",
#                                          "https://github.com/ready4-dev/ready4space",
#                                          "https://ready4-dev.github.io/ready4/")) %>%
#   ready4fun::write_pkg_setup_fls(incr_ver_1L_lgl = F,
#                                  delete_contents_of_R_dir = T,
#                                  copyright_holders_chr = "Orygen",
#                                  check_type_1L_chr = "gh",
#                                  path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4use-logo/default.png",
#                                  github_repo = "ready4-dev/ready4use",
#                                  lifecycle_stage_1L_chr = "experimental",
#                                  badges_lup = ready4fun::badges_lup,
#                                  addl_badges_ls = list(ready4 = "development"))
##
## 3. Run scripts to create the MAKE CLASS TABLE object with the metadata about the classes we will be creating.
source("data-raw/MAKE_CLASSES_S3.R")
source("data-raw/MAKE_CLASSES_S4.R")
prototype_lup <- ready4use::prototype_lup
##
## 3. Merge the two MAKE CLASS TABLE objects and pass the merged object to the method to create the new classes and make an updated PROTOTYPE LOOKUP object.
prototype_lup <- dplyr::bind_rows(s3_classes_to_make_tb,
                                 s4_classes_to_make_tb) %>%
  ready4class::make_and_update(dev_pckg_namespace = "ready4space",
                               name_prefix = "ready4_",
                               output_dir = "R",
                               file_exists_logic = "overwrite",
                               init_class_pt_lup = prototype_lup,#ready4use::prototype_lup,
                               ignore_ns_chr = c("ready4s4"),
                               required_pckg_chr_vec = c("ready4use"), ## Need to implement new delete package logic now documenting and loading package with each new class.
                               #delete_files_pattern_chr_vec = c("^C4_","^C3_"),
                               class_in_cache_logic_chr = "overwrite")
## 4. Save a copy of the updated PROTOTYPE LOOKUP object, which now contains details about the newly created classes.
usethis::use_data(prototype_lup,overwrite = T)
## 5. Document.
usethis::use_package("dplyr")
usethis::use_package("geojsonio")
usethis::use_package("googlePolylines")
usethis::use_package("googleway")
usethis::use_package("httr")
usethis::use_package("ISOcodes")
usethis::use_package("jsonlite")
usethis::use_package("lubridate")
#usethis::use_package("lwgeom")
usethis::use_package("magrittr")
usethis::use_package("mc2d")
usethis::use_package("methods")
usethis::use_package("nnet")
usethis::use_package("osrm")
usethis::use_package("purrr")
usethis::use_dev_package("ready4class")
usethis::use_dev_package("ready4fun")
usethis::use_dev_package("ready4use")
usethis::use_package("rlang")
usethis::use_package("rmapshaper")
usethis::use_package("sf")
usethis::use_package("stats")
usethis::use_package("stringi")
usethis::use_package("stringr")
usethis::use_package("tibble")
usethis::use_package("tidyr")
usethis::use_package("units")
usethis::use_package("utils")
devtools::document()


