## Script to make classes and save updated prototype table.
##
## 1. Pre-requisites
## The following files are required to be in the package's R folder:
## imp_pipe.R
## 2. Load package functions
devtools::load_all()
##
## 3. Run scripts to create the MAKE CLASS TABLE object with the metadata about the classes we will be creating.
source("data-raw/MAKE_CLASSES_S3.R")
source("data-raw/MAKE_CLASSES_S4.R")
##
## 3. Merge the two MAKE CLASS TABLE objects and pass the merged object to the method to create the new classes and make an updated PROTOTYPE LOOKUP object.
prototype_lup <- dplyr::bind_rows(s3_classes_to_make_tb,
                                 s4_classes_to_make_tb) %>%
  ready4class::make_and_update(dev_pckg_namespace = "ready4space",
                               name_prefix = "ready4_",
                               output_dir = "R",
                               file_exists_logic = "overwrite",
                               init_class_pt_lup = ready4use::prototype_lup,
                               ignore_ns_chr = c("ready4s4"),
                               delete_files_pattern_chr_vec = c("^C4_","^C3_"),
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
usethis::use_package("lwgeom")
usethis::use_package("magrittr")
usethis::use_package("mc2d")
usethis::use_package("methods")
usethis::use_package("nnet")
usethis::use_package("osrm")
usethis::use_package("purrr")
usethis::use_package("rlang")
usethis::use_package("rmapshaper")
usethis::use_package("sf")
usethis::use_package("stats")
usethis::use_package("stringi")
usethis::use_package("stringr")
usethis::use_package("tibble")
usethis::use_package("tidyr")
usethis::use_package("units")
usethis::use_dev_package("ready4use")
usethis::use_dev_package("ready4utils")
devtools::document()


