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
class_pt_lup <- ready4class::make_and_update(s4_classes_to_make_tb[5:6,], ## Had to run each line sequentially - debugging required. Not finding newly created class. Needs documentation/loading within loop?
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

