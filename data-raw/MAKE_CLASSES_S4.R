## Script to create the Make Classes Table from which S4 classes will be made.
##
## 1. Prerequisites
##    None
##
## 2. Make the ready4_class_make_tb object summarising the metadata about the S3 classes that we wish to create and export with this package.
s4_classes_to_make_tb <- ready4class::ready4_class_make_tb() %>%
  tibble::add_case(name_stub = "lookup", ## RENAME TO SP_LUPS
                   class_slots = c("sp_abbreviations_lup","sp_import_lup","sp_data_pack_lup","sp_resolution_lup","sp_site_coord_lup","sp_starter_sf_lup","sp_uid_lup") %>% list(),
                   prototype = c("ready4_sp_abbreviations_lup","ready4_sp_import_lup","ready4_sp_data_pack_lup","ready4_sp_resolution_lup","ready4_sp_site_coord_lup","ready4_sp_starter_sf_lup","ready4_sp_uid_lup") %>% list(),
                   class_desc = "Look up tables to use throughout readyforwhatsnext suite",
                   parent_class = NA_character_) %>%
  tibble::add_case(name_stub = "macro",
                   class_desc = "Macro level context",
                   prototype = list(c("character","character","numeric","ready4_lookup","numeric","POSIXt","POSIXt")),
                   class_slots = list(c("global_region","country","country_bound_year","lookup_tb","crs_nbr","temporal_min", "temporal_max")),
                   parent_class = NA_character_,
                   include_classes = list("ready4_lookup")) %>%
  tibble::add_case(name_stub = "meso_region",
                   class_desc = "Meso level context - region",
                   prototype = list(c("character","character","numeric")),
                   class_slots = list(c("region_type","region","region_bound_year")),
                   parent_class = "ready4_macro") %>%
  tibble::add_case(name_stub = "meso_area",
                   class_desc = "Meso level context - area",
                   prototype = list(c("character","character","numeric")),
                   class_slots = list(c("area_type","area","area_bound_year")),
                   parent_class = "ready4_meso_region") %>%
  tibble::add_case(name_stub = "micro",
                   class_desc = "Micro level context",
                   prototype = list(c("numeric","numeric","character")),
                   class_slots = list(c("geom_dist_km_cuts","travel_time_mins_cuts", "travel_mode")),
                   parent_class = "ready4_meso_area") %>%
  tibble::add_case(name_stub = "profiled_area",
                   class_desc = "Information to create a profiled area object",
                   prototype = list(c("character","logical", "numeric", "numeric", "numeric", "character","POSIXt")),
                   class_slots = list(c("features","use_coord_lup","geom_dist_limit_km", "drive_time_limit_mins", "nbr_bands", "data_year","data_ymds")),
                   parent_class = "ready4_micro") %>%
  tibble::add_case(name_stub = "env",
                   class_slots = c("st_data","env_sf","par_vals") %>% list(),
                   prototype = c("list","sf","tbl_df") %>% list(),
                   class_desc = "Spatiotemporal environment",
                   parent_class = NA_character_) %>%
  tibble::add_case(name_stub = "sp_local",
                   class_slots = c("lup_tbs_r4") %>% list(),#,"merge_with_chr_vec","raw_data_dir_chr","pckg_chr","overwrite_lgl", "save_lgl") %>% list(),
                   prototype = c("ready4_lookup") %>% list(),#,"character","character","character","logical", "logical") %>% list(),
                   class_desc = "Object defining data to be saved in local directory.",
                   parent_class = "ready4_local"#,#NA_character_,
                   #include_classes = list("ready4_local")
                   ) %>%
  tibble::add_case(name_stub = "sp_local_raw",
                   class_slots = c("lup_tbs_r4") %>% list(),#,c("save_type") %>% list(),
                   prototype = c("ready4_lookup") %>% list(),#,c("character") %>% list(),
                   #values = list(save_type ="raw"),
                   #allowed_values = list(save_type = "raw"),
                   class_desc = "Object defining data to be saved in local directory in a raw (unprocessed) format.",
                   parent_class = "ready4_local_raw"#,
                   #include_classes = list("ready4_local")
                   ) %>%
  tibble::add_case(name_stub = "sp_local_proc",
                   class_slots = c("lup_tbs_r4") %>% list(),#,c("save_type","proc_data_dir_chr","import_chr_vec","path_to_starter_sf_chr","import_this_ls") %>% list(),
                   prototype = c("ready4_lookup") %>% list(),#,c("character","character","character","character","list") %>% list(),
                   #values = list(save_type = "proc"),
                   #allowed_values = list(save_type = "proc"),
                   class_desc = "Object defining data to be saved in local directory in a processed (R) format.",
                   parent_class = "ready4_local_proc"#,
                  # include_classes = list("ready4_local")
                  ) %>%
  dplyr::mutate(make_s3 = FALSE) %>%
  ready4use::remake_ls_cols()
