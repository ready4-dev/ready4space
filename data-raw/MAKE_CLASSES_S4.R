## Script to create the Make Classes Table from which S4 classes will be made.
##
## 1. Prerequisites
##    None
##
## 2. Make the ready4_class_make_tb object summarising the metadata about the S3 classes that we wish to create and export with this package.
s4_classes_to_make_tb <- dplyr::bind_rows(
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "lookup", # rename to sp_lups
                                              slots_ls = c("sp_abbreviations_lup","sp_import_lup","sp_data_pack_lup","sp_resolution_lup","sp_site_coord_lup","sp_starter_sf_lup","sp_uid_lup") %>% list(),
                                              pt_ls = c("ready4_sp_abbreviations_lup","ready4_sp_import_lup","ready4_sp_data_pack_lup","ready4_sp_resolution_lup","ready4_sp_site_coord_lup","ready4_sp_starter_sf_lup","ready4_sp_uid_lup") %>% list(),
                                              vals_ls = list(save_type ="raw"),
                                              allowed_vals_ls = list(save_type = "raw"),
                                              class_desc_chr = "Look up tables to use throughout ready4 suite",
                                              parent_class_chr = NA_character_),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "macro",
                                              class_desc_chr = "Macro level context",
                                              pt_ls = list(c("character","character","numeric","ready4_lookup","numeric","POSIXt","POSIXt")),
                                              slots_ls = list(c("global_region","country","country_bound_year","lookup_tb","crs_nbr","temporal_min", "temporal_max")),
                                              parent_class_chr = NA_character_,
                                              inc_clss_ls = list("ready4_lookup")),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "meso_region",
                                              class_desc_chr = "Meso level context - region",
                                              pt_ls = list(c("character","character","numeric")),
                                              slots_ls = list(c("region_type","region","region_bound_year")),
                                              parent_class_chr = "ready4_macro"),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "meso_area",
                                              class_desc_chr = "Meso level context - area",
                                              pt_ls = list(c("character","character","numeric")),
                                              slots_ls = list(c("area_type","area","area_bound_year")),
                                              parent_class_chr = "ready4_meso_region"),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "micro",
                                              class_desc_chr = "Micro level context",
                                              pt_ls = list(c("numeric","numeric","character")),
                                              slots_ls = list(c("geom_dist_km_cuts","travel_time_mins_cuts", "travel_mode")),
                                              parent_class_chr  = "ready4_meso_area"),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "profiled_area",
                                              class_desc_chr = "Information to create a profiled area object",
                                              pt_ls = list(c("character","logical", "numeric", "numeric", "numeric", "character","POSIXt")),
                                              slots_ls = list(c("features","use_coord_lup","geom_dist_limit_km", "drive_time_limit_mins", "nbr_bands", "data_year","data_ymds")),
                                              parent_class_chr = "ready4_micro"),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "env",
                                              slots_ls = c("st_data","env_sf","par_vals") %>% list(),
                                              pt_ls = c("list","sf","tbl_df") %>% list(),
                                              class_desc_chr = "Spatiotemporal environment",
                                              parent_class_chr = NA_character_),
    ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                                name_stub_chr = "sp_local",
                                                slots_ls = c("lup_tbs_r4") %>% list(),#,"merge_with_chr_vec","raw_data_dir_chr","pckg_chr","overwrite_lgl", "save_lgl") %>% list(),
                                                pt_ls = c("ready4_lookup") %>% list(),#,"character","character","character","logical", "logical") %>% list(),
                                                class_desc_chr = "Object defining data to be saved in local directory.",
                                                parent_class_chr = "ready4_local"#,#NA_character_,
                     #include_classes = list("ready4_local")
    ),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "sp_local_raw",
                                              slots_ls = c("lup_tbs_r4") %>% list(),#,c("save_type") %>% list(),
                                              pt_ls = c("ready4_lookup") %>% list(),#,c("character") %>% list(),
                                              #values = list(save_type ="raw"),
                                              #allowed_values = list(save_type = "raw"),
                                              class_desc_chr = "Object defining data to be saved in local directory in a raw (unprocessed) format.",
                                              parent_class_chr = "ready4_local_raw"#,
                     #include_classes = list("ready4_local")
    ),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "sp_local_proc",
                                              slots_ls = c("lup_tbs_r4") %>% list(),#,c("save_type","proc_data_dir_chr","import_chr_vec","path_to_starter_sf_chr","import_this_ls") %>% list(),
                                              pt_ls = c("ready4_lookup") %>% list(),#,c("character","character","character","character","list") %>% list(),
                                              #values = list(save_type = "proc"),
                                              #allowed_values = list(save_type = "proc"),
                                              class_desc_chr = "Object defining data to be saved in local directory in a processed (R) format.",
                                              parent_class_chr = "ready4_local_proc"#,
                     # include_classes = list("ready4_local")
    )


  )

