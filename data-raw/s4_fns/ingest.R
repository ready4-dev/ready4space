
ingest_VicinityProfile <- function(x,
                                    key_var_1L_chr = character(0),
                                    what_1L_chr = "starter"){
  if(what_1L_chr == "starter"){#get_starter_sf_for_profiled_area
    sp_data_starter_sf_lup <- x@a_VicinityLookup@vicinity_templates_r3 %>%
      dplyr::filter(country_chr == x@country_chr)
    if(!is.na(x@area_bndy_yr_dbl))
    sp_data_starter_sf_lup <- sp_data_starter_sf_lup %>%
        dplyr::filter(area_bndy_yr_chr == x@area_bndy_yr_chr)
    starter_sf_nm_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = sp_data_starter_sf_lup,
                                                     match_var_nm_1L_chr = "area_type_chr",
                                                     match_value_xx = ifelse(x@area_type_chr %in% sp_data_starter_sf_lup$area_type_chr,
                                                                             x@area_type_chr,
                                                                             x@region_type_chr#region_type(x)#"STE"#"PNT"
                                                                             ),
                                                     target_var_nm_1L_chr = "starter_sf",
                                                     evaluate_1L_lgl = FALSE)
    starter_sf <- ingest.vicinity_processed(x@a_VicinityLookup@vicinity_processed_r3,
                                            col_nm_1L_chr = "name_chr",
                                            match_value_xx = starter_sf_nm_1L_chr %>% stringr::str_sub(end=-4))
    if(x@use_coord_lup_lgl){
      starter_sf <- starter_sf %>%
        sf::`st_crs<-`(x@crs_dbl[1])
      }else{
        starter_sf <-  starter_sf %>%
          dplyr::filter(!!rlang::sym(key_var_1L_chr) %in% x@features_chr)
        }
    object_xx <- starter_sf
    }
  return(object_xx)
  }
