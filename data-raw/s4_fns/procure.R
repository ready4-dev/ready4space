procure_VicinityProfile <- function(x, #get_group_by_var_from_VicinityProfile
                                    what_1L_chr = "grouping"){
  if(what_1L_chr == "grouping"){
    y_vicinity_identifiers = x@a_VicinityLookup@vicinity_identifiers_r3
    if(!x@use_coord_lup_lgl){
      object_xx <- procure.vicinity_identifiers(y_vicinity_identifiers,#get_group_by_var
                                                          geometry_rsl_1L_chr = x@area_type_chr,#get_group_by_var
                                                          area_bndy_yr_chr = as.character(x@area_bndy_yr_dbl))
    }else{
      if(is.na(x@geomc_dist_limit_km_dbl))
        object_xx <- "drive_times"
      else
        object_xx <- "distance_km"
      # procure.vicinity_identifiers(y_vicinity_identifiers,#get_group_by_var
      #                              geometry_rsl_1L_chr = "GEOMETRIC_DISTANCE",
      #                              area_bndy_yr_chr = as.character(x@area_bndy_yr_dbl) ## Addition - Not sure if correct.
      #                              ) ## MAY NEED REPLACING
    }
  }

  return(object_xx)
}
