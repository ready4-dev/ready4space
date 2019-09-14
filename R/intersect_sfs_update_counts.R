#' intersect_sfs_update_counts
#' Create a simple features object of two intersecting areas and adjust population counts by
#' fraction of spatial unit included in a profiled area.
#' @param profiled_sf PARAM_DESCRIPTION
#' @param profiled_colref PARAM_DESCRIPTION, Default: NA
#' @param profiled_rowref PARAM_DESCRIPTION, Default: NA
#' @param sp_data_list PARAM_DESCRIPTION
#' @param tot_pop_resolution PARAM_DESCRIPTION
#' @param age_sex_pop_resolution PARAM_DESCRIPTION
#' @param group_by_var PARAM_DESCRIPTION
#' @param age_sex_counts_grouped_by PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{funs}}
#' @rdname intersect_sfs_update_counts
#' @export
#' @importFrom dplyr rename_at vars one_of funs
intersect_sfs_update_counts <- function(profiled_sf,
                                        profiled_colref = NA,
                                        profiled_rowref = NA,
                                        sp_data_list,
                                        tot_pop_resolution,
                                        age_sex_pop_resolution,
                                        group_by_var,
                                        age_sex_counts_grouped_by,
                                        data_year,
                                        crs_nbr_vec){
   if(!is.null(tot_pop_resolution)){
    if(age_sex_counts_grouped_by %in% names(sp_data_list[[tot_pop_resolution]])){
      sp_data_list[[age_sex_pop_resolution]] <- merge(sp_data_list[[tot_pop_resolution]],
                                                      sf::st_set_geometry(sp_data_list[[age_sex_pop_resolution]],NULL),
                                                      by = age_sex_counts_grouped_by) %>%
        dplyr::distinct(.keep_all = T) %>%
        dplyr::select(-dplyr::ends_with(".x")) %>%
        dplyr::rename_at(.vars = dplyr::vars(dplyr::ends_with(".y")),
                         ~ stringi::stri_replace_last_regex(.x,"\\.y$",""))
      sp_data_list[[age_sex_pop_resolution]] <- rename_vars_based_on_res(sf = sp_data_list[[age_sex_pop_resolution]],
                                                                         data_type = "tot_pop",
                                                                         data_year = data_year,
                                                                         feature_nm = tot_pop_resolution) %>%
        add_kmsq_area_all_features(feature_nm = tot_pop_resolution) %>%
        add_kmsq_area_by_group(group_by_var = age_sex_counts_grouped_by,
                               feature_nm = age_sex_pop_resolution)
    }
    }
  profiled_sf <- intersect_sfs_keep_counts(profiled_sf = profiled_sf,
                                           profiled_colref = profiled_colref,
                                           profiled_rowref = profiled_rowref,
                                           attribute_sf = sp_data_list[[age_sex_pop_resolution]],
                                           attribute_unit = age_sex_pop_resolution,
                                           data_type = "age_sex",
                                           data_year = data_year,
                                           crs_nbr_vec = crs_nbr_vec)

  if(!is.null(tot_pop_resolution)){
    if(!age_sex_counts_grouped_by %in% names(sp_data_list[[tot_pop_resolution]])){
      profiled_sf <- intersect_sfs_keep_counts(profiled_sf = profiled_sf,
                                               profiled_colref = profiled_colref,
                                               profiled_rowref = profiled_rowref,
                                               attribute_sf = sp_data_list[[tot_pop_resolution]],
                                               attribute_unit = tot_pop_resolution,
                                               data_type = "tot_pop")
    }
  }
  profiled_sf <- update_pop_count_by_areas(profiled_sf = profiled_sf,
                                           group_by_var = group_by_var,
                                           age_sex_var_name = age_sex_counts_grouped_by,
                                           data_year = data_year,
                                           age_sex_pop_resolution = age_sex_pop_resolution,
                                           tot_pop_resolution = tot_pop_resolution)

  return(profiled_sf)
}


