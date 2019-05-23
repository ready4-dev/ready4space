#' @title intersect_sfs_keep_counts
#' @details This function:
#' @param profiled_sf PARAM_DESCRIPTION
#' @param profiled_colref PARAM_DESCRIPTION, Default: NA
#' @param profiled_rowref PARAM_DESCRIPTION, Default: NA
#' @param attribute_sf PARAM_DESCRIPTION
#' @param attribute_unit PARAM_DESCRIPTION
#' @param data_type PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param popl_var_prefix PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{slice}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{funs}},\code{\link[dplyr]{filter}}
#'  \code{\link[stringr]{str_subset}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[sf]{geos_measures}},\code{\link[sf]{geos_binary_ops}}
#'  \code{\link[units]{set_units}}
#' @rdname intersect_sfs_keep_counts
#' @export
#' @importFrom dplyr select pull slice mutate rename_at vars funs filter
#' @importFrom stringr str_which
#' @importFrom rlang sym
#' @importFrom sf st_area st_intersection
#' @importFrom units set_units
intersect_sfs_keep_counts <- function(profiled_sf,
                                      profiled_colref = NA,
                                      profiled_rowref = NA,
                                      attribute_sf,
                                      attribute_unit,
                                      data_type,
                                      data_year,
                                      popl_var_prefix = NULL){
  if(!is.na(profiled_colref)){
    if(!is.na(profiled_rowref)){
      profiled_sf <- profiled_sf %>%
        dplyr::select(!!profiled_colref)
      index.nbr <- profiled_sf %>%
        dplyr::pull(profiled_colref) %>%
        stringr::str_which(profiled_rowref)
      profiled_sf <- profiled_sf %>%
        dplyr::slice(index.nbr)
    }else
      profiled_sf <- profiled_sf %>%
        dplyr::select(!!profiled_colref)
  }
  attrib_res_level_vars <- get_res_specific_vars(var_names = names(attribute_sf),
                                                 data_type = data_type,
                                                 data_year = data_year,
                                                 popl_var_prefix = popl_var_prefix)
  attribute_sf <- attribute_sf %>%
    dplyr::mutate(!!rlang::sym(paste0("whl_",
                                      attribute_unit,
                                      "_area")) := sf::st_area(.) %>%
                    units::set_units(km^2)) %>%
    dplyr::rename_at(dplyr::vars(attrib_res_level_vars),
                     dplyr::funs(paste0("whl_",
                                 attribute_unit,
                                 "_",
                                 .)))
  profiled_sf <- sf::st_intersection(profiled_sf,
                                     attribute_sf)
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(new_unit_area = sf::st_area(.)) %>%
    dplyr::filter(new_unit_area != units::set_units(0,m^2))
  # if(data_type != "processed_age_sex"){
  #   profiled_sf <- profiled_sf %>%
  #     dplyr::mutate(geom_type = sf::st_geometry_type(.)) %>%
  #     dplyr::filter(geom_type %in% c("POLYGON","MULTIPOLYGON"))
  # }
  return(profiled_sf)
}
#' @title intersect_sf_drop_cols
#' @description FUNCTION_DESCRIPTION
#' @param main_sf PARAM_DESCRIPTION
#' @param adjunct_sf PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_subset}}
#'  \code{\link[sf]{geos_binary_ops}},\code{\link[sf]{st_geometry_type}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}}
#' @rdname intersect_sf_drop_cols
#' @export
#' @importFrom stringr str_which
#' @importFrom sf st_intersection st_geometry_type
#' @importFrom dplyr select mutate filter
intersect_sf_drop_cols <- function(main_sf,
                                   adjunct_sf){
  drop_names <- names(main_sf)[names(main_sf) %in% names(adjunct_sf)]
  drop_names <- drop_names[-stringr::str_which(drop_names,"geometry")]
  sf::st_intersection(main_sf,
                      adjunct_sf %>%
                        dplyr::select(-drop_names)) %>%
    dplyr::mutate(geom_type = sf::st_geometry_type(.)) %>%
    dplyr::filter(geom_type %in% c("POLYGON","MULTIPOLYGON"))
}
## NB WILL GET ATTRIBUTE RESOLUTION SPECIFIC VARS FROM FUTURE READY_SP_INPUT DATA CLASS OBJECT
#' @title get_res_specific_vars
#' @description FUNCTION_DESCRIPTION
#' @param var_names PARAM_DESCRIPTION
#' @param data_type PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param popl_var_prefix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_res_specific_vars
#' @export

get_res_specific_vars <- function(var_names,
                                  data_type,
                                  data_year,
                                  popl_var_prefix){
  if(data_type == "age_sex"){
    res_sp_vars <- var_names[var_names %>%
                               startsWith("AREASQKM") |
                               var_names %>%
                               startsWith(paste0("y",
                                                 data_year,
                                                 ".Females.")) |
                               var_names %>%
                               startsWith(paste0("y",
                                                 data_year,
                                                 ".Males.")) |
                               var_names %>%
                               startsWith(paste0("y",
                                                 data_year,
                                                 ".total")) |
                               var_names %>%
                               startsWith("seifa.percentile")]
  }
  if(data_type == "tot_pop"){
    res_sp_vars <-  var_names[var_names %>%
                                startsWith("AREASQKM") |
                                var_names %>%
                                startsWith("year_")]

  }
  if(data_type == "processed_age_sex"){
    res_sp_vars <-  var_names[
      var_names %>%
                                startsWith("pop_sp_unit_area") |
                                var_names %>%
                                startsWith(popl_var_prefix)]
  }
  return(res_sp_vars)
}
