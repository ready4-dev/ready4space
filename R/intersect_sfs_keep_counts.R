#' @title intersect_sfs_keep_counts
#' @description FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param profiled_colref PARAM_DESCRIPTION, Default: NA
#' @param profiled_rowref PARAM_DESCRIPTION, Default: NA
#' @param attribute_sf PARAM_DESCRIPTION
#' @param attribute_unit PARAM_DESCRIPTION
#' @param data_type PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{slice}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}}
#'  \code{\link[stringr]{str_subset}}
#'  \code{\link[sf]{geos_measures}}
#'  \code{\link[units]{set_units}}
#' @rdname intersect_sfs_keep_counts
#' @export
#' @importFrom dplyr select pull slice mutate filter
#' @importFrom stringr str_which
#' @importFrom sf st_area
#' @importFrom units set_units
intersect_sfs_keep_counts <- function(profiled_sf,
                                      profiled_colref = NA,
                                      profiled_rowref = NA,
                                      attribute_sf,
                                      attribute_unit,
                                      data_type,
                                      data_year,
                                      crs_nbr_vec,
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
  # attrib_res_level_vars <- get_res_specific_vars(var_names = names(attribute_sf),
  #                                                data_type = data_type,
  #                                                data_year = data_year,
  #                                                popl_var_prefix = popl_var_prefix)
  # attribute_sf <- attribute_sf %>%
  #   dplyr::mutate(!!rlang::sym(paste0("whl_",
  #                                     attribute_unit,
  #                                     "_area")) := sf::st_area(.) %>%
  #                   units::set_units(km^2)) %>%
  #   dplyr::rename_at(dplyr::vars(attrib_res_level_vars),
  #                    dplyr::funs(paste0("whl_",
  #                                       attribute_unit,
  #                                       "_",
  #                                       .)))
  attribute_sf <- rename_vars_based_on_res(sf = attribute_sf,
                                           data_type = data_type,
                                           data_year = data_year,
                                           popl_var_prefix = popl_var_prefix,
                                           feature_nm = attribute_unit)

  profiled_sf <- intersect_lon_lat_sfs(sf_1 = profiled_sf,
                                       sf_2 = attribute_sf,
                                       crs_nbr_vec = crs_nbr_vec)
  profiled_sf <- profiled_sf %>%
    add_kmsq_area_all_features(feature_nm = "newunit") ## REDO THIS
    #dplyr::mutate(new_unit_area = sf::st_area(.))
  return(profiled_sf)
}
#' @title intersect_lon_lat_sfs
#' @description FUNCTION_DESCRIPTION
#' @param sf_1 PARAM_DESCRIPTION
#' @param sf_2 PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{geos_binary_ops}},\code{\link[sf]{st_transform}}
#' @rdname intersect_lon_lat_sfs
#' @export
#' @importFrom sf st_intersection st_transform
intersect_lon_lat_sfs <- function(sf_1,
                                  sf_2,
                                  crs_nbr_vec){
  sf::st_intersection(sf_1 %>% sf::st_transform(crs_nbr_vec[2]),
                      sf_2  %>% sf::st_transform(crs_nbr_vec[2])) %>%
    sf::st_transform(crs_nbr_vec[1]) %>%
    make_valid_new_sf()
}
#' @title make_valid_new_sf
#' @description FUNCTION_DESCRIPTION
#' @param sf PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[sf]{geos_query}},\code{\link[sf]{st_geometry_type}},\code{\link[sf]{st_collection_extract}}
#'  \code{\link[lwgeom]{valid}}
#' @rdname make_valid_new_sf
#' @export
#' @importFrom dplyr filter
#' @importFrom sf st_is_valid st_geometry_type st_collection_extract
#' @importFrom lwgeom st_make_valid
make_valid_new_sf <- function(sf){
  valid_sf <- sf %>%
    dplyr::filter(sf::st_is_valid(.))
  if(nrow(valid_sf)!=nrow(sf)){
    fixed_sf <- sf %>%
      dplyr::filter(!sf::st_is_valid(.)) %>%
      lwgeom::st_make_valid()
    valid_sf <- rbind(valid_sf, fixed_sf)
  }
  gc_sf <- valid_sf %>%
    dplyr::filter(sf::st_geometry_type(.)=="GEOMETRYCOLLECTION")
  if(nrow(gc_sf)>0){
    valid_sf <- gc_sf %>%
      sf::st_collection_extract(type = c("POLYGON")) %>%
      rbind(valid_sf %>%
              dplyr::filter(sf::st_geometry_type(.)!="GEOMETRYCOLLECTION"))
  }
  valid_sf %>%
    dplyr::filter(sf::st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    dplyr::distinct(.keep_all = T)
}
#' @title add_kmsq_area_all_features
#' @description FUNCTION_DESCRIPTION
#' @param sf PARAM_DESCRIPTION
#' @param feature_nm PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[sf]{geos_measures}}
#'  \code{\link[units]{set_units}}
#' @rdname add_kmsq_area_all_features
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom sf st_area
#' @importFrom units set_units
add_kmsq_area_all_features <- function(sf,
                                       feature_nm){
  sf %>%
    dplyr::mutate(!!rlang::sym(paste0("whl_",
                                      feature_nm,
                                      "_area")) := sf::st_area(.) %>%
                    units::set_units(km^2))
}
#' @title add_kmsq_area_by_group
#' @description FUNCTION_DESCRIPTION
#' @param sf PARAM_DESCRIPTION
#' @param group_by_var PARAM_DESCRIPTION
#' @param feature_nm PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{mutate}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[sf]{geos_measures}},\code{\link[sf]{st_geometry}}
#'  \code{\link[units]{set_units}}
#' @rdname add_kmsq_area_by_group
#' @export
#' @importFrom dplyr group_by summarise mutate ungroup
#' @importFrom rlang sym
#' @importFrom sf st_area st_set_geometry
#' @importFrom units set_units
add_kmsq_area_by_group <- function(sf,
                                   group_by_var,
                                   feature_nm){
  merge(sf,
        sf %>%
          dplyr::group_by(!!rlang::sym(group_by_var)) %>%
          dplyr::summarise() %>%
          dplyr::mutate(!!rlang::sym(paste0("whl_",
                                            feature_nm,
                                            "_area")) := sf::st_area(.) %>%
                          units::set_units(km^2)) %>%
          dplyr::ungroup() %>%
          sf::st_set_geometry(NULL))

}
#' @title rename_vars_based_on_res
#' @description FUNCTION_DESCRIPTION
#' @param sf PARAM_DESCRIPTION
#' @param feature_nm PARAM_DESCRIPTION
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
#' @seealso
#'  \code{\link[dplyr]{select_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{funs}}
#' @rdname rename_vars_based_on_res
#' @export
#' @importFrom dplyr rename_at vars funs
rename_vars_based_on_res <- function(sf,
                                     feature_nm,
                                     data_type,
                                     data_year,
                                     popl_var_prefix){
  feature_res_chr_vec <- get_res_specific_vars(var_names = names(sf),
                                               data_type = data_type,
                                               data_year = data_year,
                                               popl_var_prefix = popl_var_prefix)
  sf %>%
    dplyr::rename_at(dplyr::vars(feature_res_chr_vec),
                     dplyr::funs(paste0("whl_",
                                        feature_nm,
                                        "_",
                                        .)))
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
get_res_specific_vars <- function(var_names, # THIS NEEDS TO BE MADE A CONTEXT SPECIFIC METHOD
                                  data_type,
                                  data_year,
                                  popl_var_prefix){
  if(data_type == "age_sex"){
    res_sp_vars <- var_names[var_names %>% startsWith("AREASQKM") |
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
    res_sp_vars <-  var_names[#var_names %>% startsWith("AREASQKM") |
                                var_names %>%
                                startsWith("year_")]

  }
  if(data_type == "processed_age_sex"){
    res_sp_vars <-  var_names[var_names %>%
                                startsWith("pop_sp_unit_area") |
                                var_names %>%
                                startsWith(popl_var_prefix)]
  }
  return(res_sp_vars)
}
