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
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{slice}}
#'  \code{\link[stringr]{str_subset}}
#' @rdname intersect_sfs_keep_counts
#' @export
#' @importFrom dplyr select pull slice
#' @importFrom stringr str_which
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
  # profiled_sf <- profiled_sf %>%
  #   add_kmsq_area_all_features(feature_nm = "new_unit",
  #                              prefix = "") ## REDO THIS
    #dplyr::mutate(new_unit_area = sf::st_area(.))
  return(profiled_sf)
}
#' @title intersect_lon_lat_sfs
#' @description FUNCTION_DESCRIPTION
#' @param sf_1 PARAM_DESCRIPTION
#' @param sf_2 PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
#' @param validate_lgl PARAM_DESCRIPTION, Default: T
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
                                  crs_nbr_vec,
                                  validate_lgl = T){
  sf_3 <- sf::st_intersection(sf_1 %>% sf::st_transform(crs_nbr_vec[2]),
                      sf_2  %>% sf::st_transform(crs_nbr_vec[2])) %>%
    sf::st_transform(crs_nbr_vec[1])
  if(validate_lgl)
    sf_3 %>% make_valid_new_sf()
  else
    sf_3
}

#' @title join_lon_lat_sfs
#' @description FUNCTION_DESCRIPTION
#' @param polys_sf PARAM_DESCRIPTION
#' @param points_sf PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
#' @param validate_lgl PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_join}},\code{\link[sf]{st_transform}}
#' @rdname join_lon_lat_sfs
#' @export
#' @importFrom sf st_join st_transform
join_lon_lat_sfs <- function(polys_sf,
                             points_sf,
                             crs_nbr_vec,
                             validate_lgl = T){
  sf_3 <- sf::st_join(polys_sf %>% sf::st_transform(crs_nbr_vec[2]),
                              points_sf  %>% sf::st_transform(crs_nbr_vec[2])) %>%
    sf::st_transform(crs_nbr_vec[1])
  if(validate_lgl)
    sf_3 %>% make_valid_new_sf()
  else
    sf_3
}

#' @title get_set_diff_lon_lat_sf
#' @description FUNCTION_DESCRIPTION
#' @param profile_sf PARAM_DESCRIPTION
#' @param cut_sf PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
#' @param validate_lgl PARAM_DESCRIPTION, Default: T
#' @param min_poly_area_dbl PARAM_DESCRIPTION, Default: units::set_units(0.05, km^2)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[units]{set_units}}
#'  \code{\link[sf]{geos_binary_ops}},\code{\link[sf]{st_transform}},\code{\link[sf]{geos_combine}},\code{\link[sf]{st_cast}},\code{\link[sf]{geos_measures}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{n}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#' @rdname get_set_diff_lon_lat_sf
#' @export
#' @importFrom units set_units
#' @importFrom sf st_difference st_transform st_union st_cast st_area
#' @importFrom dplyr mutate n pull filter select
#' @importFrom purrr map map_dfr
get_set_diff_lon_lat_sf <- function(profile_sf,
                                    cut_sf,
                                    crs_nbr_vec,
                                    validate_lgl = T,
                                    min_poly_area_dbl = units::set_units(0.05,km^2)){
  new_sf <- sf::st_difference(profile_sf %>% sf::st_transform(crs = crs_nbr_vec[2]),
                              sf::st_union(cut_sf) %>% sf::st_transform(crs = crs_nbr_vec[2])) %>%
    sf::st_transform(crs = crs_nbr_vec[1])
  if(validate_lgl)
    new_sf <-  new_sf %>% make_valid_new_sf()
  new_sf <-  new_sf %>%
    dplyr::mutate(feature_idx_int = 1:dplyr::n())
  new_ls <- purrr::map(dplyr::pull(new_sf,
                                   feature_idx_int),
                       ~ new_sf %>%
                         dplyr::filter(feature_idx_int == .x)  %>%
                         sf::st_cast("POLYGON") %>%
                         dplyr::mutate(new_area = sf::st_area(.)) %>%
                         dplyr::filter(new_area > units::set_units(0.05,km^2)) %>%
                         sf::st_cast() %>%
                         dplyr::select(-new_area,-feature_idx_int)
  )
  if(length(new_ls)>1){
    purrr::map_dfr(new_ls,~.x)
  }else{
    new_ls[[1]]
  }
}

#' @title make_each_uid_a_poly_sf
#' @description FUNCTION_DESCRIPTION
#' @param sf PARAM_DESCRIPTION
#' @param uid_chr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{nth}}
#'  \code{\link[sf]{valid}},\code{\link[sf]{sf}},\code{\link[sf]{st_geometry}},\code{\link[sf]{geos_combine}},\code{\link[sf]{sfc}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{reduce}}
#' @rdname make_each_uid_a_poly_sf
#' @export
#' @importFrom dplyr filter pull summarise_all first
#' @importFrom sf st_is_valid st_sf st_set_geometry st_union st_sfc
#' @importFrom rlang sym
#' @importFrom purrr map reduce
make_each_uid_a_poly_sf <- function(sf,
                                    uid_chr){
  sf <- sf %>% dplyr::filter(sf::st_is_valid(sf))
  duplicate_chr_vec <- sf %>% dplyr::filter(!!rlang::sym(uid_chr) %>%
                                              duplicated()) %>%
    dplyr::pull(!!rlang::sym(uid_chr)) %>%
    unique()
  sf_1 <- sf %>% dplyr::filter(!(!!rlang::sym(uid_chr) %in%
                                   duplicate_chr_vec))
  sf_2 <- sf %>% dplyr::filter(!!rlang::sym(uid_chr) %in%
                                 duplicate_chr_vec)
  purrr::map(duplicate_chr_vec,
             ~ sf::st_sf(sf_2 %>%
                           dplyr::filter(!!rlang::sym(uid_chr) == .x) %>%
                           sf::st_set_geometry(NULL) %>%
                           dplyr::summarise_all(.funs = dplyr::first),
                         geometry = sf_2 %>%
                           dplyr::filter(!!rlang::sym(uid_chr) == .x) %>%
                           sf::st_union() %>%
                           sf::st_sfc())) %>%
    append(list(sf_1))  %>%
    purrr::reduce(~rbind(.x,.y))
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
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{distinct}}
#'  \code{\link[sf]{valid}},\code{\link[sf]{st_geometry_type}},\code{\link[sf]{st_collection_extract}},\code{\link[sf]{st_cast}}
#' @rdname make_valid_new_sf
#' @export
#' @importFrom dplyr filter distinct
#' @importFrom sf st_is_valid st_make_valid st_geometry_type st_collection_extract st_cast
make_valid_new_sf <- function(sf){
  valid_sf <- sf %>%
    dplyr::filter(sf::st_is_valid(.))
  if(nrow(valid_sf)!=nrow(sf)){
    fixed_sf <- sf %>%
      dplyr::filter(!sf::st_is_valid(.)) %>%
      sf::st_make_valid()# Was lwgeom::st_make_valid()
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
    sf::st_cast("MULTIPOLYGON") %>%
    dplyr::distinct(.keep_all = T)
}
#' @title add_kmsq_area_all_features
#' @description FUNCTION_DESCRIPTION
#' @param sf PARAM_DESCRIPTION
#' @param feature_nm PARAM_DESCRIPTION
#' @param prefix PARAM_DESCRIPTION, Default: 'whl_'
#' @param suffix PARAM_DESCRIPTION, Default: '_area'
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
                                       feature_nm,
                                       prefix = "whl_",
                                       suffix = "_area"){
  sf %>%
    dplyr::mutate(!!rlang::sym(paste0(prefix,
                                      feature_nm,
                                      suffix)) := sf::st_area(.) %>%
                    units::set_units(km^2))
}
#' @title add_kmsq_area_by_group
#' @description FUNCTION_DESCRIPTION
#' @param sf PARAM_DESCRIPTION
#' @param group_by_var PARAM_DESCRIPTION
#' @param feature_nm PARAM_DESCRIPTION
#' @param prefix PARAM_DESCRIPTION, Default: 'whl_'
#' @param suffix PARAM_DESCRIPTION, Default: '_area'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[sf]{geos_combine}},\code{\link[sf]{st_geometry}}
#' @rdname add_kmsq_area_by_group
#' @export
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom rlang sym
#' @importFrom sf st_combine st_set_geometry
add_kmsq_area_by_group <- function(sf,
                                   group_by_var,
                                   feature_nm,
                                   prefix = "whl_",
                                   suffix = "_area"){
  merge(sf,
        sf %>%
          dplyr::group_by(!!rlang::sym(group_by_var)) %>%
          dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
          add_kmsq_area_all_features(feature_nm = feature_nm,
                                     prefix = prefix,
                                     suffix = suffix) %>%
          # dplyr::mutate(!!rlang::sym(paste0(prefix,
          #                                   feature_nm,
          #                                   suffix)) := sf::st_area(.) %>%
          #                 units::set_units(km^2)) %>%
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
get_res_specific_vars <- function(var_names, # THIS NEEDS TO BE MADE A CONTEXT SPECIFIC METHOD WITH THIS FUNCTION MOVED TO AusSPR4c
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
