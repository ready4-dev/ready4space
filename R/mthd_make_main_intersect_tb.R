#' @title make_main_intersect_tb
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_main_intersect_tb
#' @export

make_main_intersect_tb <- function(x,
                         ...){
  UseMethod("make_main_intersect_tb",x)
}
#' @title make_main_intersect_tb.ready4_lookup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param r_data_dir_chr PARAM_DESCRIPTION
#' @param template_tb PARAM_DESCRIPTION
#' @param tb_ref_var_chr PARAM_DESCRIPTION
#' @param ref_bound_yr PARAM_DESCRIPTION
#' @param ref_unit_chr PARAM_DESCRIPTION
#' @param tgt_bound_yr PARAM_DESCRIPTION
#' @param tgt_unit_chr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[sf]{geos_binary_ops}},\code{\link[sf]{geos_measures}},\code{\link[sf]{st_geometry}}
#'  \code{\link[units]{set_units}}
#'  \code{\link[purrr]{pluck}},\code{\link[purrr]{map}}
#'  \code{\link[ready4utils]{data_get}}
#' @rdname make_main_intersect_tb.ready4_lookup
#' @export
#' @importFrom dplyr filter pull mutate arrange group_by summarise
#' @importFrom rlang sym
#' @importFrom sf st_intersection st_area st_set_geometry
#' @importFrom units set_units
#' @importFrom purrr pluck map_chr
#' @importFrom ready4utils data_get
make_main_intersect_tb.ready4_lookup <- function(x,
                                                 r_data_dir_chr,
                                                 template_tb,
                                                 tb_ref_var_chr,
                                                 ref_bound_yr,
                                                 ref_unit_chr,
                                                 tgt_bound_yr,
                                                 tgt_unit_chr){
  ref_sf <-  get_data(x@sp_data_pack_lup %>%
                        dplyr::filter(area_bound_yr == ref_bound_yr),
                      "area_type",
                      ref_unit_chr,
                      r_data_dir_chr = r_data_dir_chr)
  tgt_sf <- get_data(x@sp_data_pack_lup %>%
                       dplyr::filter(area_bound_yr == tgt_bound_yr),
                     "area_type",
                     tgt_unit_chr,
                     r_data_dir_chr = r_data_dir_chr)
  ref_var_chr <- x@sp_uid_lup %>% get_data(value_chr = ref_unit_chr,
                                           area_bound_yr = ref_bound_yr)
  tgt_var_chr <- x@sp_uid_lup %>% get_data(value_chr = tgt_unit_chr,
                                           area_bound_yr = tgt_bound_yr)
  ref_sf <- ref_sf %>%
    dplyr::filter(!!rlang::sym(ref_var_chr) %in% (template_tb %>% dplyr::pull(tb_ref_var_chr)))
  ref_sf <- sf::st_intersection(ref_sf %>%
                                  dplyr::mutate(whl_area_km2 =  sf::st_area(.) %>%  units::set_units(km^2)),
                                tgt_sf)
  ref_sf <- ref_sf %>%
    dplyr::arrange(!!rlang::sym(ref_var_chr)) %>%
    dplyr::mutate(inc_area_km2 =  sf::st_area(.) %>%  units::set_units(km^2)) %>%
    dplyr::mutate(inc_fraction_dbl = inc_area_km2 /whl_area_km2)
  ref_sf <- ref_sf %>%
    dplyr::group_by(!!rlang::sym(ref_var_chr)) %>%
    dplyr::summarise(!!rlang::sym(tgt_var_chr) := !!rlang::sym(tgt_var_chr) %>% purrr::pluck(which(inc_fraction_dbl==max(inc_fraction_dbl))))
  template_tb <- template_tb %>%
    dplyr::mutate(!!rlang::sym(tgt_var_chr) := !!rlang::sym(tb_ref_var_chr) %>% as.character() %>% purrr::map_chr(~ ifelse(is.na(.x),
                                                                                                                           NA_character_,
                                                                                                                           ready4utils::data_get(ref_sf  %>% sf::st_set_geometry(NULL),
                                                                                                                                                 lookup_reference = .x,
                                                                                                                                                 lookup_variable = ref_var_chr,
                                                                                                                                                 target_variable = tgt_var_chr,
                                                                                                                                                 evaluate = F))
    ))
}
#' Make main intersection table
#' @description Add variables to a template table based on the intersections of the largest component of a feature from object A with a target feature in object B' @name make_main_intersect_tb
#' @param x Primary input object - see Usage section for allowable signatures
NULL

#' @export
#' @param r_data_dir_chr PARAM_DESCRIPTION
#' @param template_tb PARAM_DESCRIPTION
#' @param tb_ref_var_chr PARAM_DESCRIPTION
#' @param ref_bound_yr PARAM_DESCRIPTION
#' @param ref_unit_chr PARAM_DESCRIPTION
#' @param tgt_bound_yr PARAM_DESCRIPTION
#' @param tgt_unit_chr PARAM_DESCRIPTION
#' @rdname make_main_intersect_tb
methods::setMethod("make_main_intersect_tb","ready4_lookup",make_main_intersect_tb.ready4_lookup)
