#' Remove grouped population variables
#' @description remove_grouped_popl_vars() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove grouped population variables. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param featured_var_pfx_1L_chr Featured variable prefix (a character vector of length one)
#' @return Profiled (a simple features object)
#' @rdname remove_grouped_popl_vars
#' @export 
#' @importFrom dplyr select
#' @keywords internal
remove_grouped_popl_vars <- function (profiled_sf, featured_var_pfx_1L_chr) 
{
    var_names_chr <- profiled_sf %>% names()
    keep_vars_chr <- var_names_chr[!var_names_chr %>% startsWith("whl_") & 
        !var_names_chr %>% startsWith("grp_by_") & !var_names_chr %>% 
        startsWith("dupl_")]
    keep_vars_chr <- keep_vars_chr[!keep_vars_chr %>% startsWith("inc_") | 
        keep_vars_chr %>% startsWith(featured_var_pfx_1L_chr)]
    profiled_sf <- dplyr::select(profiled_sf, keep_vars_chr)
    return(profiled_sf)
}
#' Remove outlier areas
#' @description remove_outlier_areas() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove outlier areas. The function returns Areas (a simple features object).
#' @param areas_sf Areas (a simple features object)
#' @param outliers_chr Outliers (a character vector)
#' @param area_var_nm_1L_chr Area variable name (a character vector of length one), Default: character(0)
#' @return Areas (a simple features object)
#' @rdname remove_outlier_areas
#' @export 
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @importFrom stringr str_detect
#' @importFrom stats setNames
#' @importFrom purrr prepend reduce
#' @importFrom sf st_as_sf
#' @keywords internal
remove_outlier_areas <- function (areas_sf, outliers_chr, area_var_nm_1L_chr = character(0)) 
{
    included_areas_with_outliers_chr <- areas_sf %>% dplyr::pull(!!rlang::sym(area_var_nm_1L_chr))
    remove_outlier <- function(x, y) x[!stringr::str_detect(x, 
        y)]
    included_areas_excl_outliers_chr <- as.list(outliers_chr) %>% 
        stats::setNames(outliers_chr) %>% purrr::prepend(list(a = included_areas_with_outliers_chr)) %>% 
        purrr::reduce(remove_outlier)
    areas_sf <- areas_sf %>% dplyr::filter(!!rlang::sym(area_var_nm_1L_chr) %in% 
        included_areas_excl_outliers_chr) %>% sf::st_as_sf()
    return(areas_sf)
}
