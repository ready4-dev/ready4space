#' Remove grouped population variables
#' @description remove_grouped_popl_vars() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove grouped population variables. Function argument profiled_sf specifies the object to be updated. Argument featured_var_pfx_1L_chr provides the object to be updated. The function returns Profiled (a simple features object).
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
