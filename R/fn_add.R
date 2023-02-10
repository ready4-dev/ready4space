#' Add unique identifier lookup table
#' @description add_uid_lup() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add unique identifier lookup table. Function argument x_VicinityLookup specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param x_VicinityLookup PARAM_DESCRIPTION
#' @return X (Look up tables for spatiotemporal data)
#' @rdname add_uid_lup
#' @export 
#' @importFrom tibble add_row
#' @importFrom dplyr pull
#' @keywords internal
add_uid_lup <- function (x_VicinityLookup) 
{
    uid_lup_r3 <- tibble::add_row(vicinity_identifiers(), spatial_unit_chr = x_VicinityLookup@vicinity_raw_r3 %>% 
        dplyr::pull(area_type_chr), year_chr = x_VicinityLookup@vicinity_raw_r3 %>% 
        dplyr::pull(area_bndy_yr_chr), var_name_chr = x_VicinityLookup@vicinity_raw_r3 %>% 
        dplyr::pull(uid_chr))
    x_VicinityLookup <- renewSlot(x_VicinityLookup, "vicinity_identifiers_r3", 
        uid_lup_r3)
    return(x_VicinityLookup)
}
