#' Add unique identifier lookup table
#' @description add_uid_lup() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add unique identifier lookup table. Function argument lookup_tbs_r4 specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param lookup_tbs_r4 Lookup tibbles (a ready4 S4)
#' @return NULL
#' @rdname add_uid_lup
#' @export 
#' @importFrom tibble add_row
#' @importFrom dplyr pull
#' @keywords internal
add_uid_lup <- function (lookup_tbs_r4) 
{
    uid_lup_r3 <- tibble::add_row(ready4_sp_uid_lup(), spatial_unit = sp_import_lup(lookup_tbs_r4) %>% 
        dplyr::pull(area_type), year = sp_import_lup(lookup_tbs_r4) %>% 
        dplyr::pull(area_bound_yr), var_name = sp_import_lup(lookup_tbs_r4) %>% 
        dplyr::pull(uid))
    `sp_uid_lup<-`(lookup_tbs_r4, uid_lup_r3)
}
