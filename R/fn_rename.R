#' Rename areas
#' @description rename_areas() is a Rename function that renames elements of an object based on a pre-specified schema. Specifically, this function implements an algorithm to rename areas. The function returns Area names (a character vector).
#' @param area_names_chr Area names (a character vector)
#' @param correspondences_lup Correspondences (a lookup table), Default: NULL
#' @return Area names (a character vector)
#' @rdname rename_areas
#' @export 
#' @importFrom purrr map_chr
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
rename_areas <- function (area_names_chr, correspondences_lup = NULL) 
{
    if (!is.null(correspondences_lup)) {
        area_names_chr <- area_names_chr %>% purrr::map_chr(~ifelse(.x %in% 
            correspondences_lup$old_nms_chr, ready4::get_from_lup_obj(correspondences_lup, 
            match_value_xx = .x, match_var_nm_1L_chr = "old_nms_chr", 
            target_var_nm_1L_chr = "new_nms_chr"), .x))
    }
    return(area_names_chr)
}
