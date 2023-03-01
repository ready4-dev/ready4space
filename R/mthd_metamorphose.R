#' Metamorphose data from one model module (or sub-module) instance to an instance of a different model module or sub-module
#' @description metamorphose.vicinity_raw() is a metamorphose method that metamorphoses an instance of a class into an instance of a different (non-child) class. This method is implemented for the ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param y_VicinityLookup PARAM_DESCRIPTION, Default: NULL
#' @param package_1L_chr Package (a character vector of length one)
#' @param raw_fls_dir_1L_chr Raw files directory (a character vector of length one)
#' @param processed_fls_dir_1L_chr Processed files directory (a character vector of length one), Default: 'data'
#' @param lup_r4_name PARAM_DESCRIPTION
#' @param crs_nbr_dbl Coordinates reference system number (a double vector), Default: NA
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: F
#' @return Z (Look up tables for spatiotemporal data)
#' @rdname metamorphose-methods
#' @export 
#' @importFrom purrr reduce
#' @importFrom ready4 rowbind_all_tbs_in_r4_obj metamorphose
#' @importFrom dplyr slice
metamorphose.vicinity_raw <- function (x, y_VicinityLookup = NULL, package_1L_chr, raw_fls_dir_1L_chr, 
    processed_fls_dir_1L_chr = "data", lup_r4_name, crs_nbr_dbl = NA_real_, 
    overwrite_1L_lgl = F) 
{
    if (is.null(y_VicinityLookup)) 
        y_VicinityLookup <- VicinityLookup()
    x <- renew(x, what_1L_chr = "names") %>% renew(x, what_1L_chr = "order")
    z_VicinityLookup <- purrr::reduce(1:nrow(x), .init = y_VicinityLookup, 
        ~ready4::rowbind_all_tbs_in_r4_obj(tbs_r4 = .x, second_tbs_r4 = x %>% 
            dplyr::slice(.y) %>% manufacture(merge_itms_chr = manufacture(.x, 
            y_vicinity_raw = x %>% dplyr::slice(.y), path_1L_chr = processed_fls_dir_1L_chr, 
            what_1L_chr = "imports_script"), package_1L_chr = package_1L_chr, 
            raw_fls_dir_1L_chr = raw_fls_dir_1L_chr, processed_fls_dir_1L_chr = processed_fls_dir_1L_chr, 
            crs_nbr_dbl = crs_nbr_dbl, overwrite_1L_lgl = overwrite_1L_lgl), 
            r4_name_1L_chr = "VicinityLookup"))
    return(z_VicinityLookup)
}
#' @rdname metamorphose-methods
#' @aliases metamorphose,vicinity_raw-method
#' @importFrom ready4 metamorphose
methods::setMethod("metamorphose", methods::className("vicinity_raw", package = "vicinity"), metamorphose.vicinity_raw)
#' 
#' Metamorphose data from one model module (or sub-module) instance to an instance of a different model module or sub-module
#' @name metamorphose-VicinityLocalProcessed
#' @description metamorphose method applied to VicinityLocalProcessed
#' @param x An object of class VicinityLocalProcessed
#' @return Y (Look up tables for spatiotemporal data)
#' @rdname metamorphose-methods
#' @aliases metamorphose,VicinityLocalProcessed-method
#' @export 
#' @importFrom ready4use assert_single_row_tb
#' @importFrom ready4 metamorphose
methods::setMethod("metamorphose", "VicinityLocalProcessed", function (x) 
{
    y_VicinityLookup <- x@a_VicinityLookup
    vicinity_raw_r3 <- y_VicinityLookup@vicinity_raw_r3
    ready4use::assert_single_row_tb(vicinity_raw_r3)
    if (vicinity_raw_r3$data_type_chr == "Geometry") {
        y_VicinityLookup <- renew(y_VicinityLookup, path_1L_chr = x@path_to_seed_sf_1L_chr, 
            what_1L_chr = "templates") %>% renew(what_1L_chr = "identifiers")
    }
    y_VicinityLookup <- y_VicinityLookup %>% renew(template_ls = x@imports_ls, 
        tbl_data_type_1L_chr = vicinity_raw_r3$data_type_chr, 
        package_1L_chr = x@pkg_1L_chr, what_1L_chr = "processed")
    return(y_VicinityLookup)
})
#' 
#' Metamorphose data from one model module (or sub-module) instance to an instance of a different model module or sub-module
#' @name metamorphose-VicinityLocal
#' @description metamorphose method applied to VicinityLocal
#' @param x An object of class VicinityLocal
#' @param imports_chr Imports (a character vector)
#' @param raw_fls_dir_1L_chr Raw files directory (a character vector of length one)
#' @param write_1L_lgl Write (a logical vector of length one)
#' @return X (Object defining data to be saved in local directory in a processed (R) format.)
#' @rdname metamorphose-methods
#' @aliases metamorphose,VicinityLocal-method
#' @export 
#' @importFrom ready4 metamorphose
methods::setMethod("metamorphose", "VicinityLocal", function (x, imports_chr, raw_fls_dir_1L_chr, write_1L_lgl) 
{
    x_VicinityLocalProcessed <- VicinityLocalProcessed(a_VicinityLookup = x@a_VicinityLookup, 
        imports_chr = imports_chr, merge_itms_chr = x@merge_itms_chr, 
        overwrite_1L_lgl = x@overwrite_1L_lgl, raw_fls_dir_1L_chr = raw_fls_dir_1L_chr, 
        write_1L_lgl = write_1L_lgl)
    return(x_VicinityLocalProcessed)
})
