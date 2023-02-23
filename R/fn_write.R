#' Write attribute tibble
#' @description write_att_tb() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write attribute tibble. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param att_tb Attribute (a tibble)
#' @param object_nm_1L_chr Object name (a character vector of length one)
#' @param processed_fls_dir_1L_chr Processed files directory (a character vector of length one)
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one)
#' @return NULL
#' @rdname write_att_tb
#' @export 
#' @keywords internal
write_att_tb <- function (att_tb, object_nm_1L_chr, processed_fls_dir_1L_chr, 
    overwrite_1L_lgl) 
{
    path_to_att_tb_chr <- make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = processed_fls_dir_1L_chr, 
        name_chr = object_nm_1L_chr, data_type_chr = "Attribute")
    if (overwrite_1L_lgl | !file.exists(path_to_att_tb_chr)) 
        saveRDS(att_tb, file = path_to_att_tb_chr)
}
#' Write directories for import
#' @description write_dirs_for_imp() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write directories for import. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param paths_chr Paths (a character vector)
#' @return NULL
#' @rdname write_dirs_for_imp
#' @export 
#' @importFrom purrr walk
#' @keywords internal
write_dirs_for_imp <- function (paths_chr) 
{
    purrr::walk(paths_chr, ~dir.create(.x))
}
#' Write raw format directory
#' @description write_raw_format_dir() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write raw format directory. The function returns Path to raw output directory (a character vector of length one).
#' @param data_type_chr Data type (a character vector)
#' @param raw_fls_dir_1L_chr Raw files directory (a character vector of length one)
#' @return Path to raw output directory (a character vector of length one)
#' @rdname write_raw_format_dir
#' @export 
#' @keywords internal
write_raw_format_dir <- function (data_type_chr, raw_fls_dir_1L_chr) 
{
    category_1L_chr <- switch(data_type_chr, Geometry = "Geometries", 
        Attribute = "Attributes")
    path_to_raw_outp_dir_1L_chr <- make_path_for_raw_outp_dir(category_1L_chr = category_1L_chr, 
        raw_fls_dir_1L_chr = raw_fls_dir_1L_chr)
    if (!dir.exists(path_to_raw_outp_dir_1L_chr)) 
        dir.create(path_to_raw_outp_dir_1L_chr)
    return(path_to_raw_outp_dir_1L_chr)
}
