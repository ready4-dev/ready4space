#' @title save_raw.ready4_sp_import_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param required_data PARAM_DESCRIPTION
#' @param destination_directory PARAM_DESCRIPTION
#' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#' @rdname save_raw.ready4_sp_import_lup
#' @export
#' @importFrom purrr map_lgl
#' @importFrom ready4use save_raw
save_raw.ready4_sp_import_lup <- function(x, # NOTE, WHEN DOCUMENTING: IMPORTS GENERIC
                                          required_data,
                                          destination_directory,
                                          overwrite_lgl = F){
  purrr::map_lgl(required_data,
                 ~ download_data(x = x,
                                 destination_directory = destination_directory,
                                 data_lookup_ref = .x,
                                 overwrite_lgl = overwrite_lgl))
}

#' Save raw data
#' @description Save a local copy of the raw (unprocessed) input data.
#' @name save_raw
#' @param x Primary input object - see Usage section for allowable signatures
NULL

#' @importMethodsFrom ready4use save_raw
#' @export
#' @param required_data PARAM_DESCRIPTION
#' @param destination_directory PARAM_DESCRIPTION
#' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' @rdname save_raw
methods::setMethod("save_raw","ready4_sp_import_lup",save_raw.ready4_sp_import_lup) # NOTE, BOTH EXTENDS GENERIC FROM OTHER PACKAGE AND DEFAULTS TO S3 METHOD

#' @export
#' @param return_r4_lgl PARAM_DESCRIPTION
#' @rdname save_raw
methods::setMethod("save_raw",
                   "ready4_sp_local",
                   function(x,
                            return_r4_lgl){
                     sp_import_lup <- x@lup_tbs_r4@sp_import_lup
                     ready4use::assert_single_row_tb(sp_import_lup)
                     raw_format_sp_dir <- make_raw_format_dir(data_type_chr = sp_import_lup$data_type,
                                                              raw_data_dir = x@raw_data_dir_chr)
                     import_chr_vec <- get_import_chr_vec(x@lup_tbs_r4,
                                                          data_type_chr = sp_import_lup$data_type)
                     save_lgl <- save_raw(x = sp_import_lup,
                                          required_data = import_chr_vec,
                                          destination_directory = raw_format_sp_dir,
                                          overwrite_lgl = x@overwrite_lgl)
                     if(return_r4_lgl){
                       make_local_proc_r4(x,
                                          raw_data_dir_chr = raw_format_sp_dir,
                                          save_lgl = save_lgl)

                     }
                   })
