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

#' @importMethodsFrom ready4use save_raw
#' @export
methods::setMethod("save_raw","ready4_sp_import_lup",save_raw.ready4_sp_import_lup) # NOTE, BOTH EXTENDS GENERIC FROM OTHER PACKAGE AND DEFAULTS TO S3 METHOD

#' @importMethodsFrom ready4use save_raw
#' @export
methods::setMethod("save_raw",
                   c("ready4_sp_local"),
                   function(x,
                            return_r4_lgl = T) {
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
                   }) # NOTE, EXTENDS GENERIC FROM OTHER PACKAGE


#' @importMethodsFrom ready4use make_local_proc_r4 import_chr_vec<-
methods::setMethod("make_local_proc_r4",
                   c("ready4_sp_local"), function(x,
                                               raw_data_dir_chr,
                                               save_lgl){
                     ready4_local_proc(lup_tbs_r4 = x@lup_tbs_r4,
                                                 merge_with_chr_vec = x@merge_with_chr_vec, ##
                                                 raw_data_dir_chr = raw_data_dir_chr,
                                                 overwrite_lgl = x@overwrite_lgl,
                                                 save_lgl = save_lgl) %>%
                       ready4use::`import_chr_vec<-`(import_chr_vec)
                   })

#' @importMethodsFrom ready4use make_local_proc_r4 import_chr_vec<- save_lgl<- raw_data_dir_chr<-
#' @import ready4use
methods::setMethod("make_local_proc_r4",
                   c("ready4_script_data"), function(x,
                                                     raw_data_dir_chr,
                                                     save_lgl){
                     x %>%
                       ready4use::`import_chr_vec<-`(import_chr_vec) %>%
                       ready4use::`save_lgl<-`(save_lgl) %>%
                       ready4use::`raw_data_dir_chr<-`(raw_data_dir_chr)
                   })


#' @title download_data.ready4_sp_import_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param destination_directory PARAM_DESCRIPTION
#' @param data_lookup_ref PARAM_DESCRIPTION
#' @param lookup_variable PARAM_DESCRIPTION, Default: 'name'
#' @param directory_sub_divs PARAM_DESCRIPTION, Default: NULL
#' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname download_data.ready4_sp_import_lup
#' @export
#' @importMethodsFrom ready4use download_data
download_data.ready4_sp_import_lup <- function(x, # NOTE, WHEN DOCUMENTING: IMPORTS GENERIC
                                               destination_directory,
                                               data_lookup_ref,
                                               lookup_variable = "name",
                                               directory_sub_divs = NULL,
                                               overwrite_lgl = F){

  if(is.null(directory_sub_divs))
    directory_sub_divs <- names(ready4_sp_import_lup())[names(ready4_sp_import_lup()) %in% c("country","area_type","region","main_feature","year")] ## Could add boundary year as extra directory
  directory_paths <- data_import_get_dir_paths(x = x,
                                               destination_directory = destination_directory,
                                               data_lookup_ref = data_lookup_ref,
                                               lookup_variable = lookup_variable,
                                               directory_sub_divs = directory_sub_divs)
  data_import_make_directories(directory_paths = directory_paths)
  data_import_save_files(x = x,
                         data_lookup_ref = data_lookup_ref,
                         lookup_variable = lookup_variable,
                         directory_path = directory_paths[length(directory_paths)],
                         overwrite_lgl = overwrite_lgl)
}
