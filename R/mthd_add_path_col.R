#' @title add_path_col
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
#' @export
add_path_col <- function(x,
                     ...){
  UseMethod("add_path_col",x)
}

#' add_path_col
#' @name add_path_col
#' @description Generic method for getting data referenced in a lookup table
#' @rdname add_path_col
#' @export
methods::setGeneric("add_path_col")


#' @title add_path_col.ready4_sp_data_pack_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param r_data_dir_chr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{pluck}}
#'  \code{\link[stringr]{str_locate}},\code{\link[stringr]{str_sub}}
#' @rdname add_path_col.ready4_sp_data_pack_lup
#' @export
#' @importFrom dplyr mutate
#' @importFrom purrr map_dbl pluck
#' @importFrom stringr str_locate str_sub
add_path_col.ready4_sp_data_pack_lup <- function(x,
                                                 r_data_dir_chr){
  x %>%
    dplyr::mutate(start_from = purrr::map_dbl(source_reference, ~ 2 + stringr::str_locate(.x,":") %>% purrr::pluck(1))) %>%
    dplyr::mutate(start_from = purrr::map_dbl(start_from, ~ ifelse(is.na(.x),1,.x))) %>%
    dplyr::mutate(shiny_source = paste0(r_data_dir_chr,"/",stringr::str_sub(source_reference,start=start_from),".rds"))

}

#' @export
#' @param r_data_dir_chr PARAM_DESCRIPTION
#' @rdname add_path_col
methods::setMethod("add_path_col","ready4_sp_data_pack_lup",add_path_col.ready4_sp_data_pack_lup) # NOTE, BOTH EXTENDS GENERIC FROM OTHER PACKAGE AND DEFAULTS TO S3 METHOD
