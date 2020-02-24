#' @title get_data.ready4_sp_data_pack_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param col_chr PARAM_DESCRIPTION, Default: 'main_feature'
#' @param value_chr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_data.ready4_sp_data_pack_lup
#' @importFrom ready4utils data_get
#' @importMethodsFrom ready4use get_data
#' @export
get_data.ready4_sp_data_pack_lup <- function(x,
                                             col_chr = "main_feature",
                                             value_chr){
  readRDS(ready4utils::data_get(data_lookup_tb = x, # boundary_year
                                lookup_reference = value_chr,
                                lookup_variable = col_chr,
                                target_variable = "shiny_source",
                                evaluate = FALSE))
}

#' Get data
#' @description Get data referenced in a lookup table.
#' @name get_data
#' @param x Primary input object - see Usage section for allowable signatures
NULL

#' @importMethodsFrom ready4use get_data
#' @export
#' @param col_chr PARAM_DESCRIPTION, Default: 'main_feature'
#' @param value_chr PARAM_DESCRIPTION
#' @rdname get_data
methods::setMethod("get_data","ready4_sp_data_pack_lup",get_data.ready4_sp_data_pack_lup) # NOTE, BOTH EXTENDS GENERIC FROM OTHER PACKAGE AND DEFAULTS TO S3 METHOD
