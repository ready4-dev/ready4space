#' @title get_data.ready4_sp_data_pack_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param col_chr PARAM_DESCRIPTION, Default: 'main_feature'
#' @param value_chr PARAM_DESCRIPTION
#' @param r_data_dir_chr PARAM_DESCRIPTION, Default: 'NA'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4utils]{data_get}}
#' @rdname get_data.ready4_sp_data_pack_lup
#' @export
#' @importFrom ready4utils data_get
get_data.ready4_sp_data_pack_lup <- function(x,
                                             col_chr = "main_feature",
                                             value_chr,
                                             r_data_dir_chr = NA_character_){
  if(!is.na(r_data_dir_chr)){
    x <- add_path_col(x,
                      r_data_dir_chr = r_data_dir_chr)
  }
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
#' @param r_data_dir_chr PARAM_DESCRIPTION, Default: 'NA'
#' @rdname get_data
methods::setMethod("get_data","ready4_sp_data_pack_lup",get_data.ready4_sp_data_pack_lup) # NOTE, DEFAULTS TO S3 METHOD
