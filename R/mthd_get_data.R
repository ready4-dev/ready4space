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
#'  \code{\link[ready4fun]{get_from_lup}}
#' @rdname get_data.ready4_sp_data_pack_lup
#' @export
#' @importFrom ready4fun get_from_lup
get_data.ready4_sp_data_pack_lup <- function(x,
                                             col_chr = "main_feature",
                                             value_chr,
                                             r_data_dir_chr = NA_character_){
  if(!is.na(r_data_dir_chr)){
    x <- add_path_col(x,
                      r_data_dir_chr = r_data_dir_chr)
  }
  readRDS(ready4fun::get_from_lup(data_lookup_tb = x, # boundary_year
                                lookup_reference = value_chr,
                                lookup_variable = col_chr,
                                target_variable = "shiny_source",
                                evaluate = FALSE))
}

#' @title get_data.ready4_sp_abbreviations_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param col_chr PARAM_DESCRIPTION, Default: 'short_name'
#' @param value_chr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4fun]{get_from_lup}}
#' @rdname get_data.ready4_sp_abbreviations_lup
#' @export
#' @importFrom ready4fun get_from_lup
get_data.ready4_sp_abbreviations_lup <- function(x,
                                          col_chr = "short_name",
                                          value_chr){
  ready4fun::get_from_lup(data_lookup_tb = x,
                        lookup_reference = value_chr,
                        lookup_variable = col_chr,
                        target_variable = ifelse(col_chr == "short_name","long_name","short_name"),
                        evaluate = FALSE)
}

#' @title get_data.ready4_sp_uid_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param col_chr PARAM_DESCRIPTION, Default: 'spatial_unit'
#' @param value_chr PARAM_DESCRIPTION
#' @param area_bound_yr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4fun]{get_from_lup}}
#'  \code{\link[dplyr]{filter}}
#' @rdname get_data.ready4_sp_uid_lup
#' @export
#' @importFrom ready4fun get_from_lup
#' @importFrom dplyr filter
get_data.ready4_sp_uid_lup <- function(x,
                                col_chr = "spatial_unit",
                                value_chr,
                                area_bound_yr){
  ready4fun::get_from_lup(data_lookup_tb = x %>% dplyr::filter(year == area_bound_yr),
                        lookup_reference = value_chr,
                        lookup_variable = col_chr,
                        target_variable = ifelse(col_chr == "spatial_unit","var_name","spatial_unit"),
                        evaluate = FALSE)
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
methods::setMethod("get_data","ready4_sp_data_pack_lup",get_data.ready4_sp_data_pack_lup)

#' @importMethodsFrom ready4use get_data
#' @export
#' @param col_chr PARAM_DESCRIPTION, Default: 'short_name'
#' @param value_chr PARAM_DESCRIPTION
#' @rdname get_data
methods::setMethod("get_data","ready4_sp_abbreviations_lup",get_data.ready4_sp_abbreviations_lup)

#' @importMethodsFrom ready4use get_data
#' @export
#' @param col_chr PARAM_DESCRIPTION, Default: 'spatial_unit'
#' @param value_chr PARAM_DESCRIPTION
#' @param area_bound_yr PARAM_DESCRIPTION
#' @rdname get_data
methods::setMethod("get_data","ready4_sp_uid_lup",get_data.ready4_sp_uid_lup)
