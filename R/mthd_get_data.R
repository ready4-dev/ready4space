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
#' @seealso
#'  \code{\link[ready4utils]{data_get}}
#' @rdname get_data.ready4_sp_data_pack_lup
#' @export
#' @importFrom ready4utils data_get
#' @import ready4use
get_data.ready4_sp_data_pack_lup <- function(x,
                                             col_chr = "main_feature",
                                             value_chr){
  readRDS(ready4utils::data_get(data_lookup_tb = x, # boundary_year
                                lookup_reference = value_chr,
                                lookup_variable = col_chr,
                                target_variable = "shiny_source",
                                evaluate = FALSE))
}
