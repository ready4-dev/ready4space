#' @title make_import_object
#' @description Generic function to make import object
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
#' @rdname make_import_object
#' @export
make_data_packs <- function(x,
                            ...){
  UseMethod("make_import_object",x)
}

#' @title make_import_object.ready4_sp_import_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param var_val_vec PARAM_DESCRIPTION
#' @param path_str PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_import_object.ready4_sp_import_lup
#' @export

make_import_object.ready4_sp_import_lup <- function(x,
                                                    var_val_vec,
                                                    path_str){
  stop("A Make Import Object Method needs to be defined for the child class of ready4_sp_import_lup.")
}
