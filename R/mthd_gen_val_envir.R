#' @title gen_val_envir
#' @description Generate a set of parameter values by iteration for the mean absolute prediction error of demographic projections.
#' @param params_struc_tb PARAM_DESCRIPTION
#' @param it_nbr PARAM_DESCRIPTION
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
#' @rdname gen_val_envir
#' @export
#' @importFrom dplyr mutate
gen_val_envir <- function(params_struc_tb,
                           it_nbr){
params_struc_tb %>%
  dplyr::mutate(!!paste0("v_it_",it_nbr) := gen_val_envir_vect(distribution,
                                                                 dist_param_1,
                                                                 dist_param_2,
                                                                 dist_param_3,
                                                                 transformation))
}

#' @title gen_val_envir_sgl
#' @description FUNCTION_DESCRIPTION
#' @param distribution PARAM_DESCRIPTION
#' @param dist_param_1 PARAM_DESCRIPTION
#' @param dist_param_2 PARAM_DESCRIPTION
#' @param dist_param_3 PARAM_DESCRIPTION
#' @param transformation PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname gen_val_envir_sgl
#' @export

gen_val_envir_sgl <- function(distribution,
                                dist_param_1,
                                dist_param_2,
                                dist_param_3,
                                transformation){
if(distribution == "none")
  x <- dist_param_1
if(!is.na(transformation))
  x <- eval(parse(text=transformation))
return(x)
}

#' @title gen_val_envir_vect
#' @description FUNCTION_DESCRIPTION
#' @param distribution PARAM_DESCRIPTION
#' @param dist_param_1 PARAM_DESCRIPTION
#' @param dist_param_2 PARAM_DESCRIPTION
#' @param dist_param_3 PARAM_DESCRIPTION
#' @param transformation PARAM_DESCRIPTION
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
#' @rdname gen_val_envir_vect
#' @export
#' @importFrom purrr map_dbl
gen_val_envir_vect <- function(distribution,
                                dist_param_1,
                                dist_param_2,
                                dist_param_3,
                                transformation){
purrr::map_dbl(1:length(distribution),
                ~ gen_val_envir_sgl(distribution[.x],
                                      dist_param_1[.x],
                                      dist_param_2[.x],
                                      dist_param_3[.x],
                                      transformation[.x]))
}
