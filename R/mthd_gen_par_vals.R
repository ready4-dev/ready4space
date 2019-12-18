#' @title gen_par_vals
#' @description A generic function for generating new parameter value sets for each simulation iteration.
#' @param x PARAM_DESCRIPTION
#' @param n_its_int PARAM_DESCRIPTION
#' @param jt_dist PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname gen_par_vals
#' @export

gen_par_vals <- function(x,
                         n_its_int,
                         jt_dist = FALSE,
                         ...){
  if(n_its_int < 1 | n_its_int %% 1 != 0)
    stop("n_its_int must be a positive integer")
  UseMethod("gen_par_vals",x)
}

#' @describeIn gen_par_vals Method for ready4_params_struc_mape class.
#' @param x PARAM_DESCRIPTION
#' @param n_its_int PARAM_DESCRIPTION
#' @param jt_dist PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{prepend}},\code{\link[purrr]{reduce}}
#'  \code{\link[dplyr]{join}}
#' @export
#' @importFrom purrr prepend reduce
#' @importFrom dplyr inner_join
gen_par_vals.ready4_params_struc_mape <- function(x,
                                                  n_its_int,
                                                  jt_dist){
  if(n_its_int==1){
    gen_val_mape(params_struc_tb = x,
                 it_nbr = 1,
                 jt_dist = jt_dist)
  }else{
    reduce_list <- purrr::prepend(2:n_its_int,
                                  list(gen_val_mape(params_struc_tb = x,
                                                    it_nbr = 1,
                                                    jt_dist = jt_dist)))
    purrr::reduce(reduce_list,
                  ~ dplyr::inner_join(.x,
                                      gen_val_mape(params_struc_tb = x,
                                                   it_nbr = .y,
                                                   jt_dist = jt_dist))) %>%
      rfwn_par_val_mape()
  }
}

#' @describeIn gen_par_vals Method for ready4_par_str_envir class.
#' @param x PARAM_DESCRIPTION
#' @param n_its_int PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{prepend}},\code{\link[purrr]{reduce}}
#'  \code{\link[dplyr]{join}},\code{\link[dplyr]{select}}
#' @export
#' @importFrom purrr prepend reduce
#' @importFrom dplyr inner_join select
gen_par_vals.ready4_par_str_envir <- function(x,
                                              n_its_int){
  if(n_its_int==1){
    gen_val_envir(params_struc_tb = x,
                  it_nbr = 1)
  }else{
    reduce_list <- purrr::prepend(2:n_its_int,
                                  list(gen_val_envir(params_struc_tb = x,
                                                     it_nbr = 1)))
    purrr::reduce(reduce_list,
                  ~ dplyr::inner_join(.x,
                                      gen_val_envir(params_struc_tb = x,
                                                    it_nbr = .y))) %>%
      dplyr::select(-c(distribution,
                       dist_param_1,
                       dist_param_2,
                       dist_param_3,
                       transformation)) %>%
      rfwn_par_val_envir()
  }
}
