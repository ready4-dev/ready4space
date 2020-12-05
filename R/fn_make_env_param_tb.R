#' @title make_env_param_tb
#' @description Make a table of the parameter sets for a ready4_sim_env object
#' @param n_its_int PARAM_DESCRIPTION
#' @param env_str_par_tb PARAM_DESCRIPTION
#' @param mape_str_par_tb PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{bind}}
#' @rdname make_env_param_tb
#' @export
#' @importFrom dplyr bind_rows
make_env_param_tb <- function(n_its_int,
                              env_str_par_tb,
                              mape_str_par_tb,
                              jt_dist){
  par_val_mape <- gen_par_vals(x = mape_str_par_tb,
                                             n_its_int = n_its_int,
                                             jt_dist = jt_dist)
  par_val_env <- gen_par_vals(x = env_str_par_tb,
                                            n_its_int = n_its_int)
  dplyr::bind_rows(par_val_env,
                   par_val_mape)
}
