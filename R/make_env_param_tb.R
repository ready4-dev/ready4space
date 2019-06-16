#' @title make_env_param_tb
#' @description Make a table of the parameter sets for a ready4_sim_env object
#' @param nbr_its PARAM_DESCRIPTION
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
#'  \code{\link[ready4agents]{gen_par_vals}}
#'  \code{\link[dplyr]{bind}}
#' @rdname make_env_param_tb
#' @export
#' @importFrom ready4agents gen_par_vals
#' @importFrom dplyr bind_rows
make_env_param_tb <- function(nbr_its,
                              env_str_par_tb,
                              mape_str_par_tb,
                              jt_dist){
  par_val_mape <- ready4agents::gen_par_vals(x = mape_str_par_tb,
                                             nbr_its = nbr_its,
                                             jt_dist = jt_dist)
  par_val_env <- ready4agents::gen_par_vals(x = env_str_par_tb,
                                            nbr_its = nbr_its)
  dplyr::bind_rows(par_val_env,
                   par_val_mape)
}
