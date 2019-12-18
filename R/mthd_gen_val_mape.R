#' @title gen_val_mape
#' @description Generate a set of parameter values by iteration for the mean absolute prediction error of demographic projections.
#' @param params_struc_tb PARAM_DESCRIPTION
#' @param it_nbr PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[mc2d]{pert}}
#'  \code{\link[tidyr]{gather}},\code{\link[tidyr]{unite}}
#' @rdname gen_val_mape
#' @export
#' @importFrom dplyr mutate select
#' @importFrom mc2d qpert rpert
#' @importFrom tidyr gather unite
gen_val_mape <- function(params_struc_tb,
                                 it_nbr,
                                 jt_dist){
  if(jt_dist){
    params_vals_tb <- params_struc_tb %>%
      dplyr::mutate(jt_dist_loc_2 = runif(1)) %>%
      dplyr::mutate(mape_05_yr = mc2d::qpert(p = jt_dist_loc_2,
                                             mode = mape_05_yr_mde,
                                             min = mape_05_yr_min,
                                             max = mape_05_yr_max,
                                             shape = mape_05_yr_shp),
                    mape_10_yr = mc2d::qpert(p = jt_dist_loc_2,
                                             mode = mape_10_yr_mde,
                                             min = mape_10_yr_min,
                                             max = mape_10_yr_max,
                                             shape = mape_10_yr_shp),
                    mape_15_yr = mc2d::qpert(p = jt_dist_loc_2,
                                             mode = mape_15_yr_mde,
                                             min = mape_15_yr_min,
                                             max = mape_15_yr_max,
                                             shape = mape_15_yr_shp))
  }else{
    params_vals_tb <- params_struc_tb %>%
      dplyr::mutate(nbr_draws = 1) %>%
      dplyr::mutate(mape_05_yr = mc2d::rpert(n = nbr_draws,
                                             mode = mape_05_yr_mde,
                                             min = mape_05_yr_min,
                                             max = mape_05_yr_max,
                                             shape = mape_05_yr_shp),
                    mape_10_yr = mc2d::rpert(n = nbr_draws,
                                             mode = mape_10_yr_mde,
                                             min = mape_10_yr_min,
                                             max = mape_10_yr_max,
                                             shape = mape_10_yr_shp),
                    mape_15_yr = mc2d::rpert(n = nbr_draws,
                                             mode = mape_15_yr_mde,
                                             min = mape_15_yr_min,
                                             max = mape_15_yr_max,
                                             shape = mape_15_yr_shp))
  }
  params_vals_tb %>%
    dplyr::select(sex_age_band,mape_05_yr,mape_10_yr,mape_15_yr) %>%
    tidyr::gather(key = "param_name",
                  value = !!paste0("v_it_",it_nbr),
                  mape_05_yr,
                  mape_10_yr,
                  mape_15_yr) %>%
    tidyr::unite(param_name,
                 sex_age_band,
                 sep="_",
                 col="param_name")
}

# is.rfwn_par_str_mape(ready.aus.data::params_struc_mape_tb)
# test_par_str_mape <- ready.aus.data::params_struc_mape_tb
# is.rfwn_par_str_mape(test_par_str_mape)
# test_par_val_mape <- par_vals_gen(test_par_str_mape,
#                                   nbr_its = 5,
#                                   jt_dist = FALSE)
# is.rfwn_par_val_mape(test_par_val_mape)
