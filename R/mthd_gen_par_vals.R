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
#' @import ready4use
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
