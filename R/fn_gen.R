#' Gen value envir
#' @description gen_val_envir() is a Gen function that generates values for a specified varaible or variables. Specifically, this function implements an algorithm to gen value envir. The function is called for its side effects and does not return a value.
#' @param params_struc_tb Params struc (a tibble)
#' @param it_nbr PARAM_DESCRIPTION
#' @return NULL
#' @rdname gen_val_envir
#' @export 
#' @importFrom dplyr mutate
gen_val_envir <- function (params_struc_tb, it_nbr) 
{
    params_struc_tb %>% dplyr::mutate(`:=`(!!paste0("v_it_", 
        it_nbr), gen_val_envir_vect(distribution, dist_param_1, 
        dist_param_2, dist_param_3, transformation)))
}
#' Gen value envir sgl
#' @description gen_val_envir_sgl() is a Gen function that generates values for a specified varaible or variables. Specifically, this function implements an algorithm to gen value envir sgl. The function is called for its side effects and does not return a value.
#' @param distribution PARAM_DESCRIPTION
#' @param dist_param_1 PARAM_DESCRIPTION
#' @param dist_param_2 PARAM_DESCRIPTION
#' @param dist_param_3 PARAM_DESCRIPTION
#' @param transformation PARAM_DESCRIPTION
#' @return NA ()
#' @rdname gen_val_envir_sgl
#' @export 

gen_val_envir_sgl <- function (distribution, dist_param_1, dist_param_2, dist_param_3, 
    transformation) 
{
    if (distribution == "none") 
        x <- dist_param_1
    if (!is.na(transformation)) 
        x <- eval(parse(text = transformation))
    return(x)
}
#' Gen value envir vect
#' @description gen_val_envir_vect() is a Gen function that generates values for a specified varaible or variables. Specifically, this function implements an algorithm to gen value envir vect. The function is called for its side effects and does not return a value.
#' @param distribution PARAM_DESCRIPTION
#' @param dist_param_1 PARAM_DESCRIPTION
#' @param dist_param_2 PARAM_DESCRIPTION
#' @param dist_param_3 PARAM_DESCRIPTION
#' @param transformation PARAM_DESCRIPTION
#' @return NULL
#' @rdname gen_val_envir_vect
#' @export 
#' @importFrom purrr map_dbl
gen_val_envir_vect <- function (distribution, dist_param_1, dist_param_2, dist_param_3, 
    transformation) 
{
    purrr::map_dbl(1:length(distribution), ~gen_val_envir_sgl(distribution[.x], 
        dist_param_1[.x], dist_param_2[.x], dist_param_3[.x], 
        transformation[.x]))
}
#' Gen value mape
#' @description gen_val_mape() is a Gen function that generates values for a specified varaible or variables. Specifically, this function implements an algorithm to gen value mape. The function is called for its side effects and does not return a value.
#' @param params_struc_tb Params struc (a tibble)
#' @param it_nbr PARAM_DESCRIPTION
#' @param jt_dist PARAM_DESCRIPTION
#' @return NULL
#' @rdname gen_val_mape
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom mc2d qpert rpert
#' @importFrom tidyr gather unite
gen_val_mape <- function (params_struc_tb, it_nbr, jt_dist) 
{
    if (jt_dist) {
        params_vals_tb <- params_struc_tb %>% dplyr::mutate(jt_dist_loc_2 = runif(1)) %>% 
            dplyr::mutate(mape_05_yr = mc2d::qpert(p = jt_dist_loc_2, 
                mode = mape_05_yr_mde, min = mape_05_yr_min, 
                max = mape_05_yr_max, shape = mape_05_yr_shp), 
                mape_10_yr = mc2d::qpert(p = jt_dist_loc_2, mode = mape_10_yr_mde, 
                  min = mape_10_yr_min, max = mape_10_yr_max, 
                  shape = mape_10_yr_shp), mape_15_yr = mc2d::qpert(p = jt_dist_loc_2, 
                  mode = mape_15_yr_mde, min = mape_15_yr_min, 
                  max = mape_15_yr_max, shape = mape_15_yr_shp))
    }
    else {
        params_vals_tb <- params_struc_tb %>% dplyr::mutate(nbr_draws = 1) %>% 
            dplyr::mutate(mape_05_yr = mc2d::rpert(n = nbr_draws, 
                mode = mape_05_yr_mde, min = mape_05_yr_min, 
                max = mape_05_yr_max, shape = mape_05_yr_shp), 
                mape_10_yr = mc2d::rpert(n = nbr_draws, mode = mape_10_yr_mde, 
                  min = mape_10_yr_min, max = mape_10_yr_max, 
                  shape = mape_10_yr_shp), mape_15_yr = mc2d::rpert(n = nbr_draws, 
                  mode = mape_15_yr_mde, min = mape_15_yr_min, 
                  max = mape_15_yr_max, shape = mape_15_yr_shp))
    }
    params_vals_tb %>% dplyr::select(sex_age_band, mape_05_yr, 
        mape_10_yr, mape_15_yr) %>% tidyr::gather(key = "param_name", 
        value = !!paste0("v_it_", it_nbr), mape_05_yr, mape_10_yr, 
        mape_15_yr) %>% tidyr::unite(param_name, sex_age_band, 
        sep = "_", col = "param_name")
}
