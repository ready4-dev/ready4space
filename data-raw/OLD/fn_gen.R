#' Gen value envir
#' @description calculate_val_from_dstr() is a Gen function that generates values for a specified varaible or variables. Specifically, this function implements an algorithm to gen value envir. The function is called for its side effects and does not return a value.
#' @param x_vicinity_parameters Params struc (a tibble)
#' @param iter_1L_int PARAM_DESCRIPTION
#' @return NULL
#' @rdname calculate_val_from_dstr
#' @export 
#' @importFrom dplyr mutate
calculate_val_from_dstr <- function (x_vicinity_parameters, iter_1L_int) 
{
    x_vicinity_parameters %>% dplyr::mutate(`:=`(!!paste0("v_it_", 
        iter_1L_int), transform_multiple_vals(distribution, dstr_param_1, 
        dstr_param_2, dstr_param_3, transformation)))
}
#' Gen value envir sgl
#' @description transform_value() is a Gen function that generates values for a specified varaible or variables. Specifically, this function implements an algorithm to gen value envir sgl. The function is called for its side effects and does not return a value.
#' @param distribution PARAM_DESCRIPTION
#' @param dstr_param_1 PARAM_DESCRIPTION
#' @param dstr_param_2 PARAM_DESCRIPTION
#' @param dstr_param_3 PARAM_DESCRIPTION
#' @param transformation PARAM_DESCRIPTION
#' @return NA ()
#' @rdname transform_value
#' @export 

transform_value <- function (distribution, dstr_param_1, dstr_param_2, dstr_param_3, 
    transformation) 
{
    if (distribution == "none") 
        x <- dstr_param_1
    if (!is.na(transformation)) 
        x <- eval(parse(text = transformation))
    return(x)
}
#' Gen value envir vect
#' @description transform_multiple_vals() is a Gen function that generates values for a specified varaible or variables. Specifically, this function implements an algorithm to gen value envir vect. The function is called for its side effects and does not return a value.
#' @param distribution PARAM_DESCRIPTION
#' @param dstr_param_1 PARAM_DESCRIPTION
#' @param dstr_param_2 PARAM_DESCRIPTION
#' @param dstr_param_3 PARAM_DESCRIPTION
#' @param transformation PARAM_DESCRIPTION
#' @return NULL
#' @rdname transform_multiple_vals
#' @export 
#' @importFrom purrr map_dbl
transform_multiple_vals <- function (distribution, dstr_param_1, dstr_param_2, dstr_param_3, 
    transformation) 
{
    purrr::map_dbl(1:length(distribution), ~transform_value(distribution[.x], 
        dstr_param_1[.x], dstr_param_2[.x], dstr_param_3[.x], 
        transformation[.x]))
}
#' Gen value mape
#' @description calculate_mape() is a Gen function that generates values for a specified varaible or variables. Specifically, this function implements an algorithm to gen value mape. The function is called for its side effects and does not return a value.
#' @param x_vicinity_parameters Params struc (a tibble)
#' @param iter_1L_int PARAM_DESCRIPTION
#' @param joint_dstr_1L_lgl PARAM_DESCRIPTION
#' @return NULL
#' @rdname calculate_mape
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom mc2d qpert rpert
#' @importFrom tidyr gather unite
calculate_mape <- function (x_vicinity_parameters, iter_1L_int, joint_dstr_1L_lgl) 
{
    if (joint_dstr_1L_lgl) {
        params_vals_tb <- x_vicinity_parameters %>% dplyr::mutate(jt_dstr_loc_2 = runif(1)) %>% 
            dplyr::mutate(mape_05_yr = mc2d::qpert(p = jt_dstr_loc_2, 
                mode = mape_05_yr_mde_dbl, min = mape_05_yr_min_dbl, 
                max = mape_05_yr_max_dbl, shape = mape_05_yr_shp_dbl), 
                mape_10_yr = mc2d::qpert(p = jt_dstr_loc_2, mode = mape_10_yr_mde_dbl, 
                  min = mape_10_yr_min_dbl, max = mape_10_yr_max_dbl, 
                  shape = mape_10_yr_shp_dbl), mape_15_yr = mc2d::qpert(p = jt_dstr_loc_2, 
                  mode = mape_15_yr_mde_dbl, min = mape_15_yr_min_dbl, 
                  max = mape_15_yr_max_dbl, shape = mape_15_yr_shp_dbl))
    }
    else {
        params_vals_tb <- x_vicinity_parameters %>% dplyr::mutate(nbr_draws = 1) %>% 
            dplyr::mutate(mape_05_yr = mc2d::rpert(n = nbr_draws, 
                mode = mape_05_yr_mde_dbl, min = mape_05_yr_min_dbl, 
                max = mape_05_yr_max_dbl, shape = mape_05_yr_shp_dbl), 
                mape_10_yr = mc2d::rpert(n = nbr_draws, mode = mape_10_yr_mde_dbl, 
                  min = mape_10_yr_min_dbl, max = mape_10_yr_max_dbl, 
                  shape = mape_10_yr_shp_dbl), mape_15_yr = mc2d::rpert(n = nbr_draws, 
                  mode = mape_15_yr_mde_dbl, min = mape_15_yr_min_dbl, 
                  max = mape_15_yr_max_dbl, shape = mape_15_yr_shp_dbl))
    }
    params_vals_tb %>% dplyr::select(sex_age_band_chr, mape_05_yr, 
        mape_10_yr, mape_15_yr) %>% tidyr::gather(key = "param_name_chr", 
        value = !!paste0("v_it_", iter_1L_int), mape_05_yr, mape_10_yr, 
        mape_15_yr) %>% tidyr::unite(param_name_chr, sex_age_band_chr, 
        sep = "_", col = "param_name_chr")
}
