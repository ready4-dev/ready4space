
setOldClass(c("vicinity_mapes","tbl_df", "tbl", "data.frame"))
#' ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @description Create a new valid instance of the ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @param x A prototype for the ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors., Default: make_pt_vicinity_mapes()
#' @return A validated instance of the ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @details ready4 submodule for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @rdname vicinity_mapes
#' @export 
vicinity_mapes <- function(x = make_pt_vicinity_mapes()){ 
validate_vicinity_mapes(make_new_vicinity_mapes(x))
}
#' make new vicinity mean absolute prediction errors ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @description Create a new unvalidated instance of the ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @param x A prototype for the ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @return An unvalidated instance of the ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @details ready4 submodule for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @rdname make_new_vicinity_mapes
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_vicinity_mapes <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("vicinity_mapes",setdiff(make_pt_vicinity_mapes() %>% class(),class(x))),
class(x))
x
}
#' make prototype vicinity mean absolute prediction errors ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @param param_name_chr Parameter name (a character vector), Default: character(0)
#' @param var_nm_chr Variable name (a character vector), Default: character(0)
#' @param mape_05_yr_mde_dbl Mean absolute prediction error 05 year mode (a double vector), Default: numeric(0)
#' @param mape_10_yr_mde_dbl Mean absolute prediction error 10 year mode (a double vector), Default: numeric(0)
#' @param mape_15_yr_mde_dbl Mean absolute prediction error 15 year mode (a double vector), Default: numeric(0)
#' @param mape_05_yr_min_dbl Mean absolute prediction error 05 year minimum (a double vector), Default: numeric(0)
#' @param mape_10_yr_min_dbl Mean absolute prediction error 10 year minimum (a double vector), Default: numeric(0)
#' @param mape_15_yr_min_dbl Mean absolute prediction error 15 year minimum (a double vector), Default: numeric(0)
#' @param mape_05_yr_max_dbl Mean absolute prediction error 05 year maximum (a double vector), Default: numeric(0)
#' @param mape_10_yr_max_dbl Mean absolute prediction error 10 year maximum (a double vector), Default: numeric(0)
#' @param mape_15_yr_max_dbl Mean absolute prediction error 15 year maximum (a double vector), Default: numeric(0)
#' @param mape_05_yr_shp_dbl Mean absolute prediction error 05 year shape (a double vector), Default: numeric(0)
#' @param mape_10_yr_shp_dbl Mean absolute prediction error 10 year shape (a double vector), Default: numeric(0)
#' @param mape_15_yr_shp_dbl Mean absolute prediction error 15 year shape (a double vector), Default: numeric(0)
#' @return A prototype for ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' 
#' @rdname vicinity_mapes
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_vicinity_mapes <- function(param_name_chr = character(0),
var_nm_chr = character(0),
mape_05_yr_mde_dbl = numeric(0),
mape_10_yr_mde_dbl = numeric(0),
mape_15_yr_mde_dbl = numeric(0),
mape_05_yr_min_dbl = numeric(0),
mape_10_yr_min_dbl = numeric(0),
mape_15_yr_min_dbl = numeric(0),
mape_05_yr_max_dbl = numeric(0),
mape_10_yr_max_dbl = numeric(0),
mape_15_yr_max_dbl = numeric(0),
mape_05_yr_shp_dbl = numeric(0),
mape_10_yr_shp_dbl = numeric(0),
mape_15_yr_shp_dbl = numeric(0)){ 
args_ls <- list(param_name_chr = param_name_chr,
var_nm_chr = var_nm_chr,
mape_05_yr_mde_dbl = mape_05_yr_mde_dbl,
mape_10_yr_mde_dbl = mape_10_yr_mde_dbl,
mape_15_yr_mde_dbl = mape_15_yr_mde_dbl,
mape_05_yr_min_dbl = mape_05_yr_min_dbl,
mape_10_yr_min_dbl = mape_10_yr_min_dbl,
mape_15_yr_min_dbl = mape_15_yr_min_dbl,
mape_05_yr_max_dbl = mape_05_yr_max_dbl,
mape_10_yr_max_dbl = mape_10_yr_max_dbl,
mape_15_yr_max_dbl = mape_15_yr_max_dbl,
mape_05_yr_shp_dbl = mape_05_yr_shp_dbl,
mape_10_yr_shp_dbl = mape_10_yr_shp_dbl,
mape_15_yr_shp_dbl = mape_15_yr_shp_dbl) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate vicinity mean absolute prediction errors ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @description Validate an instance of the ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @param x An unvalidated instance of the ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @return A prototpe for ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @details ready4 submodule for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @rdname validate_vicinity_mapes
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_vicinity_mapes <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_vicinity_mapes())],
names(make_pt_vicinity_mapes())))!=length(names(make_pt_vicinity_mapes()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_vicinity_mapes()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_vicinity_mapes() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_vicinity_mapes())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_vicinity_mapes() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class))
  vars_chr <- class_lup %>% dplyr::pull(1) %>% unique()
  classes_chr <- vars_chr %>%  purrr::map_chr(~dplyr::filter(class_lup, variable == .x) %>%  dplyr::pull(2) %>% paste0(collapse = ", "))
purrr::map2_chr(vars_chr,
classes_chr,
~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", 
")
}),
call. = FALSE)
}

x}
#' is vicinity mean absolute prediction errors ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 submodule class for tibble object that stores spatial simulation parameters relating to Mean Absolute Prediction Errors.
#' 
#' @rdname vicinity_mapes
#' @export 
is_vicinity_mapes <- function(x) inherits(validate_vicinity_mapes(x), "vicinity_mapes")
