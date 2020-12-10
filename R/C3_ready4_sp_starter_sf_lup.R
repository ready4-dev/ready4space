
setOldClass(c("ready4_sp_starter_sf_lup","tbl_df", "tbl", "data.frame"))
#' Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @description Create a new valid instance of the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects., Default: make_pt_ready4_sp_starter_sf_lup()
#' @return A validated instance of the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @details Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @rdname ready4_sp_starter_sf_lup
#' @export 

ready4_sp_starter_sf_lup <- function(x = make_pt_ready4_sp_starter_sf_lup()){ 
validate_ready4_sp_starter_sf_lup(make_new_ready4_sp_starter_sf_lup(x))
}
#' Make new Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @description Create a new unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @return An unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @details Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @rdname make_new_ready4_sp_starter_sf_lup
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_sp_starter_sf_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_sp_starter_sf_lup",setdiff(make_pt_ready4_sp_starter_sf_lup() %>% class(),class(x))),
class(x))
x
}
#' Make prototype Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @description Create a new prototype for the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @param country PARAM_DESCRIPTION, Default: character(0)
#' @param area_type PARAM_DESCRIPTION, Default: character(0)
#' @param area_bound_yr PARAM_DESCRIPTION, Default: character(0)
#' @param starter_sf Starter (a simple features object), Default: character(0)
#' @param sf_main_sub_div PARAM_DESCRIPTION, Default: character(0)
#' @return A prototype for Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @details Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @rdname make_pt_ready4_sp_starter_sf_lup
#' @export 
#' @importFrom ready4class update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4_sp_starter_sf_lup <- function(country = character(0),
area_type = character(0),
area_bound_yr = character(0),
starter_sf = character(0),
sf_main_sub_div = character(0)){ 
args_ls <- list(country = country,
area_type = area_type,
area_bound_yr = area_bound_yr,
starter_sf = starter_sf,
sf_main_sub_div = sf_main_sub_div) %>% ready4class::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @description Validate an instance of the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @param x An unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @return A prototpe for Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @details Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @rdname validate_ready4_sp_starter_sf_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_sp_starter_sf_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4_sp_starter_sf_lup())],
names(make_pt_ready4_sp_starter_sf_lup())))!=length(names(make_pt_ready4_sp_starter_sf_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4_sp_starter_sf_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_pt_ready4_sp_starter_sf_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4_sp_starter_sf_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4_sp_starter_sf_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4_sp_starter_sf_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @description Check whether an object is a valid instance of the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @details Readyforwhatsnext S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @rdname is_ready4_sp_starter_sf_lup
#' @export 

is_ready4_sp_starter_sf_lup <- function(x) inherits(validate_ready4_sp_starter_sf_lup(x), "ready4_sp_starter_sf_lup")
