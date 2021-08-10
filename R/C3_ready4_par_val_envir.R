
setOldClass(c("ready4_par_val_envir","tbl_df", "tbl", "data.frame"))
#' Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @description Create a new valid instance of the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration., Default: make_pt_ready4_par_val_envir()
#' @return A validated instance of the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @rdname ready4_par_val_envir
#' @export 

ready4_par_val_envir <- function(x = make_pt_ready4_par_val_envir()){ 
validate_ready4_par_val_envir(make_new_ready4_par_val_envir(x))
}
#' Make new Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @description Create a new unvalidated instance of the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @return An unvalidated instance of the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @rdname make_new_ready4_par_val_envir
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_par_val_envir <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_par_val_envir",setdiff(make_pt_ready4_par_val_envir() %>% class(),class(x))),
class(x))
x
}
#' Make prototype Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @description Create a new prototype for the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @param param_name_chr PARAM_DESCRIPTION, Default: character(0)
#' @param v_it_1 PARAM_DESCRIPTION, Default: numeric(0)
#' @return A prototype for Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @rdname make_pt_ready4_par_val_envir
#' @export 
#' @importFrom ready4class update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4_par_val_envir <- function(param_name_chr = character(0),
v_it_1 = numeric(0)){ 
args_ls <- list(param_name_chr = param_name_chr,
v_it_1 = v_it_1) %>% ready4class::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @description Validate an instance of the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @param x An unvalidated instance of the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @return A prototpe for Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @rdname validate_ready4_par_val_envir
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_par_val_envir <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4_par_val_envir())],
names(make_pt_ready4_par_val_envir())))!=length(names(make_pt_ready4_par_val_envir()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4_par_val_envir()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_pt_ready4_par_val_envir() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4_par_val_envir())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4_par_val_envir() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4_par_val_envir() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @description Check whether an object is a valid instance of the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation parameter values for each iteration.
#' @rdname is_ready4_par_val_envir
#' @export 

is_ready4_par_val_envir <- function(x) inherits(validate_ready4_par_val_envir(x), "ready4_par_val_envir")
