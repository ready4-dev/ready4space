
setOldClass(c("ready4_param_str_envir","tbl_df", "tbl", "data.frame"))
#' Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @description Create a new valid instance of the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment., Default: make_pt_ready4_param_str_envir()
#' @return A validated instance of the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @rdname ready4_param_str_envir
#' @export

ready4_param_str_envir <- function(x = make_pt_ready4_param_str_envir()){
validate_ready4_param_str_envir(make_new_ready4_param_str_envir(x))
}
#' Make new Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @description Create a new unvalidated instance of the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @return An unvalidated instance of the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @rdname make_new_ready4_param_str_envir
#' @export
#' @importFrom tibble is_tibble
make_new_ready4_param_str_envir <- function(x){
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_param_str_envir",setdiff(make_pt_ready4_param_str_envir() %>% class(),class(x))),
class(x))
x
}
#' Make prototype Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @description Create a new prototype for the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param param_name_chr PARAM_DESCRIPTION, Default: character(0)
#' @param deter_val PARAM_DESCRIPTION, Default: numeric(0)
#' @param distribution PARAM_DESCRIPTION, Default: character(0)
#' @param dstr_param_1 PARAM_DESCRIPTION, Default: numeric(0)
#' @param dstr_param_2 PARAM_DESCRIPTION, Default: numeric(0)
#' @param dstr_param_3 PARAM_DESCRIPTION, Default: numeric(0)
#' @param dstr_param_4 PARAM_DESCRIPTION, Default: numeric(0)
#' @param transformation PARAM_DESCRIPTION, Default: character(0)
#' @param use_in PARAM_DESCRIPTION, Default: character(0)
#' @param source PARAM_DESCRIPTION, Default: character(0)
#' @return A prototype for Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @rdname make_pt_ready4_param_str_envir
#' @export
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4_param_str_envir <- function(param_name_chr = character(0),
deter_val = numeric(0),
distribution = character(0),
dstr_param_1 = numeric(0),
dstr_param_2 = numeric(0),
dstr_param_3 = numeric(0),
dstr_param_4 = numeric(0),
transformation = character(0),
use_in = character(0),
source = character(0)){
args_ls <- list(param_name_chr = param_name_chr,
deter_val = deter_val,
distribution = distribution,
dstr_param_1 = dstr_param_1,
dstr_param_2 = dstr_param_2,
dstr_param_3 = dstr_param_3,
dstr_param_4 = dstr_param_4,
transformation = transformation,
use_in = use_in,
source = source) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @description Validate an instance of the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param x An unvalidated instance of the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @return A prototpe for Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @rdname validate_ready4_param_str_envir
#' @export
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_param_str_envir <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4_param_str_envir())],
names(make_pt_ready4_param_str_envir())))!=length(names(make_pt_ready4_param_str_envir()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4_param_str_envir()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_pt_ready4_param_str_envir() %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::arrange(variable),
x %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::filter(variable %in% names(make_pt_ready4_param_str_envir())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4_param_str_envir() %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::pull(1),
 make_pt_ready4_param_str_envir() %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>%
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @description Check whether an object is a valid instance of the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @rdname is_ready4_param_str_envir
#' @export

is_ready4_param_str_envir <- function(x) inherits(validate_ready4_param_str_envir(x), "ready4_param_str_envir")
