
setOldClass(c("ready4space_parameters","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @description Create a new valid instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param x A prototype for the ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment., Default: make_pt_ready4space_parameters()
#' @return A validated instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @details ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @rdname ready4space_parameters
#' @export 
ready4space_parameters <- function(x = make_pt_ready4space_parameters()){ 
validate_ready4space_parameters(make_new_ready4space_parameters(x))
}
#' make new ready4space parameters ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param x A prototype for the ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @return An unvalidated instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @details ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @rdname make_new_ready4space_parameters
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_ready4space_parameters <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4space_parameters",setdiff(make_pt_ready4space_parameters() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4space parameters ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param param_name_chr Parameter name (a character vector), Default: character(0)
#' @param deterministic_val_dbl Deterministic value (a double vector), Default: numeric(0)
#' @param distribution_chr Distribution (a character vector), Default: character(0)
#' @param dstr_param_1_dbl Distribution parameter 1 (a double vector), Default: numeric(0)
#' @param dstr_param_2_dbl Distribution parameter 2 (a double vector), Default: numeric(0)
#' @param dstr_param_3_dbl Distribution parameter 3 (a double vector), Default: numeric(0)
#' @param dstr_param_4_dbl Distribution parameter 4 (a double vector), Default: numeric(0)
#' @param transformation_chr Transformation (a character vector), Default: character(0)
#' @param use_in_chr Use in (a character vector), Default: character(0)
#' @param source_chr Source (a character vector), Default: character(0)
#' @return A prototype for ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' 
#' @rdname ready4space_parameters
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4space_parameters <- function(param_name_chr = character(0),
deterministic_val_dbl = numeric(0),
distribution_chr = character(0),
dstr_param_1_dbl = numeric(0),
dstr_param_2_dbl = numeric(0),
dstr_param_3_dbl = numeric(0),
dstr_param_4_dbl = numeric(0),
transformation_chr = character(0),
use_in_chr = character(0),
source_chr = character(0)){ 
args_ls <- list(param_name_chr = param_name_chr,
deterministic_val_dbl = deterministic_val_dbl,
distribution_chr = distribution_chr,
dstr_param_1_dbl = dstr_param_1_dbl,
dstr_param_2_dbl = dstr_param_2_dbl,
dstr_param_3_dbl = dstr_param_3_dbl,
dstr_param_4_dbl = dstr_param_4_dbl,
transformation_chr = transformation_chr,
use_in_chr = use_in_chr,
source_chr = source_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate ready4space parameters ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @description Validate an instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @return A prototpe for ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @details ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @rdname validate_ready4space_parameters
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4space_parameters <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4space_parameters())],
names(make_pt_ready4space_parameters())))!=length(names(make_pt_ready4space_parameters()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4space_parameters()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4space_parameters() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4space_parameters())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4space_parameters() %>% 
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
#' is ready4space parameters ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' 
#' @rdname ready4space_parameters
#' @export 
is_ready4space_parameters <- function(x) inherits(validate_ready4space_parameters(x), "ready4space_parameters")
