
setOldClass(c("ready4space_values","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @description Create a new valid instance of the ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @param x A prototype for the ready4 S3 class for tibble object that stores simulation parameter values for each iteration., Default: make_pt_ready4space_values()
#' @return A validated instance of the ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @details ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @rdname ready4space_values
#' @export 
ready4space_values <- function(x = make_pt_ready4space_values()){ 
validate_ready4space_values(make_new_ready4space_values(x))
}
#' make new ready4space values ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @param x A prototype for the ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @return An unvalidated instance of the ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @details ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @rdname make_new_ready4space_values
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_ready4space_values <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4space_values",setdiff(make_pt_ready4space_values() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4space values ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @param param_name_chr Parameter name (a character vector), Default: character(0)
#' @param iteration_1_dbl Iteration 1 (a double vector), Default: numeric(0)
#' @return A prototype for ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' 
#' @rdname ready4space_values
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4space_values <- function(param_name_chr = character(0),
iteration_1_dbl = numeric(0)){ 
args_ls <- list(param_name_chr = param_name_chr,
iteration_1_dbl = iteration_1_dbl) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate ready4space values ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @description Validate an instance of the ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @return A prototpe for ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @details ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @rdname validate_ready4space_values
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4space_values <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4space_values())],
names(make_pt_ready4space_values())))!=length(names(make_pt_ready4space_values()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4space_values()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4space_values() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4space_values())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4space_values() %>% 
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
#' is ready4space values ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object that stores simulation parameter values for each iteration.
#' 
#' @rdname ready4space_values
#' @export 
is_ready4space_values <- function(x) inherits(validate_ready4space_values(x), "ready4space_values")
