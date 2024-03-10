
setOldClass(c("vicinity_abbreviations","tbl_df", "tbl", "data.frame"))
#' ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @description Create a new valid instance of the ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @param x A prototype for the ready4 submodule class for tibble object lookup table for spatial data abbreviations., Default: make_pt_vicinity_abbreviations()
#' @return A validated instance of the ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @details ready4 submodule for tibble object lookup table for spatial data abbreviations.
#' @rdname vicinity_abbreviations
#' @export 
vicinity_abbreviations <- function(x = make_pt_vicinity_abbreviations()){ 
validate_vicinity_abbreviations(make_new_vicinity_abbreviations(x))
}
#' make new vicinity abbreviations ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @description Create a new unvalidated instance of the ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @param x A prototype for the ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @return An unvalidated instance of the ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @details ready4 submodule for tibble object lookup table for spatial data abbreviations.
#' @rdname make_new_vicinity_abbreviations
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_vicinity_abbreviations <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("vicinity_abbreviations",setdiff(make_pt_vicinity_abbreviations() %>% class(),class(x))),
class(x))
x
}
#' make prototype vicinity abbreviations ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @param long_name_chr Long name (a character vector), Default: character(0)
#' @param short_name_chr Short name (a character vector), Default: character(0)
#' @param type_chr Type (a character vector), Default: character(0)
#' @return A prototype for ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' 
#' @rdname vicinity_abbreviations
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_vicinity_abbreviations <- function(long_name_chr = character(0),
short_name_chr = character(0),
type_chr = character(0)){ 
args_ls <- list(long_name_chr = long_name_chr,
short_name_chr = short_name_chr,
type_chr = type_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate vicinity abbreviations ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @description Validate an instance of the ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @param x An unvalidated instance of the ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @return A prototpe for ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @details ready4 submodule for tibble object lookup table for spatial data abbreviations.
#' @rdname validate_vicinity_abbreviations
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_vicinity_abbreviations <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_vicinity_abbreviations())],
names(make_pt_vicinity_abbreviations())))!=length(names(make_pt_vicinity_abbreviations()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_vicinity_abbreviations()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_vicinity_abbreviations() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_vicinity_abbreviations())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_vicinity_abbreviations() %>% 
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
#' is vicinity abbreviations ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' 
#' @rdname vicinity_abbreviations
#' @export 
is_vicinity_abbreviations <- function(x) inherits(validate_vicinity_abbreviations(x), "vicinity_abbreviations")
