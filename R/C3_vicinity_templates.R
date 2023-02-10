
setOldClass(c("vicinity_templates","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @description Create a new valid instance of the ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects., Default: make_pt_vicinity_templates()
#' @return A validated instance of the ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @details ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @rdname vicinity_templates
#' @export 
vicinity_templates <- function(x = make_pt_vicinity_templates()){ 
validate_vicinity_templates(make_new_vicinity_templates(x))
}
#' make new vicinity templates ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @return An unvalidated instance of the ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @details ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @rdname make_new_vicinity_templates
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_vicinity_templates <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("vicinity_templates",setdiff(make_pt_vicinity_templates() %>% class(),class(x))),
class(x))
x
}
#' make prototype vicinity templates ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @param country_chr Country (a character vector), Default: character(0)
#' @param area_type_chr Area type (a character vector), Default: character(0)
#' @param area_bndy_yr_chr Area boundary year (a character vector), Default: character(0)
#' @param starter_sf Starter (a simple features object), Default: character(0)
#' @param subdivision_chr Subdivision (a character vector), Default: character(0)
#' @return A prototype for ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' 
#' @rdname vicinity_templates
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_vicinity_templates <- function(country_chr = character(0),
area_type_chr = character(0),
area_bndy_yr_chr = character(0),
starter_sf = character(0),
subdivision_chr = character(0)){ 
args_ls <- list(country_chr = country_chr,
area_type_chr = area_type_chr,
area_bndy_yr_chr = area_bndy_yr_chr,
starter_sf = starter_sf,
subdivision_chr = subdivision_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate vicinity templates ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @description Validate an instance of the ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @return A prototpe for ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @details ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @rdname validate_vicinity_templates
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_vicinity_templates <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_vicinity_templates())],
names(make_pt_vicinity_templates())))!=length(names(make_pt_vicinity_templates()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_vicinity_templates()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_vicinity_templates() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_vicinity_templates())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_vicinity_templates() %>% 
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
#' is vicinity templates ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object lookup table for base file used in creation of certain spatial objects.
#' 
#' @rdname vicinity_templates
#' @export 
is_vicinity_templates <- function(x) inherits(validate_vicinity_templates(x), "vicinity_templates")
