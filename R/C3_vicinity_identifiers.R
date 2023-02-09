
setOldClass(c("vicinity_identifiers","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @description Create a new valid instance of the ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects., Default: make_pt_vicinity_identifiers()
#' @return A validated instance of the ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @details ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @rdname vicinity_identifiers
#' @export 
vicinity_identifiers <- function(x = make_pt_vicinity_identifiers()){ 
validate_vicinity_identifiers(make_new_vicinity_identifiers(x))
}
#' make new vicinity identifiers ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @return An unvalidated instance of the ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @details ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @rdname make_new_vicinity_identifiers
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_vicinity_identifiers <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("vicinity_identifiers",setdiff(make_pt_vicinity_identifiers() %>% class(),class(x))),
class(x))
x
}
#' make prototype vicinity identifiers ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @param spatial_unit_chr Spatial unit (a character vector), Default: character(0)
#' @param year_chr Year (a character vector), Default: character(0)
#' @param var_name_chr Variable name (a character vector), Default: character(0)
#' @return A prototype for ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' 
#' @rdname vicinity_identifiers
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_vicinity_identifiers <- function(spatial_unit_chr = character(0),
year_chr = character(0),
var_name_chr = character(0)){ 
args_ls <- list(spatial_unit_chr = spatial_unit_chr,
year_chr = year_chr,
var_name_chr = var_name_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate vicinity identifiers ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @description Validate an instance of the ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @return A prototpe for ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @details ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @rdname validate_vicinity_identifiers
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_vicinity_identifiers <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_vicinity_identifiers())],
names(make_pt_vicinity_identifiers())))!=length(names(make_pt_vicinity_identifiers()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_vicinity_identifiers()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_vicinity_identifiers() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_vicinity_identifiers())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_vicinity_identifiers() %>% 
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
#' is vicinity identifiers ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' 
#' @rdname vicinity_identifiers
#' @export 
is_vicinity_identifiers <- function(x) inherits(validate_vicinity_identifiers(x), "vicinity_identifiers")
