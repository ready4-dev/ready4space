
setOldClass(c("vicinity_resolutions","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @description Create a new valid instance of the ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects., Default: make_pt_vicinity_resolutions()
#' @return A validated instance of the ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @details ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @rdname vicinity_resolutions
#' @export 
vicinity_resolutions <- function(x = make_pt_vicinity_resolutions()){ 
validate_vicinity_resolutions(make_new_vicinity_resolutions(x))
}
#' make new vicinity resolutions ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @return An unvalidated instance of the ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @details ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @rdname make_new_vicinity_resolutions
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_vicinity_resolutions <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("vicinity_resolutions",setdiff(make_pt_vicinity_resolutions() %>% class(),class(x))),
class(x))
x
}
#' make prototype vicinity resolutions ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param parent_area_chr Parent area (a character vector), Default: character(0)
#' @param boundary_year_dbl Boundary year (a double vector), Default: numeric(0)
#' @param area_type_chr Area type (a character vector), Default: character(0)
#' @param area_count_chr_dbl Area count character vector (a double vector), Default: numeric(0)
#' @param complete_lgl Complete (a logical vector), Default: logical(0)
#' @param summed_area_dbl Summed area (a double vector), Default: numeric(0)
#' @param mean_size_dbl Mean size (a double vector), Default: numeric(0)
#' @return A prototype for ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' 
#' @rdname vicinity_resolutions
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_vicinity_resolutions <- function(parent_area_chr = character(0),
boundary_year_dbl = numeric(0),
area_type_chr = character(0),
area_count_chr_dbl = numeric(0),
complete_lgl = logical(0),
summed_area_dbl = numeric(0),
mean_size_dbl = numeric(0)){ 
args_ls <- list(parent_area_chr = parent_area_chr,
boundary_year_dbl = boundary_year_dbl,
area_type_chr = area_type_chr,
area_count_chr_dbl = area_count_chr_dbl,
complete_lgl = complete_lgl,
summed_area_dbl = summed_area_dbl,
mean_size_dbl = mean_size_dbl) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate vicinity resolutions ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @description Validate an instance of the ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @return A prototpe for ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @details ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @rdname validate_vicinity_resolutions
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_vicinity_resolutions <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_vicinity_resolutions())],
names(make_pt_vicinity_resolutions())))!=length(names(make_pt_vicinity_resolutions()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_vicinity_resolutions()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_vicinity_resolutions() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_vicinity_resolutions())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_vicinity_resolutions() %>% 
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
#' is vicinity resolutions ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' 
#' @rdname vicinity_resolutions
#' @export 
is_vicinity_resolutions <- function(x) inherits(validate_vicinity_resolutions(x), "vicinity_resolutions")
