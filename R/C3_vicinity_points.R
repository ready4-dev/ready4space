
setOldClass(c("vicinity_points","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @description Create a new valid instance of the ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes., Default: make_pt_vicinity_points()
#' @return A validated instance of the ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @details ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @rdname vicinity_points
#' @export 
vicinity_points <- function(x = make_pt_vicinity_points()){ 
validate_vicinity_points(make_new_vicinity_points(x))
}
#' make new vicinity points ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @return An unvalidated instance of the ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @details ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @rdname make_new_vicinity_points
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_vicinity_points <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("vicinity_points",setdiff(make_pt_vicinity_points() %>% class(),class(x))),
class(x))
x
}
#' make prototype vicinity points ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param service_type_chr Service type (a character vector), Default: character(0)
#' @param cluster_name_chr Cluster name (a character vector), Default: character(0)
#' @param service_name_chr Service name (a character vector), Default: character(0)
#' @param lat_dbl Lat (a double vector), Default: numeric(0)
#' @param lng_dbl Longitude (a double vector), Default: numeric(0)
#' @return A prototype for ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' 
#' @rdname vicinity_points
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_vicinity_points <- function(service_type_chr = character(0),
cluster_name_chr = character(0),
service_name_chr = character(0),
lat_dbl = numeric(0),
lng_dbl = numeric(0)){ 
args_ls <- list(service_type_chr = service_type_chr,
cluster_name_chr = cluster_name_chr,
service_name_chr = service_name_chr,
lat_dbl = lat_dbl,
lng_dbl = lng_dbl) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate vicinity points ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @description Validate an instance of the ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @return A prototpe for ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @details ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @rdname validate_vicinity_points
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_vicinity_points <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_vicinity_points())],
names(make_pt_vicinity_points())))!=length(names(make_pt_vicinity_points()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_vicinity_points()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_vicinity_points() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_vicinity_points())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_vicinity_points() %>% 
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
#' is vicinity points ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' 
#' @rdname vicinity_points
#' @export 
is_vicinity_points <- function(x) inherits(validate_vicinity_points(x), "vicinity_points")
