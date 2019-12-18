#' ready4_par_struc_mape
#' @name ready4_par_struc_mape
#' @description Create a new valid instance of the S3 class: ready4_par_struc_mape
#' @param x PARAM_DESCRIPTION, Default: make_prototype_ready4_par_struc_mape()
#' @return A validated instance of the ready4_par_struc_mape class
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ready4_par_struc_mape
#' @export 

ready4_par_struc_mape <- function(x = make_prototype_ready4_par_struc_mape()){ 
validate_ready4_par_struc_mape(new_ready4_par_struc_mape(x))
}
#' ready4_par_struc_mape
#' @name new_ready4_par_struc_mape
#' @description Create a new unvalidated instance of the S3 class: new_ready4_par_struc_mape
#' @param x PARAM_DESCRIPTION
#' @return An unvalidated instance of the ready4_par_struc_mape class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tibble]{is_tibble}}
#' @rdname ready4_par_struc_mape
#' @export 
#' @importFrom tibble is_tibble
new_ready4_par_struc_mape <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append("ready4_par_struc_mape",
class(x))
x
}
#' ready4_par_struc_mape
#' @name make_prototype_ready4_par_struc_mape
#' @description Create a new prototype for S3 class: make_prototype_ready4_par_struc_mape

#' @return A prototpe for ready4_par_struc_mape class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tibble]{tibble}}
#' @rdname ready4_par_struc_mape
#' @export 
#' @importFrom tibble tibble
make_prototype_ready4_par_struc_mape <- function(){ 
tibble::tibble(param_name = character(0),
sex_age_band = character(0),
mape_05_yr_mde = numeric(0),
mape_10_yr_mde = numeric(0),
mape_15_yr_mde = numeric(0),
mape_05_yr_min = numeric(0),
mape_10_yr_min = numeric(0),
mape_15_yr_min = numeric(0),
mape_05_yr_max = numeric(0),
mape_10_yr_max = numeric(0),
mape_15_yr_max = numeric(0),
mape_05_yr_shp = numeric(0),
mape_10_yr_shp = numeric(0),
mape_15_yr_shp = numeric(0))
}
#' ready4_par_struc_mape
#' @name validate_ready4_par_struc_mape
#' @description Validate an instance of the S3 class: validate_ready4_par_struc_mape
#' @param x PARAM_DESCRIPTION
#' @return A prototpe for ready4_par_struc_mape class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_detect}},\code{\link[stringr]{str_c}}
#'  \code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[tidyr]{gather}}
#'  \code{\link[purrr]{map2}}
#' @rdname ready4_par_struc_mape
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_par_struc_mape <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_prototype_ready4_par_struc_mape())],
names(make_prototype_ready4_par_struc_mape())))!=length(names(make_prototype_ready4_par_struc_mape()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_prototype_ready4_par_struc_mape()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_prototype_ready4_par_struc_mape() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_prototype_ready4_par_struc_mape())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_prototype_ready4_par_struc_mape() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_prototype_ready4_par_struc_mape() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' ready4_par_struc_mape
#' @name is_ready4_par_struc_mape
#' @description Check whether an object is a valid instance of the S3 class: is_ready4_par_struc_mape
#' @param x PARAM_DESCRIPTION
#' @return A logical value, TRUE if a valid instance of the ready4_par_struc_mape class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ready4_par_struc_mape
#' @export 

is_ready4_par_struc_mape <- function(x) inherits(validate_ready4_par_struc_mape(x), "ready4_par_struc_mape")
