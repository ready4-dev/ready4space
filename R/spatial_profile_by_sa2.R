#' @title
#' Create a simple features object with population counts based on proportion of SA2
#' included in the profiled area.
#'
#' @description
#' This function:
#'  -
#'  -
#'
#' @family spatial functions.
#'
#' @details
#'
#' @param profiled_sf A SF object corresponding to the geographic unit that is to be profiled.
#'
#' @param col_ref A variable name from the profiled_sf object from which a vector of values will
#' be used to create merge objects.
#'
#' @param res_sa1s_sf A simple features object comprised of SA1s
#'
#' @param res_sa2s_sf A simple features object comprised of SA2s.
#'
#' @return
#' A simple features object.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[seplyr]{filter_se}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{reduce}}
#'  \code{\link[dplyr]{pull}}
#' @rdname spatial_profile_by_sa2
#' @export
#' @importFrom seplyr filter_se
#' @importFrom purrr map reduce
#' @importFrom dplyr pull

spatial_profile_by_sa2 <- function(profiled_sf,
                                   col_ref,
                                   res_sa1s_sf,
                                   res_sa2s_sf
){
  filter_by_col_ref <-function(prof_sf = profiled_sf,
                           column = col_ref,
                           col_lookup){
    first_bit <- paste0(column,
                        "==")
    col_lookup_2 <- col_lookup
    filtered_profiled_sf <- seplyr::filter_se(prof_sf,
                                              paste0(first_bit,
                                                     col_lookup_2))
    return(filtered_profiled_sf)
  }
  profiled_object <- purrr::map(profiled_sf %>%
                                         dplyr::pull(!!col_ref) ,
                                       ~ spatial_profile_by_resolution_and_update_counts(profiled_sf = filter_by_col_ref(col_lookup = .),
                                                                                                          resolution_sf = res_sa2s_sf,
                                                                                                          resolution_sa1s_sf = res_sa1s_sf,
                                                                                                          resolution_sa2s_sf = res_sa2s_sf,
                                                                                                          return_resolution = "SA2")) %>%
    purrr::reduce(sf:::rbind.sf)
  return(profiled_object)
}
