#' @title
#' Generates coordinates for synthetic incident cases.
#'
#' @description
#' This function:
#'   -
#'   -
#'
#' @family spatial functions.
#'
#' @details
#'
#' @param profiled_sf A SF object corresponding to the geographic unit that is to be profiled.
#'
#' @param incident_cases A double ...
#'
#' @return
#' A simple features object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_sample}},\code{\link[sf]{st_transform}},\code{\link[sf]{st_geometry}}
#'  \code{\link[tibble]{as_tibble}}
#'  \code{\link[stats]{setNames}}
#' @rdname spatial_cases_coordinates
#' @export
#' @importFrom sf st_sample st_transform st_geometry
#' @importFrom tibble as.tibble
#' @importFrom stats setNames

spatial_cases_coordinates <- function(profiled_sf,
                                    incident_cases){
  print(incident_cases)
  sampled_cases <- 0
  while(incident_cases != sampled_cases){
    sampled_cases <- 0
    sample_patient_locations <- sf::st_sample(profiled_sf,
                                                               incident_cases)
    if(length(sample_patient_locations) > 0){
      sample_patient_locations <- sf::st_transform(sample_patient_locations,
                                                   crs=4326)
      sample_patient_locations <- do.call(rbind, sf::st_geometry(sample_patient_locations)) %>%
        tibble::as.tibble() %>%
        stats::setNames(c("lon","lat"))
      sampled_cases <- nrow(sample_patient_locations)
    }
  }
  if(incident_cases > 0)
    return(sample_patient_locations)
}
