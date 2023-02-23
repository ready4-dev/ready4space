#' Randomise locations
#' @description randomise_locations() is a Randomise function that randomly samples from data. Specifically, this function implements an algorithm to randomise locations. The function returns Sample patient locations (an output object of multiple potential types).
#' @param profiled_sf Profiled (a simple features object)
#' @param cases_int Cases (an integer vector)
#' @param crs_dbl Coordinates reference system (a double vector), Default: 4326
#' @return Sample patient locations (an output object of multiple potential types)
#' @rdname randomise_locations
#' @export 
#' @importFrom sf st_sample st_transform st_geometry
#' @importFrom tibble as.tibble
#' @importFrom stats setNames
#' @keywords internal
randomise_locations <- function (profiled_sf, cases_int, crs_dbl = 4326) 
{
    print(cases_int)
    sampled_cases_int <- 0
    while (cases_int != sampled_cases_int) {
        sampled_cases_int <- 0
        sample_patient_locations_xx <- sf::st_sample(profiled_sf, 
            cases_int)
        if (length(sample_patient_locations_xx) > 0) {
            sample_patient_locations_xx <- sf::st_transform(sample_patient_locations_xx, 
                crs = crs_dbl)
            sample_patient_locations_xx <- do.call(rbind, sf::st_geometry(sample_patient_locations_xx)) %>% 
                tibble::as.tibble() %>% stats::setNames(c("lng_dbl", 
                "lat_dbl"))
            sampled_cases_int <- nrow(sample_patient_locations_xx)
        }
    }
    if (cases_int > 0) 
        return(sample_patient_locations_xx)
}
