#' Sample agent coords
#' @description randomise_locations() is a Sample function that samples values from specified distributions. Specifically, this function implements an algorithm to sample agent coords. The function is called for its side effects and does not return a value.
#' @param profiled_sf Profiled (a simple features object)
#' @param cases_int PARAM_DESCRIPTION
#' @return NA ()
#' @rdname randomise_locations
#' @export 
#' @importFrom sf st_sample st_transform st_geometry
#' @importFrom tibble as.tibble
#' @importFrom stats setNames
randomise_locations <- function (profiled_sf, cases_int) 
{
    print(cases_int)
    sampled_cases <- 0
    while (cases_int != sampled_cases) {
        sampled_cases <- 0
        sample_patient_locations <- sf::st_sample(profiled_sf, 
            cases_int)
        if (length(sample_patient_locations) > 0) {
            sample_patient_locations <- sf::st_transform(sample_patient_locations, 
                crs = 4326)
            sample_patient_locations <- do.call(rbind, sf::st_geometry(sample_patient_locations)) %>% 
                tibble::as.tibble() %>% stats::setNames(c("lon", 
                "lat"))
            sampled_cases <- nrow(sample_patient_locations)
        }
    }
    if (cases_int > 0) 
        return(sample_patient_locations)
}
