#' Sample agent coords
#' @description sample_agent_coords() is a Sample function that samples values from specified distributions. Specifically, this function implements an algorithm to sample agent coords. The function is called for its side effects and does not return a value.
#' @param profiled_sf Profiled (a simple features object)
#' @param incident_cases PARAM_DESCRIPTION
#' @return NA ()
#' @rdname sample_agent_coords
#' @export 
#' @importFrom sf st_sample st_transform st_geometry
#' @importFrom tibble as.tibble
#' @importFrom stats setNames
sample_agent_coords <- function (profiled_sf, incident_cases) 
{
    print(incident_cases)
    sampled_cases <- 0
    while (incident_cases != sampled_cases) {
        sampled_cases <- 0
        sample_patient_locations <- sf::st_sample(profiled_sf, 
            incident_cases)
        if (length(sample_patient_locations) > 0) {
            sample_patient_locations <- sf::st_transform(sample_patient_locations, 
                crs = 4326)
            sample_patient_locations <- do.call(rbind, sf::st_geometry(sample_patient_locations)) %>% 
                tibble::as.tibble() %>% stats::setNames(c("lon", 
                "lat"))
            sampled_cases <- nrow(sample_patient_locations)
        }
    }
    if (incident_cases > 0) 
        return(sample_patient_locations)
}
