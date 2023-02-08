## Implements: https://rpubs.com/maiae/drivetime
sample_agent_coords <- function(profiled_sf,
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
