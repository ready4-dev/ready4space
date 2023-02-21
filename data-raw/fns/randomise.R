## Implements: https://rpubs.com/maiae/drivetime
randomise_locations <- function(profiled_sf,
                                cases_int,
                                crs_dbl = 4326){
  print(cases_int)
  sampled_cases_int <- 0
  while(cases_int != sampled_cases_int){
    sampled_cases_int <- 0
    sample_patient_locations_xx <- sf::st_sample(profiled_sf,
                                              cases_int)
    if(length(sample_patient_locations_xx) > 0){
      sample_patient_locations_xx <- sf::st_transform(sample_patient_locations_xx,
                                                   crs=crs_dbl)
      sample_patient_locations_xx <- do.call(rbind, sf::st_geometry(sample_patient_locations_xx)) %>%
        tibble::as.tibble() %>%
        stats::setNames(c("lng_dbl","lat_dbl"#"lon","lat"
                          ))
      sampled_cases_int <- nrow(sample_patient_locations_xx)
    }
  }
  if(cases_int > 0) # Ammend conditionality?
    return(sample_patient_locations_xx)
}
