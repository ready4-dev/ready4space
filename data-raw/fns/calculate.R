calculate_end_date <- function(#input_ls
                               model_start_ymdhms_dtm,
                               nbr_steps_start_to_end_1L_int,
                               simulation_steps_ymwd_dtm){
  end_date_dtm <- model_start_ymdhms +
    lubridate::years(simulation_steps_ymwd[1]) * nbr_steps_start_to_end +
    months(simulation_steps_ymwd[2]) * nbr_steps_start_to_end +
    lubridate::weeks(simulation_steps_ymwd[3]) * nbr_steps_start_to_end +
    lubridate::days(simulation_steps_ymwd[4]) * nbr_steps_start_to_end +
    lubridate::hours(simulation_steps_ymwd[5]) * nbr_steps_start_to_end +
    lubridate::minutes(simulation_steps_ymwd[6]) * nbr_steps_start_to_end +
    lubridate::seconds(simulation_steps_ymwd[7]) * nbr_steps_start_to_end
  return(end_date_dtm)
}
