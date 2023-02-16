calculate_end_date <- function(input_ls){
  end_data_dtm <- input_ls$model_start_ymdhms +
    lubridate::years(input_ls$simulation_steps_ymwd[1]) * input_ls$nbr_steps_start_to_end +
    months(input_ls$simulation_steps_ymwd[2]) * input_ls$nbr_steps_start_to_end +
    lubridate::weeks(input_ls$simulation_steps_ymwd[3]) * input_ls$nbr_steps_start_to_end +
    lubridate::days(input_ls$simulation_steps_ymwd[4]) * input_ls$nbr_steps_start_to_end +
    lubridate::hours(input_ls$simulation_steps_ymwd[5]) * input_ls$nbr_steps_start_to_end +
    lubridate::minutes(input_ls$simulation_steps_ymwd[6]) * input_ls$nbr_steps_start_to_end +
    lubridate::seconds(input_ls$simulation_steps_ymwd[7]) * input_ls$nbr_steps_start_to_end
  return(end_data_dtm)
}
