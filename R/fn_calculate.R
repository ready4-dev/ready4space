#' Calculate end date
#' @description calculate_end_date() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate end date. The function returns End date (a date vector).
#' @param model_start_ymdhms_dtm Model start years months days hours minutes seconds (a date vector)
#' @param nbr_steps_start_to_end_1L_int Number steps start to end (an integer vector of length one)
#' @param simulation_steps_ymwd_dtm Simulation steps years month weeks days (a date vector)
#' @return End date (a date vector)
#' @rdname calculate_end_date
#' @export 
#' @importFrom lubridate years weeks days hours minutes seconds
#' @keywords internal
calculate_end_date <- function (model_start_ymdhms_dtm, nbr_steps_start_to_end_1L_int, 
    simulation_steps_ymwd_dtm) 
{
    end_date_dtm <- model_start_ymdhms + lubridate::years(simulation_steps_ymwd[1]) * 
        nbr_steps_start_to_end + months(simulation_steps_ymwd[2]) * 
        nbr_steps_start_to_end + lubridate::weeks(simulation_steps_ymwd[3]) * 
        nbr_steps_start_to_end + lubridate::days(simulation_steps_ymwd[4]) * 
        nbr_steps_start_to_end + lubridate::hours(simulation_steps_ymwd[5]) * 
        nbr_steps_start_to_end + lubridate::minutes(simulation_steps_ymwd[6]) * 
        nbr_steps_start_to_end + lubridate::seconds(simulation_steps_ymwd[7]) * 
        nbr_steps_start_to_end
    return(end_date_dtm)
}
