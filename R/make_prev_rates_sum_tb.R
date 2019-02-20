#' make_prev_rates_sum_tb
#' Create a summary table of prevalence rate data inputs to simulation model.
#' @param prev_rates_vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_detect}},\code{\link[stringr]{str_sub}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{join}}
#' @rdname make_prev_rates_sum_tb
#' @export 
#' @importFrom stringr str_detect str_sub
#' @importFrom tibble tibble
#' @importFrom dplyr inner_join

make_prev_rates_sum_tb <- function(prev_rates_vec){
  female_vec <- prev_rates_vec[names(prev_rates_vec) %>% 
                                 stringr::str_detect("f_")]
  male_vec <- prev_rates_vec[names(prev_rates_vec) %>%
                               stringr::str_detect("m_")]
  female_tb <- tibble::tibble(age = names(female_vec) %>%
                                stringr::str_sub(start=-2),
                              Rate_Females = female_vec)
  male_tb <- tibble::tibble(age = names(male_vec) %>%
                              stringr::str_sub(start=-2),
                            Rate_Males = male_vec)
  dplyr::inner_join(female_tb,
                    male_tb)
}