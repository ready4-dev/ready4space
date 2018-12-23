#' @title
#' Select and rename variables for 0-29 year old males and females in a population tibble.
#'
#' @description
#' This function updates an inputted tibble of population data for a profiled region by subsetting
#' the data and renaming the variables.
#'
#' @family spatial functions.
#'
#' @details
#'
#' @param population_tib A tibble of population projection data for a profiled region
#' broken down by unit of resolution.
#'
#' @param year A String specifying the year to which data relates.
#'
#' @param also_include A String vector specifying the year to which data relates.
#'
#' @return
#' A tibble.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' @rdname spatial_select_rename_age_sex
#' @export
#' @importFrom dplyr select one_of

spatial_select_rename_age_sex <- function (population_tib,
                                           year,
                                           also_include){
  population_tib <- population_tib %>%
  dplyr::select(dplyr::one_of(also_include),
                which(
                  grepl('Female|Male',
                              names(population_tib)) &
                    grepl('0-4|5-9|10-14|15-19|20-24|25-29',
                                names(population_tib))))
  population_tib <- population_tib %>%
  dplyr::select(-which(grepl('Female|Male',
                                         names(population_tib)) &
                               grepl('40-44',
                                           names(population_tib))))
names(population_tib) <- names(population_tib) %>%
  gsub("\r\n",
             "",
             .) %>%
  gsub("Females",
             paste0("y",
                    year,
                    ".Females"),#"y2016.Females",
             .) %>%
  gsub("Males",
             paste0("y",
                    year,
                    ".Males"),#"y2016.Males",
             .)  %>%
  gsub(" ",
             ".",
             .) %>%
  gsub("-",
             ".",
             .)
return(population_tib)
}
