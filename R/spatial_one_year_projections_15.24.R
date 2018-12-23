#' @title
#' Summarise 15-24 female and male population by region for one year.
#'
#' @description
#' This function creates a list comprised of female population projections
#' and male population projections for specified regions in a specified
#' year.
#'
#' @family spatial functions.
#'
#' @details
#'
#' @param population_forecast_range_one_year A COMPLETETEXT
#'
#' @param namestub A String.
#'
#' @return
#' A list.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[magrittr]{extract}}
#'  \code{\link[stringr]{str_subset}},\code{\link[stringr]{str_replace}}
#'  \code{\link[purrr]{reduce}}
#' @rdname spatial_one_year_projections_15_24
#' @export
#' @importFrom magrittr extract
#' @importFrom stringr str_subset str_replace
#' @importFrom purrr reduce

spatial_one_year_projections_15_24<-function(population_forecast_range_one_year,
                                              namestub){
  female_15_24_pop_forecasts <- population_forecast_range_one_year %>%
    magrittr::extract(names(.) %>%
                        stringr::str_subset("f.(?=15.19|20.24)")) %>%
    purrr::reduce(.,
                 .f = `+`
    )
  names(female_15_24_pop_forecasts) <- female_15_24_pop_forecasts %>%
    names(.) %>%
    stringr::str_replace(.,"f.15.19","f.15.24") %>%
    stringr::str_replace(.,"5.yr.pop.pred",
                         namestub)
  male_15_24_pop_forecasts <- population_forecast_range_one_year %>%
    magrittr::extract(names(.) %>%
                        stringr::str_subset("m.(?=15.19|20.24)")) %>%
    purrr::reduce(.,
                 .f = `+`
    )
  names(male_15_24_pop_forecasts) <- male_15_24_pop_forecasts %>%
    names(.) %>%
    stringr::str_replace(.,"m.15.19","m.15.24") %>%
    stringr::str_replace(.,"5.yr.pop.pred",
                         namestub)
  return(list(female_15_24_pop_forecasts = female_15_24_pop_forecasts,
                    male_15_24_pop_forecasts = male_15_24_pop_forecasts)
  )
}
