#' @title
#' Deterministic five year population growth projections
#'
#' @description
#' This function creates a list......
#'
#' @family demographic modelling functions.
#'
#' @details
#'
#' @param demographic.data A .....
#'
#' @param pop.growth.by.age.lga A .....
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
#'  \code{\link[dplyr]{join}},\code{\link[dplyr]{mutate}}
#' @rdname demographic_deterministic_2016_2021
#' @export
#' @importFrom dplyr inner_join mutate

demographic_deterministic_2016_2021<-function(demographic.data,
                                         pop.growth.by.age.lga){
  returnobject<-demographic.data %>%
    dplyr::inner_join(pop.growth.by.age.lga) %>%
    dplyr::mutate(y2018.Females.15.19 = y2016.Females.15.19*(1+(growth.pc.5y.2021.f.15.19/100)*0.4),
                  y2019.Females.15.19 = y2016.Females.15.19**(1+(growth.pc.5y.2021.f.15.19/100)*0.6),
                  y2020.Females.15.19 = y2016.Females.15.19*(1+(growth.pc.5y.2021.f.15.19/100)*0.8),
                  y2021.Females.15.19 = y2016.Females.15.19*(1+(growth.pc.5y.2021.f.15.19/100)),
                  y2018.Females.20.24 = y2016.Females.20.24*(1+(growth.pc.5y.2021.f.20.24/100)*0.4),
                  y2019.Females.20.24 = y2016.Females.20.24*(1+(growth.pc.5y.2021.f.20.24/100)*0.6),
                  y2020.Females.20.24 = y2016.Females.20.24*(1+(growth.pc.5y.2021.f.20.24/100)*0.8),
                  y2021.Females.20.24 = y2016.Females.20.24*(1+(growth.pc.5y.2021.f.20.24/100)),
                  y2018.Males.15.19 = y2016.Males.15.19*(1+(growth.pc.5y.2021.f.15.19/100)*0.4),
                  y2019.Males.15.19 = y2016.Males.15.19*(1+(growth.pc.5y.2021.f.15.19/100)*0.6),
                  y2020.Males.15.19 = y2016.Males.15.19*(1+(growth.pc.5y.2021.f.15.19/100)*0.8),
                  y2021.Males.15.19 = y2016.Males.15.19*(1+(growth.pc.5y.2021.f.15.19/100)),
                  y2018.Males.20.24 = y2016.Males.20.24*(1+(growth.pc.5y.2021.m.20.24/100)*0.4),
                  y2019.Males.20.24 = y2016.Males.20.24*(1+(growth.pc.5y.2021.m.20.24/100)*0.6),
                  y2020.Males.20.24 = y2016.Males.20.24*(1+(growth.pc.5y.2021.m.20.24/100)*0.8),
                  y2021.Males.20.24 = y2016.Males.20.24*(1+(growth.pc.5y.2021.m.20.24/100))) %>%
    dplyr::mutate(y2018.total15to24f = y2018.Females.15.19 + y2018.Females.20.24,
                  y2019.total15to24f = y2019.Females.15.19 + y2019.Females.20.24,
                  y2020.total15to24f = y2018.Females.15.19 + y2020.Females.20.24,
                  y2021.total15to24f = y2018.Females.15.19 + y2021.Females.20.24,
                  y2018.total15to24m = y2018.Males.15.19 + y2018.Males.20.24,
                  y2019.total15to24m = y2019.Males.15.19 + y2019.Males.20.24,
                  y2020.total15to24m = y2020.Males.15.19 + y2020.Males.20.24,
                  y2021.total15to24m = y2021.Males.15.19 + y2021.Males.20.24)
  return(returnobject)
}

