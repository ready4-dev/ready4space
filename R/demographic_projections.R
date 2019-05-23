#' @title demographic_projections
#' @description Forecast population groth with prediction intervals.
#' @details Function implements calculations based on:
#' http://www.jstor.org.ezp.lib.unimelb.edu.au/stable/41110880?seq=1#page_scan_tab_contents
#' @param naive A logical value, denoting whether using a naive modelling approach (assumes
#' previous growth rates continue into future) or a theory based model.
#' @param n.replications A positive integer.
#' @return A list.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{summarise}},\code{\link[dplyr]{slice}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{pull}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[stats]{setNames}}
#' @rdname demographic_projections
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr summarise slice bind_rows select mutate pull
#' @importFrom purrr pmap
#' @importFrom stats setNames
## Needs to be rewritten, with parameters inputted and using classes for generalisation beyond Australian context.
demographic_projections <- function(naive,
                                    n.replications){
  naive.model.apes<-tibble::tibble(twoyear=c(-0.61,
                                             -0.61,
                                             -0.27,
                                             -0.58,
                                             0.45,
                                             0.35,
                                             -0.28,
                                             0.20,
                                             0.09,
                                             0.24),
                                   fiveyear=c(-0.88,
                                              -0.88,
                                              -1.01,
                                              -0.43,
                                              1.86,
                                              0.84,
                                              -0.28,
                                              -0.05,
                                              0.28,
                                              NA),
                                   tenyear=c(-2.29,
                                             -2.29,
                                             -0.18,
                                             0.75,
                                             3.83,
                                             1.43,
                                             -0.6,
                                             NA,
                                             NA,
                                             NA),
                                   fifteenyear=c(-1.89,
                                                 -1.89,
                                                 0.84,
                                                 1.74,
                                                 5.68,
                                                 NA,
                                                 NA,
                                                 NA,
                                                 NA,
                                                 NA),
                                   twentyyear=c(-1.57,
                                                -1.57,
                                                1.72,
                                                NA,
                                                NA,
                                                NA,
                                                NA,
                                                NA,
                                                NA,
                                                NA)
  )
  naive.model.apes %>%
    dplyr::summarise(twoyearmape=mean(twoyear),
                     fiveyearmape=mean(!is.na(fiveyear)),
                     tenyearmape=mean(!is.na(tenyear)),
                     fifteenyearmape=mean(!is.na(fifteenyear)),
                     twentyyearmape=mean(!is.na(twentyyear))
    )
  naive.model.apes.1981.1995<-naive.model.apes %>%
    dplyr::slice(1:7)
  naive.model.mape.1981.1995<-naive.model.apes.1981.1995 %>%
    dplyr::summarise(twoyearmape=mean(twoyear),
                     fiveyearmape=mean(!is.na(fiveyear)),
                     tenyearmape=mean(!is.na(tenyear)),
                     fifteenyearmape=mean(!is.na(fifteenyear)),
                     twentyyearmape=mean(!is.na(twentyyear))
    )
  prediction.mape.1981.1995<-tibble::tibble(twoyearmape=0.3,
                                            fiveyearmape=0.7,
                                            tenyearmape=1.1,
                                            fifteenyearmape=NA,
                                            twentyyearmape=NA
  )

  model.comparison<-dplyr::bind_rows(naive.model.mape.1981.1995,
                                     prediction.mape.1981.1995)
  model.comparison<-model.comparison %>%
    dplyr::select(twoyearmape,
                  fiveyearmape,
                  tenyearmape)
  error.multiplier.for.niaive<-1-(abs(model.comparison %>%
                                        dplyr::slice(2)
  )- abs(model.comparison %>%
           dplyr::slice(1)
  )
  ) / abs(model.comparison %>%
            dplyr::slice(2)
  )
  prediction.mapes.age.sex.5yr<-tibble::tibble(female.10.14=1.6,
                                               female.15.9=1.2,
                                               female.20.24=2,
                                               female.25.29=2.7,
                                               male.10.14=1.5,
                                               male.15.9=1,
                                               male.20.24=1.7,
                                               male.25.29=3
  )
  if(naive)
    prediction.mapes.age.sex.5yr<-prediction.mapes.age.sex.5yr*(error.multiplier.for.niaive %>%
                                                                  dplyr::select(fiveyearmape))[1,1]
  input.tibble<-tibble::tibble(mean.abs.error=as.numeric(prediction.mapes.age.sex.5yr),
                               sex=c("female",
                                     "female",
                                     "female",
                                     "female",
                                     "male",
                                     "male",
                                     "male",
                                     "male"),
                               age=c("10.14",
                                     "15.19",
                                     "20.24",
                                     "25.29",
                                     "10.14",
                                     "15.19",
                                     "20.24",
                                     "25.29"),
                               #three.std.devs.from.mean.error=rep(15,8),
                               n.five.year.predictions=rep((naive.model.apes %>%
                                                              dplyr::select(fiveyear) %>%
                                                              dplyr::summarise(countstat=sum(!is.na(.))))[[1,1]],8))
  input.tibble<-input.tibble %>%
    dplyr::mutate(three.std.devs.from.mean.error=pmax(2,
                                                      pmin(15,5*mean.abs.error)))
  input.tibble<-input.tibble %>%
    dplyr::mutate(nameslist=paste0(sex,".",age))
  growth.rate.error.list<-purrr::pmap(list(a=input.tibble %>%
                                             dplyr::select(mean.abs.error) %>%
                                             dplyr::pull(),
                                           b=input.tibble %>%
                                             dplyr::select(three.std.devs.from.mean.error) %>%
                                             dplyr::pull(),
                                           c=input.tibble %>%
                                             dplyr::select(n.five.year.predictions) %>%
                                             dplyr::pull()),
                                      ~ demographic_range_of_projections(mean.abs.error=..1,
                                                                         three.std.devs.from.mean.error=..2,
                                                                         n.five.year.predictions=..3,
                                                                         n.replications = n.replications)) %>%
    stats::setNames(input.tibble %>%
                      dplyr::select(nameslist) %>%
                      dplyr::pull())
  return(growth.rate.error.list)
}
