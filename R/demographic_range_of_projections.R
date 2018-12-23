#' @title
#' Estimate a vector of demographic projections.
#'
#' @description
#' This function creates a vector......
#'
#' @family demographic modelling functions.
#'
#' @details
#' Implements formula from http://www.healthstrategy.com/cgi-bin/datagen.pl
#' Converts absolute mean error into +/- range.
#'
#' @param mean.abs.error A .....
#'
#' @param three.std.devs.from.mean.error A .....
#'
#' @param n.five.year.predictions A positive integer.
#'
#' @param n.replications A positive integer.
#'
#' @return
#' A tibble.
#'
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[invgamma]{invgamma}}
#' @rdname demographic_range_of_projections
#' @export
#' @importFrom invgamma rinvgamma
#'
demographic_range_of_projections<-function(mean.abs.error,
                            three.std.devs.from.mean.error,
                            n.five.year.predictions,
                            n.replications){
  std.dev.error <- three.std.devs.from.mean.error/3
  std.err.error <- std.dev.error / sqrt(n.five.year.predictions)

  alpha<-(mean.abs.error*mean.abs.error) / (std.err.error*std.err.error)
  beta<-(std.err.error*std.err.error) / mean.abs.error
  potential.growth.rates<-c(invgamma::rinvgamma(n.replications/2,alpha,beta),
                            -invgamma::rinvgamma(n.replications/2,alpha,beta)
  )
  potential.growth.rates <- sort(potential.growth.rates)
  return(potential.growth.rates)
}
