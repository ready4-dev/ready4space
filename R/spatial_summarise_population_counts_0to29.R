#' @title
#' Produces a summary of population counts for 0-29 year old females, males and persons.
#'
#' @description
#' Updates an inputted tibble with summaries by the unit of resolution (currently SA2 or LGA).
#'
#' @family spatial functions.
#'
#' @details  Note that group by action uses Mean rather than Sum function : it is therefore
#' assumed that inputted data will not require summing.
#'
#' @param population_sf A SF object of data to be summarised.
#'
#' @param groupvar A String specifying the variable to group data by.
#'
#' @param unitname A String specifying the name of the data resolution unit.
#'
#' @param t0 A String specifying the baseline year.
#'
#' @param t1 A String specifying the follow up year.
#'
#' @return
#' Returns a tibble.
#'
#' @export
#'
#' @examples
#'
spatial_summarise_population_counts_0to29 <- function(population_sf,
                                                      groupvar,
                                                      unitname,
                                                      t0,
                                                      t1){
  unitnamesym<-rlang::sym(unitname)
  f.0.4.t0 <- paste0("y",t0,".Females.0.4")
  f.5.9.t0 <- paste0("y",t0,".Females.5.9")
  f.10.14.t0 <- paste0("y",t0,".Females.10.14")
  f.15.19.t0 <- paste0("y",t0,".Females.15.19")
  f.20.24.t0 <- paste0("y",t0,".Females.20.24")
  f.25.29.t0 <- paste0("y",t0,".Females.25.29")
  m.0.4.t0 <- paste0("y",t0,".Males.0.4")
  m.5.9.t0 <- paste0("y",t0,".Males.5.9")
  m.10.14.t0 <- paste0("y",t0,".Males.10.14")
  m.15.19.t0 <- paste0("y",t0,".Males.15.19")
  m.20.24.t0 <- paste0("y",t0,".Males.20.24")
  m.25.29.t0 <- paste0("y",t0,".Males.25.29")
  f.total0to14.t0 <- paste0("y",t0,".total0to14f")
  m.total0to14.t0 <- paste0("y",t0,".total0to14m")
  p.total0to14.t0 <- paste0("y",t0,".total0to14p")
  f.total15to24.t0 <- paste0("y",t0,".total15to24f")
  m.total15to24.t0 <- paste0("y",t0,".total15to24m")
  p.total15to24.t0 <- paste0("y",t0,".total15to24p")
  f.0.4.t1 <- paste0("y",t1,".Females.0.4")
  f.5.9.t1 <- paste0("y",t1,".Females.5.9")
  f.10.14.t1 <- paste0("y",t1,".Females.10.14")
  f.15.19.t1 <- paste0("y",t1,".Females.15.19")
  f.20.24.t1 <- paste0("y",t1,".Females.20.24")
  f.25.29.t1 <- paste0("y",t1,".Females.25.29")
  m.0.4.t1 <- paste0("y",t1,".Males.0.4")
  m.5.9.t1 <- paste0("y",t1,".Males.5.9")
  m.10.14.t1 <- paste0("y",t1,".Males.10.14")
  m.15.19.t1 <- paste0("y",t1,".Males.15.19")
  m.20.24.t1 <- paste0("y",t1,".Males.20.24")
  m.25.29.t1 <- paste0("y",t1,".Males.25.29")
  f.total0to14.t1 <- paste0("y",t1,".total0to14f")
  m.total0to14.t1 <- paste0("y",t1,".total0to14m")
  p.total0to14.t1 <- paste0("y",t1,".total0to14p")
  f.total15to24.t1 <- paste0("y",t1,".total15to24f")
  m.total15to24.t1 <- paste0("y",t1,".total15to24m")
  p.total15to24.t1 <- paste0("y",t1,".total15to24p")
  population_sf <- population_sf %>%
    dplyr::mutate(part.pop.weighting=Usual.Res.Pop/resident.pop.all.parts) %>%
    dplyr::mutate(Percentile.Australia=Percentile.Australia*part.pop.weighting) %>%
    dplyr::group_by(!! groupvar) %>%
    dplyr::summarize(!! unitname := dplyr::first((!!unitnamesym)),
                     STE_NAME16 = dplyr::first(STE_NAME16),
                     AREASQKM16 = mean(AREASQKM16),
                     !!f.0.4.t0 := mean(!!rlang::sym(f.0.4.t0)),
                     !!f.5.9.t0 := mean(!!rlang::sym(f.5.9.t0)),
                     !!f.10.14.t0 := mean(!!rlang::sym(f.10.14.t0)),
                     !!f.15.19.t0 := mean(!!rlang::sym(f.15.19.t0)),
                     !!f.20.24.t0 := mean(!!rlang::sym(f.20.24.t0)),
                     !!f.25.29.t0 := mean(!!rlang::sym(f.25.29.t0)),
                     !!m.0.4.t0 := mean(!!rlang::sym(m.0.4.t0)),
                     !!m.5.9.t0 := mean(!!rlang::sym(m.5.9.t0)),
                     !!m.10.14.t0 := mean(!!rlang::sym(m.10.14.t0)),
                     !!m.15.19.t0 := mean(!!rlang::sym(m.15.19.t0)),
                     !!m.20.24.t0 := mean(!!rlang::sym(m.20.24.t0)),
                     !!m.25.29.t0 := mean(!!rlang::sym(m.25.29.t0)),
                     !!f.total0to14.t0 := mean(!!rlang::sym(f.total0to14.t0)),
                     !!m.total0to14.t0 := mean(!!rlang::sym(m.total0to14.t0)),
                     !!p.total0to14.t0 := mean(!!rlang::sym(f.total0to14.t0)) + mean(!!rlang::sym(m.total0to14.t0)),
                     !!f.total15to24.t0 := mean(!!rlang::sym(f.total15to24.t0)),
                     !!m.total15to24.t0 := mean(!!rlang::sym(m.total15to24.t0)),
                     !!p.total15to24.t0 := mean(!!rlang::sym(f.total15to24.t0)) + mean(!!rlang::sym(m.total15to24.t0)),
                     !!f.0.4.t1 := mean(!!rlang::sym(f.0.4.t1)),
                     !!f.5.9.t1 := mean(!!rlang::sym(f.5.9.t1)),
                     !!f.10.14.t1 := mean(!!rlang::sym(f.10.14.t1)),
                     !!f.15.19.t1 := mean(!!rlang::sym(f.15.19.t1)),
                     !!f.20.24.t1 := mean(!!rlang::sym(f.20.24.t1)),
                     !!f.25.29.t1 := mean(!!rlang::sym(f.25.29.t1)),
                     !!m.0.4.t1 := mean(!!rlang::sym(m.0.4.t1)),
                     !!m.5.9.t1 := mean(!!rlang::sym(m.5.9.t1)),
                     !!m.10.14.t1 := mean(!!rlang::sym(m.10.14.t1)),
                     !!m.15.19.t1 := mean(!!rlang::sym(m.15.19.t1)),
                     !!m.20.24.t1 := mean(!!rlang::sym(m.20.24.t1)),
                     !!m.25.29.t1 := mean(!!rlang::sym(m.25.29.t1)),
                     !!f.total0to14.t1 := mean(!!rlang::sym(f.total0to14.t1)),
                     !!m.total0to14.t1 := mean(!!rlang::sym(m.total0to14.t1)),
                     !!p.total0to14.t1 := mean(!!rlang::sym(f.total0to14.t1)) + mean(!!rlang::sym(m.total0to14.t0)),
                     !!f.total15to24.t1 := mean(!!rlang::sym(f.total15to24.t1)),
                     !!m.total15to24.t1 := mean(!!rlang::sym(m.total15to24.t1)),
                     !!p.total15to24.t1 := mean(!!rlang::sym(f.total15to24.t1)) + mean(!!rlang::sym(m.total15to24.t1)),
                     seifa.percentile = sum(Percentile.Australia)#,
                     #total.pop.2011=mean(resident.pop.all.parts)
    )
  return(population_sf)
}

