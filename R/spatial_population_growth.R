#' @title
#' Calculate population growth numbers and rate.
#'
#' @description
#' This function updates an inputted tibble with the actual or projected growth in a profiled
#' region by each unit of resolution.
#'
#' @family spatial functions.
#'
#' @details
#'
#' @param population_tib A tibble of population projection data for a profiled region
#' broken down by unit of resolution.
#'
#' @param t0 A String specifying the baseline year. This must match up with the years
#' specified in the datasource specified in population_tib.
#'
#' @param t1 A String specifying the followup year. This must match up with the years
#' specified in the datasource specified in population_tib.
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
#'  \code{\link[rlang]{sym}}
#'  \code{\link[dplyr]{mutate}}
#' @rdname spatial_population_growth
#' @export
#' @importFrom rlang sym
#' @importFrom dplyr mutate

spatial_population_growth <- function (population_tib,
                                       t0,
                                       t1){
  horizon<-as.character(as.numeric(t1)-as.numeric(t0))
  growth_n_y_f_0_4 <- paste0("growth.n.",horizon,"y.",t1,".f.0.4")
  growth_n_y_f_0_4_sym <- rlang::sym(growth_n_y_f_0_4)
  growth_pc_y_f_0_4 <- paste0("growth.pc.",horizon,"y.",t1,".f.0.4")
  t1_females_0_4 <- rlang::sym(paste0("y",t1,".Females.0.4"))
  t0_females_0_4 <- rlang::sym(paste0("y",t0,".Females.0.4"))
  growth_n_y_f_5_9 <- paste0("growth.n.",horizon,"y.",t1,".f.5.9")
  growth_n_y_f_5_9_sym <- rlang::sym(growth_n_y_f_5_9)
  growth_pc_y_f_5_9 <- paste0("growth.pc.",horizon,"y.",t1,".f.5.9")
  t1_females_5_9 <- rlang::sym(paste0("y",t1,".Females.5.9"))
  t0_females_5_9 <- rlang::sym(paste0("y",t0,".Females.5.9"))
  growth_n_y_f_10_14 <- paste0("growth.n.",horizon,"y.",t1,".f.10.14")
  growth_n_y_f_10_14_sym <- rlang::sym(growth_n_y_f_10_14)
  growth_pc_y_f_10_14 <- paste0("growth.pc.",horizon,"y.",t1,".f.10.14")
  t1_females_10_14 <- rlang::sym(paste0("y",t1,".Females.10.14"))
  t0_females_10_14 <- rlang::sym(paste0("y",t0,".Females.10.14"))
  growth_n_y_f_15_19 <- paste0("growth.n.",horizon,"y.",t1,".f.15.19")
  growth_n_y_f_15_19_sym <- rlang::sym(growth_n_y_f_15_19)
  growth_pc_y_f_15_19 <- paste0("growth.pc.",horizon,"y.",t1,".f.15.19")
  t1_females_15_19 <- rlang::sym(paste0("y",t1,".Females.15.19"))
  t0_females_15_19 <- rlang::sym(paste0("y",t0,".Females.15.19"))
  growth_n_y_f_20_24 <- paste0("growth.n.",horizon,"y.",t1,".f.20.24")
  growth_n_y_f_20_24_sym <- rlang::sym(growth_n_y_f_20_24)
  growth_pc_y_f_20_24 <- paste0("growth.pc.",horizon,"y.",t1,".f.20.24")
  t1_females_20_24 <- rlang::sym(paste0("y",t1,".Females.20.24"))
  t0_females_20_24 <- rlang::sym(paste0("y",t0,".Females.20.24"))
  growth_n_y_f_25_29 <- paste0("growth.n.",horizon,"y.",t1,".f.25.29")
  growth_n_y_f_25_29_sym <- rlang::sym(growth_n_y_f_25_29)
  growth_pc_y_f_25_29 <- paste0("growth.pc.",horizon,"y.",t1,".f.25.29")
  t1_females_25_29 <- rlang::sym(paste0("y",t1,".Females.25.29"))
  t0_females_25_29 <- rlang::sym(paste0("y",t0,".Females.25.29"))
  growth_n_y_m_0_4 <- paste0("growth.n.",horizon,"y.",t1,".m.0.4")
  growth_n_y_m_0_4_sym <- rlang::sym(growth_n_y_m_0_4)
  growth_pc_y_m_0_4 <- paste0("growth.pc.",horizon,"y.",t1,".m.0.4")
  t1_males_0_4 <- rlang::sym(paste0("y",t1,".Males.0.4"))
  t0_males_0_4 <- rlang::sym(paste0("y",t0,".Males.0.4"))
  growth_n_y_m_5_9 <- paste0("growth.n.",horizon,"y.",t1,".m.5.9")
  growth_n_y_m_5_9_sym <- rlang::sym(growth_n_y_m_5_9)
  growth_pc_y_m_5_9 <- paste0("growth.pc.",horizon,"y.",t1,".m.5.9")
  t1_males_5_9 <- rlang::sym(paste0("y",t1,".Males.5.9"))
  t0_males_5_9 <- rlang::sym(paste0("y",t0,".Males.5.9"))
  growth_n_y_m_10_14 <- paste0("growth.n.",horizon,"y.",t1,".m.10.14")
  growth_n_y_m_10_14_sym <- rlang::sym(growth_n_y_m_10_14)
  growth_pc_y_m_10_14 <- paste0("growth.pc.",horizon,"y.",t1,".m.10.14")
  t1_males_10_14 <- rlang::sym(paste0("y",t1,".Males.10.14"))
  t0_males_10_14 <- rlang::sym(paste0("y",t0,".Males.10.14"))
  growth_n_y_m_15_19 <- paste0("growth.n.",horizon,"y.",t1,".m.15.19")
  growth_n_y_m_15_19_sym <- rlang::sym(growth_n_y_m_15_19)
  growth_pc_y_m_15_19 <- paste0("growth.pc.",horizon,"y.",t1,".m.15.19")
  t1_males_15_19 <- rlang::sym(paste0("y",t1,".Males.15.19"))
  t0_males_15_19 <- rlang::sym(paste0("y",t0,".Males.15.19"))
  growth_n_y_m_20_24 <- paste0("growth.n.",horizon,"y.",t1,".m.20.24")
  growth_n_y_m_20_24_sym <- rlang::sym(growth_n_y_m_20_24)
  growth_pc_y_m_20_24 <- paste0("growth.pc.",horizon,"y.",t1,".m.20.24")
  t1_males_20_24 <- rlang::sym(paste0("y",t1,".Males.20.24"))
  t0_males_20_24 <- rlang::sym(paste0("y",t0,".Males.20.24"))
  growth_n_y_m_25_29 <- paste0("growth.n.5y.",t1,".m.25.29")
  growth_n_y_m_25_29_sym <- rlang::sym(growth_n_y_m_25_29)
  growth_pc_y_m_25_29 <- paste0("growth.pc.",horizon,"y.",t1,".m.25.29")
  t1_males_25_29 <- rlang::sym(paste0("y",t1,".Males.25.29"))
  t0_males_25_29 <- rlang::sym(paste0("y",t0,".Males.25.29"))
  population_tib <- population_tib %>%
    dplyr::mutate(!!growth_n_y_f_0_4 := !!t1_females_0_4 - !!t0_females_0_4,
                  !!growth_n_y_f_5_9 := !!t1_females_5_9 - !!t0_females_5_9,
                  !!growth_n_y_f_10_14 := !!t1_females_10_14 - !!t0_females_10_14,
                  !!growth_n_y_f_15_19 := !!t1_females_15_19 - !!t0_females_15_19,
                  !!growth_n_y_f_20_24 := !!t1_females_20_24 - !!t0_females_20_24,
                  !!growth_n_y_f_25_29 := !!t1_females_25_29 - !!t0_females_25_29,
                  !!growth_n_y_m_0_4 := !!t1_males_0_4 - !!t0_males_0_4,
                  !!growth_n_y_m_5_9 := !!t1_males_5_9 - !!t0_males_5_9,
                  !!growth_n_y_m_10_14 := !!t1_males_10_14 - !!t0_males_10_14,
                  !!growth_n_y_m_15_19 := !!t1_males_15_19 - !!t0_males_15_19,
                  !!growth_n_y_m_20_24 := !!t1_males_20_24 - !!t0_males_20_24,
                  !!growth_n_y_m_25_29 := !!t1_males_25_29 - !!t0_males_25_29)
  population_tib <- population_tib %>%
    dplyr::mutate(!!growth_pc_y_f_0_4 := 100*(!!growth_n_y_f_0_4_sym)/(!!t0_females_0_4),
                  !!growth_pc_y_f_5_9 := 100*(!!growth_n_y_f_5_9_sym)/(!!t0_females_5_9),
                  !!growth_pc_y_f_10_14 := 100*(!!growth_n_y_f_10_14_sym)/(!!t0_females_10_14),
                  !!growth_pc_y_f_15_19 := 100*(!!growth_n_y_f_15_19_sym)/(!!t0_females_15_19),#y2016.Females.15.19,
                  !!growth_pc_y_f_20_24 := 100*(!!growth_n_y_f_20_24_sym)/(!!t0_females_20_24),#y2016.Females.20.24,
                  !!growth_pc_y_f_25_29 := 100*(!!growth_n_y_f_25_29_sym)/(!!t0_females_25_29),#y2016.Females.25.29,
                  !!growth_pc_y_m_0_4 := 100*(!!growth_n_y_m_0_4_sym)/(!!t0_males_0_4),
                  !!growth_pc_y_m_5_9 := 100*(!!growth_n_y_m_5_9_sym)/(!!t0_males_5_9),
                  !!growth_pc_y_m_10_14 := 100*(!!growth_n_y_m_10_14_sym)/(!!t0_males_10_14),
                  !!growth_pc_y_m_15_19 := 100*(!!growth_n_y_m_15_19_sym)/(!!t0_males_15_19),#y2016.Males.15.19,
                  !!growth_pc_y_m_20_24 := 100*(!!growth_n_y_m_20_24_sym)/(!!t0_males_20_24),#y2016.Males.20.24,
                  !!growth_pc_y_m_25_29 := 100*(!!growth_n_y_m_25_29_sym)/(!!t0_males_25_29)) #y2016.Males.25.29)
  return(population_tib)
}
