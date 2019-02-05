vic_lga_y_16_31 <- ready.space::spatial_vic_pop_growth_lga(
  vic_pop_growth_by_age_lga_t0 =
    ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                         lookup_reference = "vic_pop_growth_by_age_lga_2016_tb"),
  vic_pop_growth_by_age_lga_t1 =
    ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                         lookup_reference = "vic_pop_growth_by_age_lga_2031_tb"),
  t0 ="2016",
  t1 ="2031")


## Get yearly age / sex population projections for included years

vic_lga_age_12_25_y_16_19_31_25 <- ready.space::demographic_by_yearly_age_sex(profiled_sf = vic_lga_y_16_31,
                                                                                  years = c(2016,2019, 2031, 2025),
                                                                                  age0 = 12,
                                                                                  age1 = 18)

## Apply prevalence estimates
prev_rates <- ymh.epi::prev_rates
pref_source <- ymh.epi::pref_source
pick_rate_from_source <-function(disorder,
                                 period,
                                 source,
                                 age,
                                 sex,
                                 prev_rates = ymh.epi::prev_rates){
  look_up <- paste0(sex,"_",age)
  sel_rate <- prev_rates  %>% dplyr::filter(Disorder==disorder, Period==period, Source==source) %>% dplyr::select(Female_12) %>% dplyr::pull()
  return(sel_rate)
}
pick_source <- function(disorder,
                        period,
                        age,
                        sex,
                        pref_source = ymh.epi::pref_source){
  look_up <- paste0(sex,"_",age)
  sel_source <- pref_source  %>% dplyr::filter(Disorder==disorder, Period==period) %>% dplyr::select(Female_12) %>% dplyr::pull() %>% na.omit %>% purrr::pluck(1)
  return(sel_source)
}
sel_source <- pick_source("Anxiety", "Year", age=12, sex="Female")
pick_rate_from_source("Anxiety", "Year", source=sel_source, age=12, sex="Female")
# disorder <- "Anxiety"
# period <- "Year"
# ages <- 13:19
# sexes <- c("Female", "Male")
pref_sources_for_age_range <- function(disorder,
                                     period,
                                     ages,
                                     sexes,
                                     pref_source = ymh.epi::pref_source,
                                     prev_rates = ymh.epi::prev_rates){
  base_tib <- pref_source %>% dplyr::select(Disorder, Period) %>% dplyr::filter(Disorder == disorder,
                                                                                Period == period)
  add_each_age <- function(ages,
                           sex){purrr::map_chr(ages,
                                                ~ paste0(sex,"_",.))}
  #add_each_age(ages, "Female")
  new_cols <- purrr::map(sexes,
                  ~ add_each_age(ages = ages, sex = .)) %>% purrr::flatten_chr()

  purrr::reduce(purrr::prepend(new_cols,
                               list(a=base_tib)),
                .f = function(x,y) x %>%
                  dplyr::mutate(!!y := pick_source(disorder = disorder,
                                                                        period = period,
                                                                        age = stringr::str_sub(y,-2),
                                                                        sex = stringr::str_sub(y,1,-4))))
}
age_sex_source_tb <- pref_sources_for_age_range("Anxiety","Year",13:19,sexes="Female")
# %>%
#   dplyr::rename_at(dplyr::vars(dplyr::contains("Female")),
#                    dplyr::funs(gsub("Female","f",.))) %>%
#   dplyr::rename_at(dplyr::vars(dplyr::contains("Male")),
#                    dplyr::funs(gsub("Male","m",.)))

age_sex_source_tb <- age_sex_source_tb %>%
  dplyr::select(dplyr::starts_with("Female_"),
                dplyr::starts_with(("Male_")))
age_sex_vec  <- age_sex_source_tb %>%
  names()
source_vec <- age_sex_source_tb %>%
   unlist() %>%
  as.vector()

prev_rates_vec <- purrr::map2_dbl(age_sex_vec,
                source_vec,
                ~ pick_rate_from_source("Anxiety",
                                        "Year",
                                        source=.y,
                                        age=.x %>%
                                          stringr::str_sub(start=-2) %>%
                                          as.numeric(),
                                        sex=.x %>%
                                          stringr::str_sub(end=-4))) %>%
  stats::setNames(age_sex_vec %>%
                    gsub("Female","f",.)%>%
                    gsub("Male","m",.))

pop_totals_vec <- env_sf(st_envir(sim_data)) %>% names()
pop_totals_vec <- pop_totals_vec[pop_totals_vec%>% startsWith(prefix="tx_")]
pop_totals_vec <-pop_totals_vec[purrr::map_lgl(pop_totals_vec,
               ~ .x %>% stringr::str_sub(start = -4) %in% names(prev_rates_vec))]
#ann_prev_rates <- prev_rates %>% dplyr::filter(Period=="Year")
#rates_to_apply <-
