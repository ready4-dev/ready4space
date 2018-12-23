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
disorder <- "Anxiety"
period <- "Year"
ages <- 13:19
sexes <- c("Female", "Male")
pref_sources_for_age_range <- function(disorder,
                                     period,
                                     ages,
                                     sex,
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
#ann_prev_rates <- prev_rates %>% dplyr::filter(Period=="Year")
#rates_to_apply <-
