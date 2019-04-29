make_ced_summary_for_ste <- function(ste,
                                     ced_boundaries_sf,
                                     ced_lookup_tbs,
                                     validation_tb = readxl::read_xls("C:/Users/mahamilton/Desktop/CED/CED_pops_2017.xls",
                                                                       sheet = "Matt Calcs"),
                                     output_file_path = "C:/Users/mahamilton/Desktop/CED/default_stats.csv"){
  all_electorates <- ced_boundaries_sf  %>%
    dplyr::pull(CED_NAME18) %>%
    as.character()
  multi_state_electorates <-all_electorates[duplicated(all_electorates)]
  sngl_state_electorates <- all_electorates[!all_electorates %in% multi_state_electorates]
  all_electorates <- all_electorates[!duplicated(all_electorates)]
  ste_electorates <- ced_boundaries_sf  %>%
    dplyr::filter(STE_NAME16 %in% ste) %>%
    dplyr::pull(CED_NAME18) %>%
    as.character() %>%
    unique()
  sngl_state_ste_electorates <- ste_electorates[!ste_electorates %in% multi_state_electorates]
  profiled_area_input <- list(profiled_area_type = "CED",
                              geom_dist_limit_km = NA_real_,
                              drive_time_limit_mins = NA_real_,
                              profiled_area = sngl_state_ste_electorates)
  profiled_area_input <-ymh.epi.sim::transform_profiled_area_inputs(profiled_area_input = profiled_area_input,
                                                                    lookup_tb_r4 = ced_lookup_tbs)

  input_data <- list(
    ## DEMOGRAPHIC INPUTS
    age_lower = 12,
    age_upper = 25,
    sexes = c("Female","Male"),
    ## EPIDEMIOLOGICAL INPUTS
    disorder = "Any_Common",
    period = "Year",
    ## SPATIAL INPUTS
    at_highest_res = c("ERP", "ERP by age and sex", "Population projections"),
    at_specified_res = list(a=c("SEIFA","SA2")),
    age_sex_pop_str = "ERP by age and sex",
    group_at_profile_unit = TRUE,
    pop_projs_str = "Population projections",
    profiled_area_input = profiled_area_input,
    tot_pop_str = "ERP",
    ## TEMPORAL INPUTS
    model_start_ymdhms = lubridate::ymd_hms("2019_07_01 12:00:00"),
    nbr_steps_start_to_end = 1,
    simulation_steps_ymwd = c(1,0,0,0),
    ## UNCERTAINTY INPUTS
    deterministic = TRUE,
    nbr_its = 1,
    uncertainty_int = c(0.025,0.975),
    ## PARAMETER MATRIX
    env_str_par_tb = ready.sim::add_prev_to_env_str_par_tb(pref_source = ymh.epi.lit::pref_source,
                                                           prev_rates = ymh.epi.lit::prev_rates,
                                                           env_str_par_tb = ready.aus.prev::par_str_environment_tb,
                                                           Period = "Year",
                                                           disorder = "Any_Common",
                                                           age_range = c(12,25),
                                                           sexes = c("Female","Male"))
  )
  sim_data <-
    #ready.space::
    make_sim_data_env(input_data = input_data)
  grouping_for_sim <- ifelse(!is.na(input_data$profiled_area_input %>%
                                      ready.s4::geom_dist_limit_km()),
                             "distance_km",
                             ifelse(!is.na(input_data$profiled_area_input %>%
                                             ready.s4::drive_time_limmit_mins()),
                                    "drive_times",
                                    "SA2_MAIN16"
                                    # "CED_NAME18"
                             ))
  sim_results_ls <- ready.sim::runSimulation(x = sim_data,
                                             nbr_its = input_data$nbr_its,
                                             group_by = grouping_for_sim)
  prev_inp_tbl <- make_epi_inputs_tb(env_str_par_tb = input_data$env_str_par_tb,
                                     epi_stat = "prev",
                                     disorder = input_data$disorder)
  epi_results_tbs_ls <- make_epi_results_tbs_ls(sim_results_ls = sim_results_ls,
                                                input_parameters_ls = input_data)
  sim_res_tb <- sim_results_ls[[1]] %>%
    dplyr::mutate(electorate = get_electorate(pop_sp_unit_id))
  sim_res_tb_grpd <- sim_res_tb %>%
    dplyr::group_by(electorate) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::starts_with("t0")),
                        sum)
  sim_res_tb_grpd <- sim_res_tb_grpd %>%
    dplyr::mutate(t0_prev_any_common_f_12_17 = t0_prev_any_common_f_12 +
                    t0_prev_any_common_f_13 +
                    t0_prev_any_common_f_14 +
                    t0_prev_any_common_f_15 +
                    t0_prev_any_common_f_16 +
                    t0_prev_any_common_f_17,
                  t0_prev_any_common_m_12_17 = t0_prev_any_common_m_12 +
                    t0_prev_any_common_m_13 +
                    t0_prev_any_common_m_14 +
                    t0_prev_any_common_m_15 +
                    t0_prev_any_common_m_16 +
                    t0_prev_any_common_m_17,
                  t0_prev_any_common_f_18_24 = t0_prev_any_common_f_18 +
                    t0_prev_any_common_f_19 +
                    t0_prev_any_common_f_20 +
                    t0_prev_any_common_f_21 +
                    t0_prev_any_common_f_22 +
                    t0_prev_any_common_f_23 +
                    t0_prev_any_common_f_24,
                  t0_prev_any_common_m_18_24 = t0_prev_any_common_m_18 +
                    t0_prev_any_common_m_19 +
                    t0_prev_any_common_m_20 +
                    t0_prev_any_common_m_21 +
                    t0_prev_any_common_m_22 +
                    t0_prev_any_common_m_23 +
                    t0_prev_any_common_m_24) %>%
    dplyr::mutate(t0_prev_mod_sev_any_common_p_12_17 = (0.33 + 0.23)*(t0_prev_any_common_f_12_17 + t0_prev_any_common_m_12_17),
                  t0_prev_mod_sev_any_common_p_18_25 = (0.28 + 0.14)*(t0_prev_any_common_f_18_24 + t0_prev_any_common_m_18_24 +
                                                                        t0_prev_any_common_f_25 + t0_prev_any_common_m_25),
                  t0_prev_mod_sev_any_common_p_tl = t0_prev_mod_sev_any_common_p_12_17 + t0_prev_mod_sev_any_common_p_18_25)
  sim_res_tb_grpd_sum <- sim_res_tb_grpd  %>%
    dplyr::select(electorate,
                  t0_20160701_p_tl,
                  t0_prev_any_common_p_tl,
                  t0_prev_mod_sev_any_common_p_tl) %>%
    sf::st_set_geometry(NULL)
  sim_res_tb_grpd_sum <- sim_res_tb_grpd_sum %>%
    dplyr::rename(age_12_to_25_erp = t0_20160701_p_tl,
                  prevalent_any_common_12_to_25 = t0_prev_any_common_p_tl,
                  moderate_to_severe_any_common_12_to_25 = t0_prev_mod_sev_any_common_p_tl)
  sim_data_sum <- ready.sim::st_envir(sim_data) %>%
    ready.sim::st_data() %>%
    purrr::pluck("profiled_sf") %>%
    dplyr::select(CED_NAME18,
                  whl_SA1_year_2011,
                  whl_SA1_year_2016pr,
                  inc_SA1_prop,
                  inc_SA1_popl_inc_SA1_popl,
                  dplyr::starts_with("inc_SA1_popl_y2016.")) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::mutate(inc_SA1_all_age_pop_2011 = whl_SA1_year_2011 * inc_SA1_prop) %>%
    dplyr::mutate(u15 = inc_SA1_popl_y2016.Females.0.4 +
                    inc_SA1_popl_y2016.Females.5.9 +
                    inc_SA1_popl_y2016.Females.10.14 +
                    inc_SA1_popl_y2016.Males.0.4 +
                    inc_SA1_popl_y2016.Males.5.9 +
                    inc_SA1_popl_y2016.Males.10.14) %>%
    dplyr::mutate(u18o15 = inc_SA1_popl_y2016.Females.15.19 * 3/5 +
                    inc_SA1_popl_y2016.Males.15.19 * 3/5) %>%
    dplyr::mutate(u18 = u15+ u18o15) %>%
    dplyr::group_by(CED_NAME18) %>%
    dplyr::summarise(all_age_pop_2011 = sum(inc_SA1_all_age_pop_2011),
                     projected_all_age_pop_2016 = sum(inc_SA1_popl_inc_SA1_popl),
                     under_18_pop_2016 = sum(u18),
                     best_guess_adult_pop_2016 = projected_all_age_pop_2016 - under_18_pop_2016)
  ced_summary_tb <- dplyr::inner_join(sim_res_tb_grpd_sum,
                                      sim_data_sum,
                                      by = c("electorate" = "CED_NAME18"))
  # validation_tb <- readxl::read_xls("C:/Users/mahamilton/Desktop/CED/CED_pops_2017.xls",
  #                                   sheet = "Matt Calcs")
  ced_summary_tb_with_valid <- dplyr::inner_join(ced_summary_tb,
                                                 validation_tb)
  ced_summary_tb_with_valid <- ced_summary_tb_with_valid %>%
    dplyr::select(electorate,
                  over_18_2017,
                  age_12_to_25_erp,
                  prevalent_any_common_12_to_25,
                  moderate_to_severe_any_common_12_to_25,
                  all_age_pop_2011,
                  projected_all_age_pop_2016,
                  all_age_2017,
                  under_18_pop_2016,
                  u18_2017,
                  best_guess_adult_pop_2016) %>%
    dplyr::rename(adult_erp_2017 = over_18_2017,
                  aged_12_to_25_erp_2016 = age_12_to_25_erp,
                  aged_12_to_25_prevalent_common_2016 = prevalent_any_common_12_to_25,
                  aged_12_to_25_moderate_to_severe_common_2016 = moderate_to_severe_any_common_12_to_25,
                  validation_all_age_pop_2011 = all_age_pop_2011,
                  validation_proj_all_age_erp_2016 = projected_all_age_pop_2016,
                  validation_all_age_erp_2017 = all_age_2017,
                  validation_under_18_erp_2016 = under_18_pop_2016,
                  validation_under_18_erp_2017 = u18_2017,
                  validation_proj_adult_erp_2016 = best_guess_adult_pop_2016) %>%
    dplyr::mutate(validation_diff_u18_erp = (validation_under_18_erp_2017 - validation_under_18_erp_2016) / validation_under_18_erp_2016,
                  validation_diff_adult_erp = (adult_erp_2017 - validation_proj_adult_erp_2016) / validation_proj_adult_erp_2016,
                  validation_diff_all_age_erp = (validation_all_age_erp_2017 - validation_proj_all_age_erp_2016) / validation_proj_all_age_erp_2016) %>%
    dplyr::mutate(potential_youth_pop_calc_error = ifelse(validation_diff_u18_erp > 0.05, TRUE, FALSE),
                  potential_all_age_pop_calc_error = ifelse(validation_diff_all_age_erp >0.10, TRUE, FALSE))
  write.csv(ced_summary_tb_with_valid,output_file_path)
}

get_electorate <- function(electorate_sub_divs){
  purrr::map_chr(electorate_sub_divs,
                 ~ #strt_end <- stringr::str_locate_all(.,"_")[[1]][2:3,1]+c(1,-1)
                   stringr::str_sub(.,
                                    start = stringr::str_locate_all(.,"_")[[1]][2,1] + 1,
                                    end = stringr::str_locate_all(.,"_")[[1]][3,1] - 1))
}
make_plot_list <- function(profiled_sf,
                           plot_names_vec,
                           plot_col_names_vec,
                           plot_titles_vec,
                           plot_legend_vec){
  purrr::pmap(list(plot_col_names_vec = plot_col_names_vec,
                   plot_titles_vec = plot_titles_vec,
                   plot_legend_vec = plot_legend_vec),
              ~ ggplot2::ggplot(profiled_sf) +
                ggplot2::geom_sf(ggplot2::aes(fill = !!rlang::sym(..1)),colour=NA) +
                ggplot2::ggtitle(..2) +
                viridis::scale_fill_viridis(..3) +
                ggplot2::theme_bw()) %>%
    stats::setNames(plot_names_vec)
}
make_epi_inputs_tb <- function(env_str_par_tb,
                               disorder,
                               epi_stat){
  env_str_par_tb %>%
    dplyr::filter(startsWith(param_name,epi_stat)) %>%
    dplyr::select(param_name, deter_val) %>%
    dplyr::mutate(sex = param_name %>%
                    stringr::str_sub(start = -4, end =-4) %>%
                    purrr::map_chr(.,
                                   ~ switch(.x, "f" = "Female", "m" = "Male"))) %>%
    dplyr::mutate(age = param_name %>% stringr::str_sub(start = -2)) %>%
    dplyr::mutate(disorder = disorder) %>%
    dplyr::mutate(annual_prevalence_percent = deter_val *100) %>%
    dplyr::select(disorder,sex,age,annual_prevalence_percent)

}
make_epi_results_tbs_ls <- function(sim_results_ls,
                                    input_parameters_ls){
  all_epi_results_tb <- ready.sim::analyse_results_prev(sim_results = sim_results_ls,
                                                        drop_cols = NULL,
                                                        disorder = input_parameters_ls$disorder,
                                                        data_year = ready.s4::data_year(input_parameters_ls$profiled_area_input),
                                                        model_end_year = (ready.s4::data_year(input_parameters_ls$profiled_area_input) %>%
                                                                            as.numeric() +
                                                                            input_parameters_ls$nbr_steps_start_to_end) %>%
                                                          as.character(),
                                                        input_data = input_parameters_ls,
                                                        deterministic = input_parameters_ls$deterministic,
                                                        uncertainty_int = input_parameters_ls$uncertainty_int)
  t0_res_pop_tb <- all_epi_results_tb %>%
    dplyr::filter(year == ready.s4::data_year(input_parameters_ls$profiled_area_input)) %>%
    dplyr::filter(population_type == "Resident population") %>%
    dplyr::select(-c("year","population_type")) %>%
    dplyr::arrange(sex)
  tx_res_pop_tb <-  all_epi_results_tb %>%
    dplyr::filter(year == (ready.s4::data_year(input_parameters_ls$profiled_area_input) %>%
                             as.numeric() +
                             input_parameters_ls$nbr_steps_start_to_end) %>%
                    as.character()) %>%
    dplyr::filter(population_type == "Resident population") %>%
    dplyr::select(-c("year","population_type")) %>%
    dplyr::arrange(sex)
  dl_res_pop_tb <- all_epi_results_tb %>%
    dplyr::filter(population_type == "Change in resident population since 2016") %>%
    dplyr::select(-c("year","population_type")) %>%
    dplyr::arrange(sex)
  list(t0_res_pop_tb = t0_res_pop_tb,
       tx_res_pop_tb = tx_res_pop_tb,
       dl_res_pop_tb = dl_res_pop_tb)
}
make_summary_pop_tb <- function(sim_results,
                                table_type){
  deterministic <- sim_results %>% purrr::pluck("input_parameters_ls") %>% purrr::pluck("deterministic")
  main_stat <- ifelse(deterministic,"Point_Estimate","Median")
  customGreen0 = "#DeF7E9"
  customGreen = "#71CA97"
  customRed = "#ff7f7f"
  age_lower <- sim_results %>% purrr::pluck("input_parameters_ls") %>% purrr::pluck("age_lower")
  age_upper <- sim_results %>% purrr::pluck("input_parameters_ls") %>% purrr::pluck("age_upper")
  if(table_type == "Total")
    sex_cat <- c("Female", "Male", "Persons")
  else
    sex_cat <- table_type
  res_tb <- sim_results %>% purrr::pluck("epi_results_tbs_ls") %>%
    purrr::pluck("tx_res_pop_tb") %>%
    dplyr::filter(sex %in% sex_cat)
  if(table_type == "Total"){
    res_tb <- res_tb %>%
      dplyr::filter(!age %in% as.character(age_lower:age_upper)) %>%
      dplyr::rename(Sex = sex)
  }else{
    res_tb <- res_tb %>%
      dplyr::filter(age %in% as.character(age_lower:age_upper)) %>%
      dplyr::select(-sex)
  }
  res_tb <-  res_tb %>%
    dplyr::rename(Age = age) %>%
    dplyr::rename(Prediction = !!rlang::sym(main_stat)) %>%
    # dplyr::mutate_if(is.numeric,
    #                  dplyr::funs(round(.,0))) %>%
    dplyr::mutate_if(is.numeric,
                     dplyr::funs(formattable::comma(., digits=0))) %>%
    # dplyr::mutate_if(is.numeric,
    #                  dplyr::funs(prettyNum(., big.mark =","))) %>%
    dplyr::mutate(Prediction = formattable::color_tile(customGreen, customGreen0)(Prediction)) #%>%
  # dplyr::mutate(Prediction = prettyNum(as.numeric(Prediction), big.mark =",")) %>%
  # dplyr::mutate(!!rlang::sym(main_stat) := color_tile(customGreen, customGreen0)(!!rlang::sym(main_stat)))

  if(!deterministic){
    res_tb <-  res_tb  %>%
      dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Low_Bound_"),
                                   dplyr::starts_with("High_Bound_")),
                       dplyr::funs(color_tile(customGreen, customGreen0)(.)))
    res_tb <- rename_q_bounds(res_tb)
  }
  res_tb %>%
    # dplyr::mutate_if(is.numeric,
    #                  dplyr::funs(prettyNum(.,big.mark = ","))) %>%
    knitr::kable(escape = F
                 #, format = "html", digits = 0, format.args = list(big.mark = ",")
    ) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "left")
}
rename_q_bounds <- function(res_tb){
  res_tb %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with("Low_Bound_"),
                                 dplyr::starts_with("High_Bound_")),
                     dplyr::funs(paste0("UI ",
                                        stringr::str_replace_all(.,"_"," ") %>% stringr::str_sub(end=-8),
                                        "(",
                                        (stringr::str_sub(.,start=-5) %>% as.numeric() * 100),
                                        "%)")))
}



