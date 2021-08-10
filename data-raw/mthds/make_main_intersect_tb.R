make_main_intersect_tb.ready4_lookup <- function(x,
                                                 r_data_dir_chr,
                                                 template_tb,
                                                 tb_ref_var_chr,
                                                 ref_bound_yr,
                                                 ref_unit_chr,
                                                 tgt_bound_yr,
                                                 tgt_unit_chr){
  ref_sf <-  get_data(x@sp_data_pack_lup %>%
                        dplyr::filter(area_bound_yr == ref_bound_yr),
                      "area_type",
                      ref_unit_chr,
                      r_data_dir_chr = r_data_dir_chr)
  tgt_sf <- get_data(x@sp_data_pack_lup %>%
                       dplyr::filter(area_bound_yr == tgt_bound_yr),
                     "area_type",
                     tgt_unit_chr,
                     r_data_dir_chr = r_data_dir_chr)
  ref_var_chr <- x@sp_uid_lup %>% get_data(value_chr = ref_unit_chr,
                                           area_bound_yr = ref_bound_yr)
  tgt_var_chr <- x@sp_uid_lup %>% get_data(value_chr = tgt_unit_chr,
                                           area_bound_yr = tgt_bound_yr)
  ref_sf <- ref_sf %>%
    dplyr::filter(!!rlang::sym(ref_var_chr) %in% (template_tb %>% dplyr::pull(tb_ref_var_chr)))
  ref_sf <- sf::st_intersection(ref_sf %>%
                                  dplyr::mutate(whl_area_km2 =  sf::st_area(.) %>%  units::set_units(km^2)),
                                tgt_sf)
  ref_sf <- ref_sf %>%
    dplyr::arrange(!!rlang::sym(ref_var_chr)) %>%
    dplyr::mutate(inc_area_km2 =  sf::st_area(.) %>%  units::set_units(km^2)) %>%
    dplyr::mutate(inc_fraction_dbl = inc_area_km2 /whl_area_km2)
  ref_sf <- ref_sf %>%
    dplyr::group_by(!!rlang::sym(ref_var_chr)) %>%
    dplyr::summarise(!!rlang::sym(tgt_var_chr) := !!rlang::sym(tgt_var_chr) %>% purrr::pluck(which(inc_fraction_dbl==max(inc_fraction_dbl))))
  template_tb <- template_tb %>%
    dplyr::mutate(!!rlang::sym(tgt_var_chr) := !!rlang::sym(tb_ref_var_chr) %>% as.character() %>% purrr::map_chr(~ ifelse(is.na(.x),
                                                                                                                           NA_character_,
                                                                                                                           ready4fun::get_from_lup(ref_sf  %>% sf::st_set_geometry(NULL),
                                                                                                                                                 lookup_reference = .x,
                                                                                                                                                 lookup_variable = ref_var_chr,
                                                                                                                                                 target_variable = tgt_var_chr,
                                                                                                                                                 evaluate = F))
    ))
}
