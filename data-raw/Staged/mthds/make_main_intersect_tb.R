make_main_intersect_tb.VicinityLookup <- function(x,
                                                 processed_fls_dir_1L_chr,
                                                 template_tb,
                                                 tb_ref_var_chr,
                                                 ref_bound_yr,
                                                 ref_unit_chr,
                                                 tgt_bound_yr,
                                                 tgt_unit_chr){
  ref_sf <-  procure(x@sp_data_pack_lup %>%
                        dplyr::filter(area_bndy_yr_chr == ref_bound_yr),
                      "area_type_chr",
                      ref_unit_chr,
                      processed_fls_dir_1L_chr = processed_fls_dir_1L_chr)
  tgt_sf <- procure(x@sp_data_pack_lup %>%
                       dplyr::filter(area_bndy_yr_chr == tgt_bound_yr),
                     "area_type_chr",
                     tgt_unit_chr,
                     processed_fls_dir_1L_chr = processed_fls_dir_1L_chr)
  ref_var_chr <- x@sp_uid_lup %>% procure(match_value_xx = ref_unit_chr,
                                           area_bndy_yr_chr = ref_bound_yr)
  tgt_var_chr <- x@sp_uid_lup %>% procure(match_value_xx = tgt_unit_chr,
                                           area_bndy_yr_chr = tgt_bound_yr)
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
                                                                                                                           ready4::get_from_lup_obj(ref_sf  %>% sf::st_set_geometry(NULL),
                                                                                                                                                 match_value_xx = .x,
                                                                                                                                                 match_var_nm_1L_chr = ref_var_chr,
                                                                                                                                                 target_var_nm_1L_chr = tgt_var_chr,
                                                                                                                                                 evaluate_1L_lgl = F))
    ))
}
