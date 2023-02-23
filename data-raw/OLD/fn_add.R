#' Add attr list to
#' @description add_attr_list_to_sf() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add attr list to simple features object. Function argument x specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param lookup_tb_r4 Lookup (a ready4 S4 collection of tibbles)
#' @return NULL
#' @rdname add_attr_list_to_sf
#' @export 
#' @importFrom ready4fun get_from_lup
add_attr_list_to_sf <- function (x, y, lookup_tb_r4) 
{
    att_data_xx <- make_att_data_xx(lookup_tb_r4 = lookup_tb_r4, 
        match_value_xx = y, starter_sf = x)
    add_att_to_sf(area_sf = x, att_data_tb = att_data_xx, 
        att_data_desc = ready4::get_from_lup_obj(data_lookup_tb = sp_data_pack_lup(lookup_tb_r4), 
            match_value_xx = y, match_var_nm_1L_chr = "name", target_var_nm_1L_chr = "main_feature", 
            evaluate_1L_lgl = FALSE))
}
#' Add attr recrly to
#' @description add_attr_recrly_to_sf() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add attr recrly to simple features object. Function argument input_ls specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param input_ls Input (a list)
#' @param subdivision_1L_chr PARAM_DESCRIPTION, Default: NULL
#' @param area_unit_1L_chr PARAM_DESCRIPTION
#' @param boundary_year PARAM_DESCRIPTION
#' @param attribute_data PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_attr_recrly_to_sf
#' @export 
#' @importFrom dplyr filter
#' @importFrom purrr map reduce
#' @importFrom stats setNames
add_attr_recrly_to_sf <- function (input_ls, subdivision_1L_chr = NULL, area_unit_1L_chr, boundary_year, 
    attribute_data) 
{
    lookup_tb_r4 <- lookup_tb(input_ls$x_VicinityProfile)
    data_lookup_tb <- sp_data_pack_lup(lookup_tb_r4)
    boundary_file <- procure(data_lookup_tb %>% dplyr::filter(area_type == 
        area_unit_1L_chr) %>% dplyr::filter(main_feature == "Boundary") %>% 
        dplyr::filter(as.numeric(year_start) == max(as.numeric(year_start)[as.numeric(year_start) <= 
            as.numeric(boundary_year)])), match_value_xx = "Boundary")
    attribute_data_list <- purrr::map(attribute_data, ~.x) %>% 
        stats::setNames(attribute_data)
    purrr::map(attribute_data_list, ~add_attr_list_to_sf(x = boundary_file, 
        y = .x, lookup_tb_r4 = lookup_tb_r4)) %>% transform_sf_ls() %>% 
        purrr::reduce(~rbind(.x, .y))
}
#' Add attr tibble to data pack
#' @description add_attrs_to_processed_lup() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add attr tibble to data pack lookup table. Function argument data_pack_lup specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param data_pack_lup Data pack (a lookup table)
#' @param att_tb Attr (a tibble)
#' @param object_name PARAM_DESCRIPTION
#' @param area_type PARAM_DESCRIPTION
#' @param area_bound_yr PARAM_DESCRIPTION
#' @param region PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @param year_start PARAM_DESCRIPTION
#' @param year_end PARAM_DESCRIPTION
#' @param main_feature PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_attrs_to_processed_lup
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
add_attrs_to_processed_lup <- function (data_pack_lup, att_tb, object_name, area_type, area_bound_yr, 
    region, year, year_start, year_end, main_feature) 
{
    tibble::tibble(name = object_name, country = "Australia", 
        area_type = area_type, area_bound_yr = area_bound_yr, 
        region = region, data_type = "Attribute", main_feature = main_feature, 
        year = year, year_start = year_start, year_end = year_end, 
        source_reference = object_name) %>% dplyr::bind_rows(data_pack_lup, 
        .)
}
#' Add attr tibble to data pack lookup table from argument list
#' @description add_att_tb_to_processed_lup() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add attr tibble to data pack lookup table from argument list. Function argument x specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_att_tb_to_processed_lup
#' @export 

add_att_tb_to_processed_lup <- function (x, y) 
{
    add_attrs_to_processed_lup(data_pack_lup = x, att_tb = y[[1]], 
        object_name = y[[2]], area_type = y[[3]], area_bound_yr = y[[4]], 
        region = y[[5]], year = y[[6]], year_start = y[[7]], 
        year_end = y[[8]], main_feature = y[[9]])
}
#' Add attr to global
#' @description add_att_to_global() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add attr to global. Function argument combined_ste_ppr_ls specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param combined_ste_ppr_ls Combined ste ppr (a list)
#' @param object_name_stub PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_att_to_global
#' @export 
#' @importFrom purrr walk
#' @importFrom stringr str_sub
add_att_to_global <- function (combined_ste_ppr_ls, object_name_stub) 
{
    purrr::walk(names(combined_ste_ppr_ls), ~eval(parse(text = paste0(object_name_stub, 
        .x %>% stringr::str_sub(start = 2), "<<-combined_ste_ppr_ls$", 
        .x))))
}
#' Add attr to
#' @description add_att_to_sf() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add attr to simple features object. Function argument area_sf specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param area_sf Area (a simple features object)
#' @param att_data_tb Attr data (a tibble)
#' @param att_data_desc PARAM_DESCRIPTION
#' @return NA ()
#' @rdname add_att_to_sf
#' @export 
#' @importFrom dplyr inner_join
#' @importFrom stringr str_detect
#' @importFrom sf st_as_sf
add_att_to_sf <- function (area_sf, att_data_tb, att_data_desc) 
{
    if (att_data_desc == "PPR") {
        merged_units <- dplyr::inner_join(area_sf, att_data_tb)
    }
    if (stringr::str_detect(att_data_desc, "ERP_TOT")) {
        merged_units <- dplyr::inner_join(area_sf, att_data_tb) %>% 
            sf::st_as_sf()
    }
    if (att_data_desc == "ERP_ASX") {
        merged_units <- dplyr::inner_join(area_sf, att_data_tb) %>% 
            sf::st_as_sf()
    }
    return(merged_units)
}
#' Add attribute to data pack
#' @description add_attribute_to_data_pack() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add attribute to data pack. Function argument combined_ste_ppr_ls specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param combined_ste_ppr_ls Combined ste ppr (a list)
#' @param object_name_stub PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_attribute_to_data_pack
#' @export 
#' @importFrom purrr walk
#' @importFrom stringr str_sub
add_attribute_to_data_pack <- function (combined_ste_ppr_ls, object_name_stub) 
{
    add_att_to_global(combined_ste_ppr_ls = combined_ste_ppr_ls, 
        object_name_stub = object_name_stub)
    purrr::walk(names(combined_ste_ppr_ls), ~eval(parse(text = paste0("usethis::use_data(", 
        object_name_stub, .x %>% stringr::str_sub(start = 2), 
        ", overwrite = TRUE)"))))
}
#' Add attribute to data pack from
#' @description add_attribute_to_data_pack_from_tb() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add attribute to data pack from tibble. Function argument att_tb specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param att_tb Attr (a tibble)
#' @param object_name PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_attribute_to_data_pack_from_tb
#' @export 

add_attribute_to_data_pack_from_tb <- function (att_tb, object_name) 
{
    eval(parse(text = paste0(object_name, "<<-att_tb")))
    eval(parse(text = paste0("usethis::use_data(", object_name, 
        ", overwrite = TRUE)")))
}
#' Add data pack
#' @description add_data_pack_lup() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add data pack lookup table. Function argument lookup_tbs_r4 specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param lookup_tbs_r4 Lookup tibbles (a ready4 S4)
#' @param tbl_data_type_1L_chr PARAM_DESCRIPTION, Default: 'Geometry'
#' @param template_ls Template (a list), Default: NULL
#' @param package_1L_chr PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_data_pack_lup
#' @export 
#' @importFrom purrr map2 reduce map2_chr
#' @importFrom ready4fun get_from_lup
#' @importFrom dplyr mutate
add_data_pack_lup <- function (lookup_tbs_r4, tbl_data_type_1L_chr = "Geometry", template_ls = NULL, 
    package_1L_chr) 
{
    data_pk_lup_arguments_ls <- purrr::map2(template_ls, names(template_ls), 
        ~list(.x, .y, ready4::get_from_lup_obj(data_lookup_tb = lookup_tbs_r4 %>% 
            sp_import_lup(), target_var_nm_1L_chr = "area_type", match_var_nm_1L_chr = "name", 
            match_value_xx = .y, evaluate_1L_lgl = FALSE), ready4::get_from_lup_obj(data_lookup_tb = lookup_tbs_r4 %>% 
            sp_import_lup(), target_var_nm_1L_chr = "area_bound_yr", 
            match_var_nm_1L_chr = "name", match_value_xx = .y, 
            evaluate_1L_lgl = FALSE), ready4::get_from_lup_obj(data_lookup_tb = lookup_tbs_r4 %>% 
            sp_import_lup(), target_var_nm_1L_chr = "region", match_var_nm_1L_chr = "name", 
            match_value_xx = .y, evaluate_1L_lgl = FALSE), ready4::get_from_lup_obj(data_lookup_tb = lookup_tbs_r4 %>% 
            sp_import_lup(), target_var_nm_1L_chr = "year", match_var_nm_1L_chr = "name", 
            match_value_xx = .y, evaluate_1L_lgl = FALSE), ready4::get_from_lup_obj(data_lookup_tb = lookup_tbs_r4 %>% 
            sp_import_lup(), target_var_nm_1L_chr = "year_start", 
            match_var_nm_1L_chr = "name", match_value_xx = .y, 
            evaluate_1L_lgl = FALSE), ready4::get_from_lup_obj(data_lookup_tb = lookup_tbs_r4 %>% 
            sp_import_lup(), target_var_nm_1L_chr = "year_end", match_var_nm_1L_chr = "name", 
            match_value_xx = .y, evaluate_1L_lgl = FALSE), ready4::get_from_lup_obj(data_lookup_tb = lookup_tbs_r4 %>% 
            sp_import_lup(), target_var_nm_1L_chr = "main_feature", 
            match_var_nm_1L_chr = "name", match_value_xx = .y, 
            evaluate_1L_lgl = FALSE)))
    data_pack_lup_r3 <- purrr::reduce(data_pk_lup_arguments_ls, 
        .init = lookup_tbs_r4 %>% sp_data_pack_lup(), ~add_att_tb_to_processed_lup(.x, 
            .y)) %>% dplyr::mutate(data_type = tbl_data_type_1L_chr)
    package_1L_chr <- ifelse(package_1L_chr == "" | is.na(package_1L_chr), "", 
        paste0(package_1L_chr, "::"))
    data_pack_lup_r3 <- data_pack_lup_r3 %>% dplyr::mutate(source_reference = paste0(package_1L_chr, 
        source_reference)) %>% dplyr::mutate(source_reference = purrr::map2_chr(main_feature, 
        source_reference, ~ifelse(.x == "Boundary", paste0(.y, 
            "_sf"), .y)))
    lookup_tbs_r4 <- `sp_data_pack_lup<-`(lookup_tbs_r4, data_pack_lup_r3)
    lookup_tbs_r4
}
#' Add dynamic sp vars to
#' @description add_dynamic_vars_to_sf() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add dynamic sp vars to simple features object. Function argument dynamic_vars_sf specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param dynamic_vars_sf Dynamic sp vars (a simple features object)
#' @param profiled_sf Pop attr (a simple features object)
#' @param dynamic_var_rsl_1L_chr PARAM_DESCRIPTION
#' @param dynamic_var_nm_1L_chr PARAM_DESCRIPTION
#' @param featured_var_pfx_1L_chr PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param crs_nbr_dbl PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_dynamic_vars_to_sf
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
add_dynamic_vars_to_sf <- function (dynamic_vars_sf, profiled_sf, dynamic_var_rsl_1L_chr, 
    dynamic_var_nm_1L_chr, featured_var_pfx_1L_chr, data_year, crs_nbr_dbl) 
{
    profiled_sf <- make_intersecting_profiled_area(profiled_sf = dynamic_vars_sf, 
        profiled_sf_col_1L_chr = NA, profiled_sf_row_1L_chr = NA, attribute_sf = profiled_sf, 
        attribute_rsl_1L_chr = dynamic_var_rsl_1L_chr, data_type = "processed_age_sex", 
        data_year = data_year, featured_var_pfx_1L_chr = featured_var_pfx_1L_chr, 
        crs_nbr_dbl = crs_nbr_dbl) %>% add_km_sqd_by_group(group_by_var_1L_chr = dynamic_var_nm_1L_chr, 
        feature_nm_1L_chr = dynamic_var_rsl_1L_chr)
    dyn_param_unit_id <- names(dynamic_vars_sf)[1]
    profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(dynamic_var_nm_1L_chr), 
        paste0(!!rlang::sym(dyn_param_unit_id), "_", !!rlang::sym(dynamic_var_nm_1L_chr))))
    update_popl_counts(profiled_sf = profiled_sf, group_by_var_1L_chr = group_by_var_1L_chr, 
        dynamic_var_nm_1L_chr = dynamic_var_nm_1L_chr, data_year = data_year, 
        dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr, reference_var_rsl_1L_chr = NULL, 
        featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
}
#' Add kmsq area all features
#' @description add_km_sqd() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add kmsq area all features. Function argument sf specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param sf Simple features object (a simple features object)
#' @param feature_nm_1L_chr PARAM_DESCRIPTION
#' @param prefix PARAM_DESCRIPTION, Default: 'whl_'
#' @param suffix PARAM_DESCRIPTION, Default: '_area'
#' @return NULL
#' @rdname add_km_sqd
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom sf st_area
#' @importFrom units set_units
add_km_sqd <- function (sf, feature_nm_1L_chr, prefix = "whl_", suffix = "_area") 
{
    sf %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(prefix, feature_nm_1L_chr, 
        suffix)), sf::st_area(.) %>% units::set_units(km^2)))
}
#' Add kmsq area by group
#' @description add_km_sqd_by_group() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add kmsq area by group. Function argument sf specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param sf Simple features object (a simple features object)
#' @param group_by_var_1L_chr PARAM_DESCRIPTION
#' @param feature_nm_1L_chr PARAM_DESCRIPTION
#' @param prefix PARAM_DESCRIPTION, Default: 'whl_'
#' @param suffix PARAM_DESCRIPTION, Default: '_area'
#' @return NULL
#' @rdname add_km_sqd_by_group
#' @export 
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom rlang sym
#' @importFrom sf st_combine st_set_geometry
add_km_sqd_by_group <- function (sf, group_by_var_1L_chr, feature_nm_1L_chr, prefix = "whl_", suffix = "_area") 
{
    merge(sf, sf %>% dplyr::group_by(!!rlang::sym(group_by_var_1L_chr)) %>% 
        dplyr::summarise(geometry = sf::st_combine(geometry)) %>% 
        add_km_sqd(feature_nm_1L_chr = feature_nm_1L_chr, prefix = prefix, 
            suffix = suffix) %>% dplyr::ungroup() %>% sf::st_set_geometry(NULL))
}
#' Add names
#' @description add_names() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add names. Function argument x specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_names
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr pmap_chr
#' @importFrom ready4fun get_from_lup
#' @importFrom stringr str_sub
add_names <- function (x) 
{
    data(ISO_3166_1, package = "ISOcodes", envir = environment())
    x <- x %>% dplyr::mutate(name = purrr::pmap_chr(list(country, 
        area_type, region, data_type, main_feature, year), ~paste0(ready4::get_from_lup_obj(data_lookup_tb = ISO_3166_1, 
        match_value_xx = ..1, match_var_nm_1L_chr = "Name", target_var_nm_1L_chr = "Alpha_3", 
        evaluate_1L_lgl = FALSE) %>% tolower(), "_", tolower(..2), "_", 
        tolower(..3 %>% stringr::str_sub(end = 3)), ifelse(..4 == 
            "Geometry", ifelse(..5 == "Boundary", "_bnd_", "_crd_"), 
            paste0("_", tolower(..5), "_")), ..6)))
    x %>% dplyr::mutate(name = make.unique(name)) %>% dplyr::mutate(name = map_chr(name, 
        ~ifelse(stringr::str_sub(.x, start = -2, end = -2) == 
            ".", paste0(stringr::str_sub(.x, end = 11), stringr::str_sub(.x, 
            start = -1), stringr::str_sub(.x, start = 12, end = -3)), 
            .x)))
}
#' Add ppr list to data pack
#' @description add_popl_predn_ls() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add ppr list to data pack lookup table. Function argument x specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_popl_predn_ls
#' @export 

add_popl_predn_ls <- function (x, y) 
{
    add_popl_predn(data_pack_lup = x, combined_ste_ppr_ls = y[[1]], 
        object_name_stub = y[[2]], area_type = y[[3]], area_bound_yr = y[[4]], 
        region = y[[5]])
}
#' Add ppr to data pack
#' @description add_popl_predn() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add ppr to data pack lookup table. Function argument data_pack_lup specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param data_pack_lup Data pack (a lookup table)
#' @param combined_ste_ppr_ls Combined ste ppr (a list)
#' @param object_name_stub PARAM_DESCRIPTION
#' @param area_type PARAM_DESCRIPTION
#' @param area_bound_yr PARAM_DESCRIPTION
#' @param region PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_popl_predn
#' @export 
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @importFrom dplyr bind_rows
add_popl_predn <- function (data_pack_lup, combined_ste_ppr_ls, object_name_stub, 
    area_type, area_bound_yr, region) 
{
    tibble::tibble(name = paste0(object_name_stub, names(combined_ste_ppr_ls) %>% 
        stringr::str_sub(start = 2)), country = "Australia", 
        area_type = area_type, area_bound_yr = area_bound_yr, 
        region = region, data_type = "Attribute", main_feature = "Population projections", 
        year = names(combined_ste_ppr_ls) %>% stringr::str_sub(start = 2), 
        source_reference = paste0(object_name_stub, names(combined_ste_ppr_ls) %>% 
            stringr::str_sub(start = 2))) %>% dplyr::bind_rows(data_pack_lup, 
        .)
}
#' Add sp resolution
#' @description add_resolutions_lup() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add sp resolution. Function argument lookup_tbs_r4 specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param lookup_tbs_r4 Lookup tibbles (a ready4 S4)
#' @param processed_fls_dir_1L_chr PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_resolutions_lup
#' @export 
#' @importFrom dplyr filter select mutate pull arrange
#' @importFrom stringr str_which
#' @importFrom purrr pmap_dfr
#' @importFrom tibble tibble
add_resolutions_lup <- function (lookup_tbs_r4, processed_fls_dir_1L_chr) 
{
    dr_dp_tb <- sp_data_pack_lup(lookup_tbs_r4) %>% dplyr::filter(main_feature == 
        "Boundary") %>% dplyr::select(area_type, country, region, 
        source_reference, year) %>% dplyr::mutate(source_reference = paste0(processed_fls_dir_1L_chr, 
        "/", source_reference, ".RDS"))
    dr_dp_vec <- dr_dp_tb %>% dplyr::pull(source_reference)
    dr_nt_vec <- dr_dp_tb %>% dplyr::pull(region)
    if (any(dr_nt_vec == "National")) {
        nat_sf <- readRDS(dr_dp_vec[stringr::str_which(dr_nt_vec, 
            "National") %>% min()])
        nat_area <- nat_sf %>% make_km_sqd_dbl()
    }
    else {
        nat_area <- NA_real_
    }
    resolution_lup_r3 <- purrr::pmap_dfr(dr_dp_tb, ~tibble::tibble(parent_area = ..2, 
        boundary_year = as.numeric(..5), area_type = ..1, area_count = nrow(readRDS(..4)) %>% 
            as.double(), complete = T, summed_area = ifelse(..3 == 
            "National", nat_area, readRDS(..4) %>% make_km_sqd_dbl()), 
        mean_size = summed_area/area_count))
    resolution_lup_r3 <- resolution_lup_r3 %>% vicinity_resolutions() %>% 
        dplyr::arrange(mean_size)
    `sp_resolution_lup<-`(lookup_tbs_r4, resolution_lup_r3)
}
#' Add starter simple features object to lups
#' @description add_templates() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add starter simple features object to lups. Function argument lookup_tbs_r4 specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param lookup_tbs_r4 Lookup tibbles (a ready4 S4)
#' @param path_to_seed_sf_1L_chr Path to starter simple features object (a character vector)
#' @return NULL
#' @rdname add_templates
#' @export 
#' @importFrom tibble add_row
#' @importFrom dplyr pull
add_templates <- function (lookup_tbs_r4, path_to_seed_sf_1L_chr) 
{
    starter_sf_name <- get_name_from_path_chr(path_to_seed_sf_1L_chr, 
        with_ext_1L_lgl = F)
    starter_sf_lup_r3 <- tibble::add_row(sp_starter_sf_lup(lookup_tbs_r4), 
        country = sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(country), 
        area_type = sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_type), 
        area_bound_yr = sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_bound_yr), 
        starter_sf = starter_sf_name, sf_main_sub_div = sp_import_lup(lookup_tbs_r4) %>% 
            dplyr::pull(uid))
    `sp_starter_sf_lup<-`(lookup_tbs_r4, starter_sf_lup_r3)
}
#' Add uid
#' @description add_uid_lup() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add uid lookup table. Function argument lookup_tbs_r4 specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param lookup_tbs_r4 Lookup tibbles (a ready4 S4)
#' @return NULL
#' @rdname add_uid_lup
#' @export 
#' @importFrom tibble add_row
#' @importFrom dplyr pull
add_uid_lup <- function (lookup_tbs_r4) 
{
    uid_lup_r3 <- tibble::add_row(vicinity_identifiers(), spatial_unit = sp_import_lup(lookup_tbs_r4) %>% 
        dplyr::pull(area_type), year = sp_import_lup(lookup_tbs_r4) %>% 
        dplyr::pull(area_bound_yr), var_name = sp_import_lup(lookup_tbs_r4) %>% 
        dplyr::pull(uid))
    `sp_uid_lup<-`(lookup_tbs_r4, uid_lup_r3)
}
