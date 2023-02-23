#' Procure items from a dataset
#' @description procure.vicinity_abbreviations() is a procure method that procures data by executing a search and retrieval algorithm using data contained in an instance of a class. This method is implemented for the ready4 S3 class for tibble object lookup table for spatial data abbreviations. The function returns Object (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for tibble object lookup table for spatial data abbreviations.
#' @param col_nm_1L_chr Column name (a character vector of length one), Default: 'short_name_chr'
#' @param match_value_xx Match value (an output object of multiple potential types)
#' @return Object (an output object of multiple potential types)
#' @rdname procure-methods
#' @export 
#' @importFrom ready4 get_from_lup_obj procure
procure.vicinity_abbreviations <- function (x, col_nm_1L_chr = "short_name_chr", match_value_xx) 
{
    object_xx <- ready4::get_from_lup_obj(data_lookup_tb = x, 
        match_value_xx = match_value_xx, match_var_nm_1L_chr = col_nm_1L_chr, 
        target_var_nm_1L_chr = ifelse(col_nm_1L_chr == "short_name_chr", 
            "long_name_chr", "short_name_chr"), evaluate_1L_lgl = FALSE)
    return(object_xx)
}
#' @rdname procure-methods
#' @aliases procure,vicinity_abbreviations-method
#' @importFrom ready4 procure
methods::setMethod("procure", methods::className("vicinity_abbreviations", package = "vicinity"), procure.vicinity_abbreviations)
#' Procure items from a dataset
#' @description procure.vicinity_identifiers() is a procure method that procures data by executing a search and retrieval algorithm using data contained in an instance of a class. This method is implemented for the ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects. The function returns Object (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @param col_nm_1L_chr Column name (a character vector of length one), Default: 'spatial_unit_chr'
#' @param geometry_rsl_1L_chr Geometry resolution (a character vector of length one), Default: character(0)
#' @param group_at_geom_unit_1L_lgl Group at geometry unit (a logical vector of length one), Default: TRUE
#' @param data_rsl_1L_chr Data resolution (a character vector of length one), Default: character(0)
#' @param match_value_xx Match value (an output object of multiple potential types), Default: NULL
#' @param area_bndy_yr_chr Area boundary year (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'match'
#' @return Object (an output object of multiple potential types)
#' @rdname procure-methods
#' @export 
#' @importFrom ready4 get_from_lup_obj procure
#' @importFrom dplyr filter
procure.vicinity_identifiers <- function (x, col_nm_1L_chr = "spatial_unit_chr", geometry_rsl_1L_chr = character(0), 
    group_at_geom_unit_1L_lgl = TRUE, data_rsl_1L_chr = character(0), 
    match_value_xx = NULL, area_bndy_yr_chr = character(0), what_1L_chr = "match") 
{
    if (what_1L_chr == "match") {
        object_xx <- ready4::get_from_lup_obj(data_lookup_tb = x %>% 
            dplyr::filter(year_chr == area_bndy_yr_chr), match_value_xx = match_value_xx, 
            match_var_nm_1L_chr = col_nm_1L_chr, target_var_nm_1L_chr = ifelse(col_nm_1L_chr == 
                "spatial_unit_chr", "var_name_chr", "spatial_unit_chr"), 
            evaluate_1L_lgl = FALSE)
    }
    if (what_1L_chr == "grouping") {
        object_xx <- ifelse(group_at_geom_unit_1L_lgl, ready4::get_from_lup_obj(data_lookup_tb = x %>% 
            dplyr::filter(spatial_unit_chr == geometry_rsl_1L_chr) %>% 
            dplyr::filter(as.numeric(year_chr) == as.numeric(area_bndy_yr_chr)), 
            match_var_nm_1L_chr = "spatial_unit_chr", match_value_xx = geometry_rsl_1L_chr, 
            target_var_nm_1L_chr = "var_name_chr", evaluate_1L_lgl = FALSE), 
            ready4::get_from_lup_obj(data_lookup_tb = x, match_var_nm_1L_chr = "spatial_unit_chr", 
                match_value_xx = data_rsl_1L_chr, target_var_nm_1L_chr = "var_name_chr", 
                evaluate_1L_lgl = FALSE))
    }
    return(object_xx)
}
#' @rdname procure-methods
#' @aliases procure,vicinity_identifiers-method
#' @importFrom ready4 procure
methods::setMethod("procure", methods::className("vicinity_identifiers", package = "vicinity"), procure.vicinity_identifiers)
#' Procure items from a dataset
#' @description procure.vicinity_raw() is a procure method that procures data by executing a search and retrieval algorithm using data contained in an instance of a class. This method is implemented for the ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import. The function returns Object (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param inc_script_1L_lgl Include script (a logical vector of length one), Default: T
#' @param forced_choice_chr Forced choice (a character vector), Default: 'NA'
#' @param match_value_xx Match value (an output object of multiple potential types), Default: NULL
#' @param what_1L_chr What (a character vector of length one), Default: 'source'
#' @return Object (an output object of multiple potential types)
#' @rdname procure-methods
#' @export 
#' @importFrom ready4use assert_single_row_tb
#' @importFrom purrr discard
#' @importFrom dplyr filter select pull
#' @importFrom ready4 procure
procure.vicinity_raw <- function (x, inc_script_1L_lgl = T, forced_choice_chr = NA_character_, 
    match_value_xx = NULL, what_1L_chr = "source") 
{
    if (what_1L_chr == "source") {
        ready4use::assert_single_row_tb(x)
        source_ls <- list(script_chr = x$path_to_make_script_chr, 
            local_chr = x$local_file_src_chr, repo_chr = x$data_repo_db_ui_chr, 
            source_url_chr = x$download_url_chr) %>% purrr::discard(is.na)
        if ("script_chr" %in% names(source_ls) & !inc_script_1L_lgl) 
            source_ls$script_chr <- NULL
        if (!is.na(forced_choice_chr)) {
            if (!forced_choice_chr %in% names(source_ls)) 
                stop("Forced choice option is not available from input lookup table")
            source_ls <- source_ls[names(source_ls) == forced_choice_chr]
        }
        object_xx <- source_ls[1]
    }
    if (what_1L_chr == "match") {
        object_xx <- x %>% dplyr::filter(data_type_chr == match_value_xx)
    }
    if (what_1L_chr == "match_names") {
        object_xx <- procure.vicinity_raw(x, match_value_xx = match_value_xx, 
            what_1L_chr = "match") %>% dplyr::select(name_chr) %>% 
            dplyr::pull()
    }
    return(object_xx)
}
#' @rdname procure-methods
#' @aliases procure,vicinity_raw-method
#' @importFrom ready4 procure
methods::setMethod("procure", methods::className("vicinity_raw", package = "vicinity"), procure.vicinity_raw)
#' Procure items from a dataset
#' @description procure.vicinity_resolutions() is a procure method that procures data by executing a search and retrieval algorithm using data contained in an instance of a class. This method is implemented for the ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects. The function returns Resolution (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param options_chr Options (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'resolution'
#' @param whole_area_1L_lgl Whole area (a logical vector of length one), Default: TRUE
#' @param year_1L_dbl Year (a double vector of length one), Default: numeric(0)
#' @return Resolution (an output object of multiple potential types)
#' @rdname procure-methods
#' @export 
#' @importFrom dplyr filter arrange desc pull
#' @importFrom ready4 procure
procure.vicinity_resolutions <- function (x, options_chr = character(0), what_1L_chr = "resolution", 
    whole_area_1L_lgl = TRUE, year_1L_dbl = numeric(0)) 
{
    if (what_1L_chr == "resolution") {
        if (!is.na(options_chr[1])) {
            resolutions_chr <- procure.vicinity_resolutions(x, 
                year_1L_dbl = as.numeric(year_1L_dbl), what_1L_chr = "hierarchy")
            resolution_1L_chr <- resolutions_chr[min(which(resolutions_chr %in% 
                options_chr))]
        }
        else {
            resolution_1L_chr <- NA_character_
        }
        resolution_xx <- resolution_1L_chr
    }
    if (what_1L_chr == "hierarchy") {
        resolution_hierarchy <- x %>% dplyr::filter(boundary_year_dbl == 
            year_1L_dbl)
        if (whole_area_1L_lgl) {
            resolution_hierarchy <- resolution_hierarchy %>% 
                dplyr::filter(complete_lgl == TRUE)
        }
        resolution_xx <- resolution_hierarchy %>% dplyr::arrange(dplyr::desc(area_count_dbl)) %>% 
            dplyr::pull(area_type_chr)
    }
    return(resolution_xx)
}
#' @rdname procure-methods
#' @aliases procure,vicinity_resolutions-method
#' @importFrom ready4 procure
methods::setMethod("procure", methods::className("vicinity_resolutions", package = "vicinity"), procure.vicinity_resolutions)
#' 
#' Procure items from a dataset
#' @name procure-VicinityProfile
#' @description procure method applied to VicinityProfile
#' @param x An object of class VicinityProfile
#' @param exclude_dif_bndy_yr_1L_lgl Exclude different boundary year (a logical vector of length one), Default: TRUE
#' @param highest_rsl_chr Highest resolution (a character vector), Default: character(0)
#' @param key_var_1L_chr Key variable (a character vector of length one), Default: character(0)
#' @param match_year_1L_lgl Match year (a logical vector of length one), Default: TRUE
#' @param travel_mode_1L_chr Travel mode (a character vector of length one), Default: character(0)
#' @param years_chr Years (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'grouping'
#' @return Object (an output object of multiple potential types)
#' @rdname procure-methods
#' @aliases procure,VicinityProfile-method
#' @export 
#' @importFrom purrr map map_chr map2 flatten_chr map2_chr map_dbl reduce
#' @importFrom dplyr filter pull
#' @importFrom ready4 get_from_lup_obj procure
methods::setMethod("procure", "VicinityProfile", function (x, exclude_dif_bndy_yr_1L_lgl = TRUE, highest_rsl_chr = character(0), 
    key_var_1L_chr = character(0), match_year_1L_lgl = TRUE, 
    travel_mode_1L_chr = character(0), years_chr = character(0), 
    what_1L_chr = "grouping") 
{
    if (what_1L_chr == "attribute_names") {
        spatial_lookup_tb <- x@a_VicinityLookup@vicinity_processed_r3
        lookup_tbl_ls <- purrr::map(highest_rsl_chr, ~spatial_lookup_tb %>% 
            dplyr::filter(main_feature_chr == .x) %>% dplyr::filter(year_chr %in% 
            years_chr[if (.x == key_var_1L_chr) 
                1:length(years_chr)
            else 1]))
        data_rsl_chr <- purrr::map_chr(lookup_tbl_ls, ~.x %>% 
            dplyr::pull(area_type_chr) %>% unique() %>% procure.vicinity_resolutions(x = x@a_VicinityLookup@vicinity_resolutions_r3, 
            year_1L_dbl = as.numeric(x@data_year_1L_chr)))
        data_unavail_for_year <- is.na(data_rsl_chr)
        if (match_year_1L_lgl & sum(data_unavail_for_year) > 
            0) 
            stop("Data not available for specified year for all data requested")
        matched_years_chr <- highest_rsl_chr[!data_unavail_for_year]
        matched_yr_lookup_tbl_ls <- lookup_tbl_ls[!data_unavail_for_year]
        matched_yr_data_rsl_chr <- data_rsl_chr[!data_unavail_for_year]
        non_matched_years_chr <- highest_rsl_chr[is.na(data_rsl_chr)]
        matched_yr_lookup_tbl_ls <- purrr::map2(matched_yr_lookup_tbl_ls, 
            matched_yr_data_rsl_chr, ~.x %>% dplyr::filter(area_type_chr == 
                .y))
        attribute_names_chr <- purrr::map(matched_yr_lookup_tbl_ls, 
            ~.x %>% dplyr::pull(name)) %>% purrr::flatten_chr()
        if (!identical(non_matched_years_chr, character(0))) {
            closest_yrs_ls <- manufacture.vicinity_processed(spatial_lookup_tb, 
                main_incld_feature_chr = non_matched_years_chr, 
                target_year_1L_chr = x@data_year_1L_chr, what_1L_chr = "closest year")
            extra_names <- purrr::map2_chr(non_matched_years_chr, 
                closest_yrs_ls, ~ready4::get_from_lup_obj(data_lookup_tb = spatial_lookup_tb %>% 
                  dplyr::filter(year_chr == .y), match_value_xx = .x, 
                  match_var_nm_1L_chr = "main_feature_chr", target_var_nm_1L_chr = "name_chr", 
                  evaluate_1L_lgl = FALSE))
            non_matched_positions <- purrr::map_dbl(non_matched_years_chr, 
                ~which(highest_rsl_chr == .x))
            attribute_names_chr <- purrr::reduce(1:length(non_matched_positions), 
                .init = attribute_names_chr, ~append(.x, extra_names[.y], 
                  after = non_matched_positions[.y] - 1))
        }
        object_xx <- attribute_names_chr
    }
    if (what_1L_chr == "grouping") {
        y_vicinity_identifiers = x@a_VicinityLookup@vicinity_identifiers_r3
        if (!x@use_coord_lup_lgl) {
            object_xx <- procure.vicinity_identifiers(y_vicinity_identifiers, 
                geometry_rsl_1L_chr = x@area_type_chr, area_bndy_yr_chr = as.character(x@area_bndy_yr_dbl))
        }
        if (what_1L_chr == "proximity") {
            if (is.na(x@geomc_dist_limit_km_dbl)) 
                object_xx <- paste0(travel_mode_1L_chr, "_times")
            else object_xx <- "distance_in_km_dbl"
        }
    }
    return(object_xx)
})
