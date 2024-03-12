#' Manufacture a new object
#' @description manufacture.vicinity_abbreviations() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 submodule class for tibble object lookup table for spatial data abbreviations. The function returns Object (an output object of multiple potential types).
#' @param x An instance of `vicinity_abbreviations`, a ready4 submodule class for tibble object lookup table for spatial data abbreviations.
#' @param agent_areas_tb Agent areas (a tibble), Default: NULL
#' @param area_names_chr Area names (a character vector), Default: character(0)
#' @param area_var_nm_1L_chr Area variable name (a character vector of length one), Default: 'Suburb'
#' @param areas_sf Areas (a simple features object), Default: NULL
#' @param correspondences_lup Correspondences (a lookup table), Default: NULL
#' @param large_area_var_nm_1L_chr Large area variable name (a character vector of length one), Default: character(0)
#' @param large_areas_chr Large areas (a character vector), Default: character(0)
#' @param large_area_idx_1L_int Large area index (an integer vector of length one), Default: 1
#' @param match_value_xx Match value (an output object of multiple potential types), Default: NULL
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: character(0)
#' @param outliers_chr Outliers (a character vector), Default: character(0)
#' @param small_area_var_nm_1L_chr Small area variable name (a character vector of length one), Default: character(0)
#' @param title_case_1L_lgl Title case (a logical vector of length one), Default: T
#' @param type_1L_chr Type (a character vector of length one), Default: 'suffix'
#' @param unknown_area_val_1L_chr Unknown area value (a character vector of length one), Default: 'Unk'
#' @param valid_names_chr Valid names (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'areas'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @export 
#' @importFrom dplyr filter pull rowwise mutate group_by summarise rename inner_join
#' @importFrom rlang sym
#' @importFrom stringr str_sub str_length str_detect str_sort str_replace str_to_title
#' @importFrom purrr map
#' @importFrom ready4 manufacture
manufacture.vicinity_abbreviations <- function (x, agent_areas_tb = NULL, area_names_chr = character(0), 
    area_var_nm_1L_chr = "Suburb", areas_sf = NULL, correspondences_lup = NULL, 
    large_area_var_nm_1L_chr = character(0), large_areas_chr = character(0), 
    large_area_idx_1L_int = 1L, match_value_xx = NULL, match_var_nm_1L_chr = character(0), 
    outliers_chr = character(0), small_area_var_nm_1L_chr = character(0), 
    title_case_1L_lgl = T, type_1L_chr = "suffix", unknown_area_val_1L_chr = "Unk", 
    valid_names_chr = character(0), what_1L_chr = "areas") 
{
    if (what_1L_chr == "areas") {
        if (nrow(x) > 0) {
            suffix_1L_chr <- x %>% dplyr::filter(long_nm_chr == 
                !!(match_value_xx)) %>% dplyr::pull(short_nm_chr)
            if (type_1L_chr == "geometry") {
                agent_areas_tb <- transform_agent_areas(agent_areas_tb, 
                  area_var_nm_1L_chr = area_var_nm_1L_chr, match_var_nm_1L_chr = match_var_nm_1L_chr, 
                  match_value_xx = match_value_xx, title_case_1L_lgl = title_case_1L_lgl)
                included_areas_chr <- get_agent_areas(agent_areas_tb, 
                  area_var_nm_1L_chr = area_var_nm_1L_chr, type_1L_chr = "names")
                all_areas_included_regions_chr <- areas_sf %>% 
                  dplyr::filter(!!rlang::sym(large_area_var_nm_1L_chr) %in% 
                    large_areas_chr) %>% dplyr::pull(!!rlang::sym(small_area_var_nm_1L_chr)) %>% 
                  as.character()
                unmatched_areas_chr <- setdiff(included_areas_chr, 
                  all_areas_included_regions_chr)
                matched_as_raw_chr <- included_areas_chr[!included_areas_chr %in% 
                  unmatched_areas_chr]
                with_sfx_chr <- manufacture.vicinity_abbreviations(x, 
                  area_names_chr = unmatched_areas_chr, match_value_xx = large_areas_chr[large_area_idx_1L_int], 
                  type_1L_chr = "suffix", what_1L_chr = "areas")
                unmatched_areas_with_sfx_chr <- setdiff(with_sfx_chr, 
                  all_areas_included_regions_chr)
                matched_with_sfx_chr <- setdiff(with_sfx_chr, 
                  unmatched_areas_with_sfx_chr)
                suffix_length_1L_int <- get_from_lup_obj(x, match_value_xx = large_areas_chr[large_area_idx_1L_int], 
                  match_var_nm_1L_chr = "long_name_chr", target_var_nm_1L_chr = "long_name_chr") %>% 
                  nchar() + 3
                unmatched_areas_raw_chr <- unmatched_areas_with_sfx_chr %>% 
                  stringr::str_sub(1, stringr::str_length(.) - 
                    suffix_length_1L_int)
                manual_changes_chr <- purrr::map(unmatched_areas_raw_chr, 
                  ~rename_areas(.x, correspondences_lup = correspondences_lup)) %>% 
                  unlist()
                matched_manual_changes_chr <- manual_changes_chr[!stringr::str_detect(manual_changes_chr, 
                  setdiff(manual_changes_chr, areas_sf %>% dplyr::pull(SSC_NAME16) %>% 
                    as.character()))]
                updated_included_areas_chr <- c(matched_as_raw_chr, 
                  matched_with_sfx_chr, matched_manual_changes_chr) %>% 
                  stringr::str_sort()
                updated_client_locations <- agent_areas_tb %>% 
                  dplyr::rowwise() %>% dplyr::mutate(`:=`(!!rlang::sym(area_var_nm_1L_chr), 
                  manufacture.vicinity_abbreviations(x, area_names_chr = !!rlang::sym(area_var_nm_1L_chr), 
                    correspondences_lup = correspondences_lup, 
                    match_value_xx = large_areas_chr[large_area_idx_1L_int], 
                    type_1L_chr = "transformation", valid_names_chr = updated_included_areas_chr, 
                    what_1L_chr = "areas")))
                area_summary_tb <- updated_client_locations %>% 
                  dplyr::group_by(!!rlang::sym(area_var_nm_1L_chr)) %>% 
                  dplyr::summarise(clients = n())
                unknown_area_1L_chr <- manufacture.vicinity_abbreviations(x, 
                  area_names_chr = unknown_area_val_1L_chr, match_value_xx = large_areas_chr[large_area_idx_1L_int], 
                  type_1L_chr = "suffix", what_1L_chr = "areas")
                known_area_summary_tb <- area_summary_tb %>% 
                  dplyr::filter(!!rlang::sym(area_var_nm_1L_chr) != 
                    unknown_area_1L_chr)
                known_area_summary_tb <- known_area_summary_tb %>% 
                  dplyr::rename(`:=`(!!rlang::sym(small_area_var_nm_1L_chr), 
                    !!rlang::sym(area_var_nm_1L_chr)))
                areas_sf <- dplyr::inner_join(areas_sf, known_area_summary_tb)
                if (!identical(outliers_chr, character(0))) {
                  areas_sf <- remove_outlier_areas(areas_sf, 
                    outliers_chr = outliers_chr, area_var_nm_1L_chr = small_area_var_nm_1L_chr)
                }
                object_xx <- areas_sf
            }
            if (type_1L_chr == "suffix") {
                object_xx <- paste0(area_names_chr, paste0(" (", 
                  suffix_1L_chr, ")"))
            }
            if (type_1L_chr == "main") {
                object_xx <- stringr::str_replace(area_names_chr, 
                  paste0(" (", suffix_1L_chr, ")"), "")
            }
            if (type_1L_chr == "transformation") {
                if (title_case_1L_lgl) {
                  area_names_chr <- stringr::str_to_title(area_names_chr)
                }
                if (!is.null(correspondences_lup)) {
                  area_names_chr <- rename_areas(area_names_chr, 
                    correspondences_lup = correspondences_lup)
                }
                if (any(!area_names_chr %in% valid_names_chr)) {
                  area_names_chr <- manufacture.vicinity_abbreviations(x, 
                    area_names_chr = area_names_chr, match_value_xx = match_value_xx, 
                    type_1L_chr = "suffix", what_1L_chr = "areas")
                }
                object_xx <- area_names_chr
            }
        }
        else {
            object_xx <- area_names_chr
        }
    }
    return(object_xx)
}
#' @rdname manufacture-methods
#' @aliases manufacture,vicinity_abbreviations-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("vicinity_abbreviations", package = "vicinity"), manufacture.vicinity_abbreviations)
#' Manufacture a new object
#' @description manufacture.vicinity_parameters() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 submodule class for tibble object that stores simulation structural parameters relating to the spatial environment. The function returns Object (an output object of multiple potential types).
#' @param x An instance of `vicinity_parameters`, a ready4 submodule class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param y_vicinity_mapes PARAM_DESCRIPTION, Default: NULL
#' @param n_its_int N its (an integer vector), Default: integer(0)
#' @param joint_dstr_1L_lgl Joint distribution (a logical vector of length one), Default: T
#' @param what_1L_chr What (a character vector of length one), Default: 'values'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @export 
#' @importFrom dplyr bind_rows
#' @importFrom ready4 manufacture
manufacture.vicinity_parameters <- function (x, y_vicinity_mapes = NULL, n_its_int = integer(0), 
    joint_dstr_1L_lgl = T, what_1L_chr = "values") 
{
    if (what_1L_chr == "values") {
        param_vals_tb <- reckon(x, n_its_int = n_its_int)
        if (!is.null(y_vicinity_mapes)) {
            mape_vals_tb <- reckon(y_vicinity_mapes, n_its_int = n_its_int, 
                joint_dstr_1L_lgl = joint_dstr_1L_lgl)
            param_vals_tb <- dplyr::bind_rows(param_vals_tb, 
                mape_vals_tb)
        }
        object_xx <- param_vals_tb
    }
    return(object_xx)
}
#' @rdname manufacture-methods
#' @aliases manufacture,vicinity_parameters-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("vicinity_parameters", package = "vicinity"), manufacture.vicinity_parameters)
#' Manufacture a new object
#' @description manufacture.vicinity_points() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 submodule class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes. The function returns Object (an output object of multiple potential types).
#' @param x An instance of `vicinity_points`, a ready4 submodule class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param bands_1L_dbl Bands (a double vector of length one), Default: numeric(0)
#' @param crs_nbr_dbl Coordinates reference system number (a double vector), Default: numeric(0)
#' @param land_sf Land (a simple features object), Default: NULL
#' @param metres_1L_dbl Metres (a double vector of length one), Default: numeric(0)
#' @param service_1L_chr Service (a character vector of length one), Default: character(0)
#' @param time_min_1L_dbl Time minimum (a double vector of length one), Default: numeric(0)
#' @param time_max_1L_dbl Time maximum (a double vector of length one), Default: numeric(0)
#' @param time_steps_1L_dbl Time steps (a double vector of length one), Default: numeric(0)
#' @param travel_mode_1L_chr Travel mode (a character vector of length one), Default: 'car'
#' @param type_1L_chr Type (a character vector of length one), Default: 'single'
#' @param what_1L_chr What (a character vector of length one), Default: 'geometric'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @export 
#' @importFrom dplyr filter select pull arrange
#' @importFrom sf st_as_sf st_transform st_buffer st_union st_intersection st_sf
#' @importFrom purrr map
#' @importFrom stats setNames
#' @importFrom ready4 manufacture
manufacture.vicinity_points <- function (x, bands_1L_dbl = numeric(0), crs_nbr_dbl = numeric(0), 
    land_sf = NULL, metres_1L_dbl = numeric(0), service_1L_chr = character(0), 
    time_min_1L_dbl = numeric(0), time_max_1L_dbl = numeric(0), 
    time_steps_1L_dbl = numeric(0), travel_mode_1L_chr = "car", 
    type_1L_chr = "single", what_1L_chr = "geometric") 
{
    if (what_1L_chr == "isochrones") {
        if (type_1L_chr == "single") {
            one_service_tb <- x %>% dplyr::filter(service_name_chr == 
                service_1L_chr)
            one_service_sf <- make_isochrones(lat_1L_dbl = one_service_tb %>% 
                dplyr::select(lat_dbl) %>% dplyr::pull(), lng_1L_dbl = one_service_tb %>% 
                dplyr::select(lng_dbl) %>% dplyr::pull(), time_min_1L_dbl = time_min_1L_dbl, 
                time_max_1L_dbl = time_max_1L_dbl, time_steps_1L_dbl = time_steps_1L_dbl, 
                travel_mode_1L_chr = travel_mode_1L_chr)
            object_xx <- one_service_sf
        }
    }
    if (what_1L_chr == "geometric") {
        if (type_1L_chr == "single") {
            distance_from_pts_sf <- sf::st_as_sf(x, coords = c("lng_dbl", 
                "lat_dbl"), crs = crs_nbr_dbl[1]) %>% sf::st_transform(crs_nbr_dbl[2])
            distance_from_pts_on_land_sf <- sf::st_buffer(distance_from_pts_sf, 
                dist = metres_1L_dbl) %>% sf::st_union() %>% 
                sf::st_intersection(land_sf %>% sf::st_transform(crs_nbr_dbl[2])) %>% 
                sf::st_transform(crs_nbr_dbl[1]) %>% sf::st_sf()
            object_xx <- distance_from_pts_on_land_sf
        }
        if (type_1L_chr == "bands") {
            distance_in_km_1L_dbl <- metres_1L_dbl * 1000
            distances_dbl <- seq(from = distance_in_km_1L_dbl/bands_1L_dbl, 
                to = distance_in_km_1L_dbl, by = distance_in_km_1L_dbl/bands_1L_dbl)
            service_clusters_chr <- x %>% dplyr::pull(cluster_name_chr) %>% 
                unique()
            service_vicinity_points_ls <- purrr::map(service_clusters_chr, 
                ~x %>% dplyr::filter(cluster_name_chr == .x)) %>% 
                stats::setNames(service_clusters_chr)
            service_clusters_by_distance_ls <- purrr::map(distances_dbl, 
                ~make_cluster_bndys(clusters_chr = service_clusters_chr, 
                  crs_nbr_dbl = crs_nbr_dbl, distance_in_km_1L_dbl = .x, 
                  land_boundary_sf = land_sf, vicinity_points_ls = service_vicinity_points_ls)) %>% 
                stats::setNames(., paste0("km_", distances_dbl, 
                  "from_service"))
            geometric_distance_by_cluster_circles <- purrr::map(1:length(service_clusters_chr), 
                ~reorder_clusters_by_distances(clusters_by_distance_ls = service_clusters_by_distance_ls, 
                  distances_dbl = distances_dbl, index_val_1L_int = .x)) %>% 
                stats::setNames(., service_vicinity_points_ls %>% 
                  names())
            geometric_distance_by_cluster_bands <- purrr::map(geometric_distance_by_cluster_circles, 
                ~transform_circles_to_bands(geomc_dist_circles_ls = .x)) %>% 
                stats::setNames(., service_vicinity_points_ls %>% 
                  names())
            geometric_distance_by_cluster_circles_merged_list <- purrr::map(geometric_distance_by_cluster_circles, 
                ~do.call(rbind, .x)) %>% stats::setNames(., service_vicinity_points_ls %>% 
                names()) %>% purrr::map(., ~.x %>% dplyr::arrange(desc(distance_in_km_dbl)))
            geometric_distance_by_cluster_bands_merged_list <- purrr::map(geometric_distance_by_cluster_bands, 
                ~do.call(rbind, .x)) %>% stats::setNames(., service_vicinity_points_ls %>% 
                names()) %>% purrr::map(., ~.x %>% dplyr::arrange(desc(distance_in_km_dbl)) %>% 
                transform_to_simpler_sf(crs_dbl = crs_nbr_dbl[1]))
            object_xx <- geometric_distance_by_cluster_bands_merged_list
        }
    }
    return(object_xx)
}
#' @rdname manufacture-methods
#' @aliases manufacture,vicinity_points-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("vicinity_points", package = "vicinity"), manufacture.vicinity_points)
#' Manufacture a new object
#' @description manufacture.vicinity_processed() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data). The function returns Object (an output object of multiple potential types).
#' @param x An instance of `vicinity_processed`, a ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param approximation_1L_chr Approximation (a character vector of length one), Default: 'abs'
#' @param main_incld_feature_chr Main included feature (a character vector), Default: character(0)
#' @param target_area_1L_chr Target area (a character vector of length one), Default: character(0)
#' @param target_year_1L_chr Target year (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'closest years'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @export 
#' @importFrom ready4 manufacture
manufacture.vicinity_processed <- function (x, approximation_1L_chr = "abs", main_incld_feature_chr = character(0), 
    target_area_1L_chr = character(0), target_year_1L_chr = character(0), 
    what_1L_chr = "closest years") 
{
    if (what_1L_chr == "closest years") {
        closest_yrs_ls <- make_closest_yrs_ls(x, main_incld_feature_chr = main_incld_feature_chr, 
            target_area_1L_chr = target_area_1L_chr, target_year_1L_chr = target_year_1L_chr, 
            approximation_1L_chr = approximation_1L_chr)
        object_xx <- closest_yrs_ls
    }
    return(object_xx)
}
#' @rdname manufacture-methods
#' @aliases manufacture,vicinity_processed-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("vicinity_processed", package = "vicinity"), manufacture.vicinity_processed)
#' Manufacture a new object
#' @description manufacture.vicinity_raw() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import. The function returns Object (an output object of multiple potential types).
#' @param x An instance of `vicinity_raw`, a ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param args_ls Arguments (a list), Default: NULL
#' @param crs_nbr_dbl Coordinates reference system number (a double vector), Default: NA
#' @param fn Function (a function), Default: function(x, ...) {
#'    NULL
#'}
#' @param match_value_xx Match value (an output object of multiple potential types), Default: NULL
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: character(0)
#' @param merge_itms_chr Merge items (a character vector), Default: character(0)
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: F
#' @param package_1L_chr Package (a character vector of length one), Default: character(0)
#' @param processed_fls_dir_1L_chr Processed files directory (a character vector of length one), Default: character(0)
#' @param raw_fls_dir_1L_chr Raw files directory (a character vector of length one), Default: character(0)
#' @param sub_dirs_chr Sub directories (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'output'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @export 
#' @importFrom rlang exec
#' @importFrom ready4use assert_single_row_tb get_valid_path_chr manufacture.ready4use_dataverses ready4use_dataverses
#' @importFrom purrr map_chr accumulate
#' @importFrom ready4 get_from_lup_obj manufacture
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
manufacture.vicinity_raw <- function (x, args_ls = NULL, crs_nbr_dbl = NA_real_, fn = function(x, 
    ...) {
    NULL
}, match_value_xx = NULL, match_var_nm_1L_chr = character(0), 
    merge_itms_chr = character(0), overwrite_1L_lgl = F, package_1L_chr = character(0), 
    processed_fls_dir_1L_chr = character(0), raw_fls_dir_1L_chr = character(0), 
    sub_dirs_chr = character(0), what_1L_chr = "output") 
{
    if (what_1L_chr == "ingest") {
        object_xx <- rlang::exec(fn, x, !!!args_ls)
        stop("A Make Import Object Method needs to be defined for the child class of vicinity_raw.")
    }
    if (what_1L_chr == "output") {
        ready4use::assert_single_row_tb(x)
        y_VicinityLookup <- VicinityLookup()
        y_VicinityLookup <- renewSlot(y_VicinityLookup, "vicinity_raw_r3", 
            x)
        import_type_ls <- procure.vicinity_raw(x, inc_script_1L_lgl = T, 
            forced_choice_chr = NA_character_, what_1L_chr = "source")
        if (names(import_type_ls) == "script_chr") {
            make_class_fn_chr <- eval(parse(text = import_type_ls))
            script_args_ls <- list(a_VicinityLookup = y_VicinityLookup, 
                merge_itms_chr = merge_itms_chr, processed_fls_dir_1L_chr = processed_fls_dir_1L_chr, 
                raw_fls_dir_1L_chr = raw_fls_dir_1L_chr, pkg_1L_chr = package_1L_chr, 
                overwrite_1L_lgl = overwrite_1L_lgl, crs_nbr_dbl = crs_nbr_dbl)
            z_VicinityArguments <- rlang::exec(make_class_fn_chr, 
                !!!script_args_ls)
            object_xx <- author(z_VicinityArguments)
        }
        else {
            object_xx <- VicinityLocalRaw(a_VicinityLookup = y_VicinityLookup, 
                merge_itms_chr = merge_itms_chr, raw_fls_dir_1L_chr = raw_fls_dir_1L_chr, 
                pkg_1L_chr = package_1L_chr, overwrite_1L_lgl = overwrite_1L_lgl) %>% 
                author(processed_fls_dir_1L_chr_chr = processed_fls_dir_1L_chr, 
                  crs_nbr_dbl = crs_nbr_dbl)
        }
    }
    if (what_1L_chr == "path") {
        path_element_chr <- purrr::map_chr(c("country_chr", "area_type_chr", 
            "region_chr", "main_feature_chr", "year_chr", "inc_file_main_chr"), 
            ~ready4::get_from_lup_obj(data_lookup_tb = x, match_var_nm_1L_chr = "name_chr", 
                match_value_xx = match_value_xx, target_var_nm_1L_chr = .x, 
                evaluate_1L_lgl = FALSE))
        object_xx <- paste0(raw_fls_dir_1L_chr, "/", paste(path_element_chr, 
            collapse = "/"))
    }
    if (what_1L_chr == "paths_chr") {
        paths_chr <- purrr::map_chr(sub_dirs_chr, ~ready4::get_from_lup_obj(data_lookup_tb = x, 
            match_value_xx = match_value_xx, match_var_nm_1L_chr = match_var_nm_1L_chr, 
            target_var_nm_1L_chr = .x, evaluate_1L_lgl = FALSE))
        paths_chr <- purrr::accumulate(paths_chr, ~paste0(.x, 
            "/", .y)) %>% paste0(processed_fls_dir_1L_chr, "/", 
            .)
        object_xx <- paths_chr
    }
    if (what_1L_chr == "source") {
        assert_single_row_tb(x)
        import_type_ls <- procure(x, inc_script_1L_lgl = !is.null(script_args_ls), 
            forced_choice_chr = forced_choice_chr, what_1L_chr = "source")
        object_xx <- switch(names(import_type_ls), script_chr = rlang::exec(VicinityArguments, 
            !!!script_args_ls), local_chr = ready4use::get_valid_path_chr(import_type_ls[[1]]), 
            repo_chr = ready4use::manufacture.ready4use_dataverses(x %>% 
                dplyr::select(names(ready4use::ready4use_dataverses())) %>% 
                tibble::as_tibble() %>% ready4use::ready4use_dataverses()), 
            source_url_chr = url(import_type_ls[[1]]))
    }
    return(object_xx)
}
#' @rdname manufacture-methods
#' @aliases manufacture,vicinity_raw-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("vicinity_raw", package = "vicinity"), manufacture.vicinity_raw)
#' 
#' Manufacture a new object
#' @name manufacture-VicinityLookup
#' @description manufacture method applied to VicinityLookup
#' @param x An object of class VicinityLookup
#' @param area_sf Area (a simple features object), Default: NULL
#' @param area_unit_1L_chr Area unit (a character vector of length one), Default: character(0)
#' @param att_data_xx Attribute data (an output object of multiple potential types), Default: NULL
#' @param boundary_year_1L_chr Boundary year (a character vector of length one), Default: character(0)
#' @param match_value_xx Match value (an output object of multiple potential types), Default: character(0)
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: 'Geometry'
#' @param what_1L_chr What (a character vector of length one), Default: 'attribute'
#' @param y_vicinity_raw PARAM_DESCRIPTION, Default: NULL
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,VicinityLookup-method
#' @export 
#' @importFrom stats setNames
#' @importFrom ready4 get_from_lup_obj manufacture
#' @importFrom dplyr filter pull
#' @importFrom purrr map reduce pluck map_chr
#' @importFrom stringr str_detect
methods::setMethod("manufacture", "VicinityLookup", function (x, area_sf = NULL, area_unit_1L_chr = character(0), 
    att_data_xx = NULL, boundary_year_1L_chr = character(0), 
    match_value_xx = character(0), path_1L_chr = character(0), 
    type_1L_chr = "Geometry", what_1L_chr = "attribute", y_vicinity_raw = NULL, 
    ...) 
{
    if (what_1L_chr == "attribute") {
        if (type_1L_chr == "inner") {
            att_data_xx <- ingest(x@vicinity_processed_r3, col_nm_1L_chr = "name_chr", 
                match_value_xx = match_value_xx)
            if (is.data.frame(att_data_xx)) {
                att_data_xx <- list(att_data_xx) %>% stats::setNames(ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3, 
                  match_value_xx = match_value_xx, match_var_nm_1L_chr = "name_chr", 
                  target_var_nm_1L_chr = "year_chr", evaluate_1L_lgl = FALSE))
            }
            region_short_nm_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3, 
                match_value_xx = match_value_xx, match_var_nm_1L_chr = "name_chr", 
                target_var_nm_1L_chr = "region_chr", evaluate_1L_lgl = FALSE)
            region_short_long_chr <- c(region_short_nm_1L_chr, 
                ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_abbreviations_r3, 
                  match_value_xx = region_short_nm_1L_chr, match_var_nm_1L_chr = "short_name_chr", 
                  target_var_nm_1L_chr = "long_name_chr", evaluate_1L_lgl = FALSE))
            area_names_var_chr <- ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3, 
                match_value_xx = match_value_xx, match_var_nm_1L_chr = "name_chr", 
                target_var_nm_1L_chr = "area_type_chr", evaluate_1L_lgl = FALSE) %>% 
                ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_templates_r3, 
                  match_value_xx = ., match_var_nm_1L_chr = "area_type_chr", 
                  target_var_nm_1L_chr = "subdivision_chr", evaluate_1L_lgl = FALSE)
            area_names_var_chr <- area_names_var_chr[area_names_var_chr %in% 
                names(area_sf)]
            boundary_year_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3, 
                match_value_xx = match_value_xx, match_var_nm_1L_chr = "name_chr", 
                target_var_nm_1L_chr = "area_bndy_yr_chr", evaluate_1L_lgl = F)
            area_unit_1L_chr <- x@vicinity_identifiers_r3 %>% 
                dplyr::filter(var_name_chr %in% area_names_var_chr) %>% 
                dplyr::filter(as.numeric(year_chr) == max(as.numeric(year_chr)[as.numeric(year_chr) <= 
                  as.numeric(boundary_year_1L_chr)])) %>% dplyr::pull(var_name_chr)
            att_data_xx <- manufacture(x, att_data_xx = att_data_xx, 
                area_sf = area_sf, area_unit_1L_chr = area_unit_1L_chr, 
                region_short_long_chr = region_short_long_chr, 
                match_value_xx = match_value_xx, type_1L_chr = "custom")
            object_xx <- att_data_xx
        }
        if (!type_1L_chr %in% c("inner")) {
            if (!identical(match_value_xx, character(0))) {
                att_data_xx <- manufacture(x, match_value_xx = match_value_xx, 
                  area_sf = area_sf, type_1L_chr = "inner")
                object_xx <- add_att_to_sf(area_sf = area_sf, 
                  att_data_tb = att_data_xx, att_data_desc = ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_processed_r3, 
                    match_value_xx = match_value_xx, match_var_nm_1L_chr = "name_chr", 
                    target_var_nm_1L_chr = "main_feature_chr"))
            }
            else {
                if (!identical(area_unit_1L_chr, character(0))) {
                  boundary_sf <- ingest(x@vicinity_processed_r3 %>% 
                    dplyr::filter(area_type_chr == area_unit_1L_chr) %>% 
                    dplyr::filter(main_feature_chr == "Boundary") %>% 
                    dplyr::filter(as.numeric(year_start_chr) == 
                      max(as.numeric(year_start_chr)[as.numeric(year_start_chr) <= 
                        as.numeric(boundary_year_1L_chr)])), 
                    match_value_xx = "Boundary")
                  attribute_data_ls <- purrr::map(att_data_xx, 
                    ~.x) %>% stats::setNames(att_data_xx, )
                  object_xx <- purrr::map(attribute_data_ls, 
                    ~manufacture(x, area_sf = boundary_sf, match_value_xx = .x)) %>% 
                    transform_sf_ls() %>% purrr::reduce(~rbind(.x, 
                    .y))
                }
                else {
                  object_xx <- att_data_xx
                }
            }
        }
    }
    if (what_1L_chr == "imports") {
        if (type_1L_chr == "Geometry") {
            imports_chr <- x@vicinity_raw_r3 %>% dplyr::filter(main_feature_chr == 
                "Boundary") %>% dplyr::pull(name_chr)
        }
        else {
            imports_chr <- x@vicinity_raw_r3 %>% dplyr::filter(type_1L_chr == 
                "Attribute") %>% dplyr::pull(name_chr)
            return(imports_chr)
        }
        object_xx <- imports_chr
    }
    if (what_1L_chr == "import_script") {
        if (is.null(y_vicinity_raw %>% dplyr::pull(add_bndys_from_ls) %>% 
            purrr::pluck(1))) {
            script_1L_chr <- NA_character_
        }
        else {
            if (is.na(y_vicinity_raw %>% dplyr::pull(add_bndys_from_ls) %>% 
                purrr::pluck(1)) %>% any()) {
                script_1L_chr <- NA_character_
            }
            else {
                script_1L_chr <- purrr::map_chr(y_vicinity_raw %>% 
                  dplyr::pull(add_bndys_from_ls) %>% purrr::pluck(1), 
                  ~{
                    obj_nm_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3, 
                      match_value_xx = .x, match_var_nm_1L_chr = "uid_chr", 
                      target_var_nm_1L_chr = "name_chr", evaluate_1L_lgl = FALSE)
                    action_1L_chr <- obj_nm_1L_chr %>% ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3, 
                      match_value_xx = ., match_var_nm_1L_chr = "name_chr", 
                      target_var_nm_1L_chr = "path_to_make_script_chr", 
                      evaluate_1L_lgl = FALSE)
                    ifelse(is.na(action_1L_chr, action_1L_chr, 
                      action_1L_chr %>% ifelse(stringr::str_detect(., 
                        "::"), ., paste0("readRDS(\"", path_1L_chr, 
                        "/", ., ".rds\")"))))
                  })
            }
        }
        object_xx <- script_1L_chr
    }
    return(object_xx)
})
#' 
#' Manufacture a new object
#' @name manufacture-VicinityProfile
#' @description manufacture method applied to VicinityProfile
#' @param x An object of class VicinityProfile
#' @param attributes_ls Attributes (a list), Default: NULL
#' @param attributes_to_import_chr Attributes to import (a character vector), Default: character(0)
#' @param exclude_dif_bndy_yr_1L_lgl Exclude different boundary year (a logical vector of length one), Default: TRUE
#' @param highest_rsl_chr Highest resolution (a character vector), Default: character(0)
#' @param key_var_1L_chr Key variable (a character vector of length one), Default: character(0)
#' @param match_year_1L_lgl Match year (a logical vector of length one), Default: FALSE
#' @param model_start_ymdhms_dtm Model start years months days hours minutes seconds (a date vector), Default: NULL
#' @param nbr_steps_start_to_end_1L_int Number steps start to end (an integer vector of length one), Default: integer(0)
#' @param reference_var_rsl_1L_chr Reference variable resolution (a character vector of length one), Default: NULL
#' @param reference_vals_chr Reference values (a character vector), Default: c("tot_pop", "age_sex")
#' @param simulation_steps_ymwd_dtm Simulation steps years month weeks days (a date vector), Default: NULL
#' @param specified_rsl_chr Specified resolution (a character vector), Default: character(0)
#' @param subdivision_1L_chr Subdivision (a character vector of length one), Default: NULL
#' @param type_1L_chr Type (a character vector of length one), Default: 'inner'
#' @param what_1L_chr What (a character vector of length one), Default: 'attributes'
#' @param years_chr Years (a character vector), Default: character(0)
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,VicinityProfile-method
#' @export 
#' @importFrom stringr str_sub str_length
#' @importFrom purrr map map_chr map2 map_lgl transpose map_dbl pluck
#' @importFrom stats setNames
#' @importFrom dplyr filter pull mutate select
#' @importFrom ready4 get_from_lup_obj manufacture
#' @importFrom nnet which.is.max
#' @importFrom sf st_area st_transform
#' @importFrom rlang sym
#' @importFrom lubridate year
methods::setMethod("manufacture", "VicinityProfile", function (x, attributes_ls = NULL, attributes_to_import_chr = character(0), 
    exclude_dif_bndy_yr_1L_lgl = TRUE, highest_rsl_chr = character(0), 
    key_var_1L_chr = character(0), match_year_1L_lgl = FALSE, 
    model_start_ymdhms_dtm = NULL, nbr_steps_start_to_end_1L_int = integer(0), 
    reference_var_rsl_1L_chr = NULL, reference_vals_chr = c("tot_pop", 
        "age_sex"), simulation_steps_ymwd_dtm = NULL, specified_rsl_chr = character(0), 
    subdivision_1L_chr = NULL, type_1L_chr = "inner", what_1L_chr = "attributes", 
    years_chr = character(0)) 
{
    if (what_1L_chr == "attributes") {
        if (type_1L_chr == "inner") {
            boundary_rsl_chr <- stringr::str_sub(attributes_to_import_chr, 
                5, 7) %>% unique() %>% toupper()
            data_names_ls <- purrr::map(boundary_rsl_chr, ~attributes_to_import_chr[stringr::str_sub(attributes_to_import_chr, 
                5, 7) == tolower(.x)]) %>% stats::setNames(boundary_rsl_chr)
            extra_names_chr <- purrr::map(specified_rsl_chr, 
                ~x@a_VicinityLookup@vicinity_processed_r3 %>% 
                  dplyr::filter(main_feature_chr == .x[1]) %>% 
                  dplyr::filter(make_filter_by_year_logic(data_tb = ., 
                    years_chr = years_chr)) %>% ready4::get_from_lup_obj(match_value_xx = .x[1], 
                  match_var_nm_1L_chr = "main_feature_chr", target_var_nm_1L_chr = "name_chr", 
                  evaluate_1L_lgl = FALSE)) %>% stats::setNames(purrr::map_chr(specified_rsl_chr, 
                ~.x[2]))
            rsl_to_merge_chr <- names(extra_names_chr)[names(extra_names_chr) %in% 
                boundary_rsl_chr]
            if (!identical(rsl_to_merge_chr, character(0))) {
                merged_elements_ls <- purrr::map2(data_names_ls[rsl_to_merge_chr], 
                  extra_names_chr[rsl_to_merge_chr], ~c(.x, .y))
                if (length(merged_elements_ls) == length(data_names_ls)) {
                  data_names_ls <- merged_elements_ls
                }
                else {
                  data_names_ls <- append(data_names_ls[names(data_names_ls)[!names(data_names_ls) %in% 
                    rsl_to_merge_chr]], merged_elements_ls)
                }
            }
            extra_rsls_chr <- names(extra_names_chr)[!names(extra_names_chr) %in% 
                boundary_rsl_chr]
            if (!identical(extra_rsls_chr, character(0))) {
                data_names_ls <- append(data_names_ls, extra_names_chr[extra_rsls_chr])
                boundary_rsl_chr <- c(boundary_rsl_chr, extra_rsls_chr)
            }
            attributes_ls <- purrr::map2(boundary_rsl_chr, data_names_ls, 
                ~manufacture(x@a_VicinityLookup, area_unit_1L_chr = .x, 
                  att_data_xx = .y, boundary_year_1L_dbl = x@a_VicinityLookup@vicinity_processed_r3 %>% 
                    dplyr::filter(name_chr %in% .y) %>% dplyr::pull(year_chr) %>% 
                    min(as.numeric()), what_1L_chr = "attribute")) %>% 
                stats::setNames(boundary_rsl_chr)
            index_ppr_dbl <- purrr::map_lgl(data_names_ls, ~ratify.vicinity_processed(x@a_VicinityLookup@vicinity_processed_r3, 
                data_items_chr = .x, key_var_1L_chr = key_var_1L_chr, 
                what_1L_chr = "population")) %>% which() + 1
            attributes_ls <- append(attributes_ls, list(index_ppr_dbl = index_ppr_dbl), 
                after = 0)
        }
        if (type_1L_chr == "outer") {
            years_chr <- manufacture(x, model_start_ymdhms_dtm = model_start_ymdhms_dtm, 
                nbr_steps_start_to_end_1L_int = nbr_steps_start_to_end_1L_int, 
                simulation_steps_ymwd_dtm = simulation_steps_ymwd_dtm, 
                what_1L_chr = "years")
            attributes_to_import_chr = procure(x, exclude_dif_bndy_yr_1L_lgl = exclude_dif_bndy_yr_1L_lgl, 
                highest_rsl_chr = highest_rsl_chr, key_var_1L_chr = key_var_1L_chr, 
                match_year_1L_lgl = match_year_1L_lgl, years_chr = years_chr, 
                what_1L_chr = "grouping")
            attributes_ls <- manufacture(x, attributes_to_import_chr = attributes_to_import_chr, 
                key_var_1L_chr = key_var_1L_chr, specified_rsl_chr = specified_rsl_chr, 
                type_1L_chr = "inner", what_1L_chr = "attributes", 
                years_chr = years_chr)
        }
        if (type_1L_chr == "preliminary") {
            lists_to_merge_ls <- purrr::map(subdivisions_chr, 
                ~manufacture(x, attributes_to_import_chr = attributes_to_import_chr, 
                  exclude_dif_bndy_yr_1L_lgl = exclude_dif_bndy_yr_1L_lgl, 
                  highest_rsl_chr = highest_rsl_chr, key_var_1L_chr = key_var_1L_chr, 
                  match_year_1L_lgl = match_year_1L_lgl, model_start_ymdhms_dtm = model_start_ymdhms_dtm, 
                  nbr_steps_start_to_end_1L_int = nbr_steps_start_to_end_1L_int, 
                  reference_var_rsl_1L_chr = NULL, simulation_steps_ymwd_dtm = simulation_steps_ymwd_dtm, 
                  specified_rsl_chr = specified_rsl_chr, subdivision_1L_chr = .x, 
                  type_1L_chr = "outer", what_1L_chr = "attributes", 
                  years_chr = years_chr))
            lists_to_merge_ls <- purrr::transpose(lists_to_merge_ls)
            merged_ls <- purrr::map(lists_to_merge_ls[2:length(lists_to_merge_ls)], 
                ~do.call(rbind, .x))
            names_ppr_chr <- purrr::map_chr(lists_to_merge_ls[[1]], 
                ~ifelse(length(.x[1]) == 0, NA_character_, names(.x[1])))
            ppr_idx_dbl <- purrr::map_dbl(lists_to_merge_ls[[1]], 
                ~ifelse(length(.x[1]) == 0, NA_real_, .x[1])) %>% 
                stats::setNames(names_ppr_chr)
            attribtues_ls <- append(merged_ls, list(ppr_idx_dbl = ppr_idx_dbl), 
                after = 0)
        }
        if (type_1L_chr == "updated") {
            group_by_var_1L_chr <- procure(x, what_1L_chr = "grouping")
            dynamic_var_rsl_1L_chr <- names(attributes_ls)[which(highest_rsl_chr == 
                key_var_1L_chr) + 1]
            reference_grouping_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = x@a_VivinityLookup@vicinity_identifiers_r3 %>% 
                dplyr::filter(year_chr %in% c(x@data_year_1L_chr)), 
                match_var_nm_1L_chr = "spatial_unit_chr", match_value_xx = dynamic_var_rsl_1L_chr, 
                target_var_nm_1L_chr = "var_name_chr", evaluate_1L_lgl = FALSE)
            if (!is.null(reference_var_rsl_1L_chr)) {
                reference_var_rsl_1L_chr <- names(attributes_ls)[which(highest_rsl_chr == 
                  reference_var_rsl_1L_chr) + 1]
                use_reference_var_1L_lgl <- c(dynamic_var_rsl_1L_chr, 
                  reference_var_rsl_1L_chr) %>% purrr::map_dbl(~ready4::get_from_lup_obj(data_lookup_tb = x@a_VivinityLookup@vicinity_resolutions_r3, 
                  match_var_nm_1L_chr = "area_type_chr", match_value_xx = .x, 
                  target_var_nm_1L_chr = "mean_size_dbl", evaluate_1L_lgl = F)) %>% 
                  nnet::which.is.max() == 1
                if (!use_reference_var_1L_lgl) 
                  reference_var_rsl_1L_chr <- NULL
            }
            by_band_pop_counts_sf_ls <- purrr::map(profiled_area_bands_ls, 
                ~make_reconciled_intersecting_area(profiled_sf = .x, 
                  profiled_sf_col_1L_chr = NA, profiled_sf_row_1L_chr = NA, 
                  spatial_atts_ls = attributes_ls, reference_var_rsl_1L_chr = reference_var_rsl_1L_chr, 
                  dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr, 
                  group_by_var_1L_chr = group_by_var_1L_chr, 
                  reference_grouping_1L_chr = reference_grouping_1L_chr, 
                  reference_vals_chr = reference_vals_chr, data_year_1L_chr = x@data_year_1L_chr, 
                  crs_nbr_dbl = x@crs_dbl))
            by_band_pop_counts_sf_ls <- purrr::map2(by_band_pop_counts_sf_ls, 
                names(by_band_pop_counts_sf_ls), ~.x %>% dplyr::mutate(popl_spatial_unit_chr = paste0(.y, 
                  "_", tolower(dynamic_var_rsl_1L_chr), "_", 
                  rownames(.x))) %>% dplyr::mutate(popl_spatial_unit_area_dbl = sf::st_area(.)))
            profiled_sf <- do.call(rbind, by_band_pop_counts_sf_ls)
            featured_var_pfx_1L_chr <- make_featured_var_pfx(dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr, 
                reference_vals_chr = reference_vals_chr, reference_var_rsl_1L_chr = reference_var_rsl_1L_chr, 
                data_year_1L_chr = x@data_year_1L_chr)
            profiled_sf <- remove_grouped_popl_vars(profiled_sf = profiled_sf, 
                featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
            profiled_sf <- add_dynamic_vars_to_sf(dynamic_vars_sf = attributes_ls[[attributes_ls$ppr_idx_dbl[1]]] %>% 
                dplyr::select(1), profiled_sf = profiled_sf, 
                dynamic_var_rsl_1L_chr = "UNIT_ID", dynamic_var_nm_1L_chr = "popl_spatial_unit_chr", 
                featured_var_pfx_1L_chr = featured_var_pfx_1L_chr, 
                data_year_1L_chr = x@data_year_1L_chr, crs_nbr_dbl = x@crs_dbl, 
                reference_vals_chr = reference_vals_chr)
            attributes_ls <- append(attributes_ls, list(profiled_sf = profiled_sf, 
                featured_var_pfx_1L_chr = featured_var_pfx_1L_chr))
        }
        object_xx <- attribtues_ls
    }
    if (what_1L_chr == "subdivisions") {
        group_by_var_1L_chr <- procure(x, what_1L_chr = "grouping")
        st_profiled_sf <- ingest(x, key_var_1L_chr = group_by_var_1L_chr)
        subdivision_var_nm_1L_chr <- ifelse(x@use_coord_lup_lgl, 
            x@VicinityLookup@vicinity_identifiers_r3 %>% ready4::get_from_lup_obj(match_var_nm_1L_chr = "spatial_unit_chr", 
                match_value_xx = x@rregion_type_chr, target_var_nm_1L_chr = "var_name_chr", 
                evaluate_1L_lgl = F), ready4::get_from_lup_obj(data_lookup_tb = x@VicinityLookup@vicinity_templates_r3 %>% 
                dplyr::filter(country_chr == x@country_chr) %>% 
                dplyr::filter(area_bndy_yr_dbl == x@area_bndy_yr_dbl), 
                match_var_nm_1L_chr = "area_type_chr", match_value_xx = x@area_type_chr, 
                target_var_nm_1L_chr = "subdivision_chr", evaluate_1L_lgl = FALSE))
        if (!x@use_coord_lup_lgl) {
            profiled_sf <- st_profiled_sf
            profiled_area_bands_ls <- make_sf_ls(profiled_sf = profiled_sf, 
                group_by_var_1L_chr = group_by_var_1L_chr)
            subdivisions_chr <- profiled_sf %>% dplyr::pull(!!rlang::sym(subdivision_var_nm_1L_chr)) %>% 
                as.character() %>% unique()
        }
        else {
            y_vicinity_points <- x@a_VicinityLookup@vicinity_points_r3 %>% 
                dplyr::filter(service_name_chr %in% x@features_chr)
            if (!is.na(geom_dist_limit_km(x))) {
                profiled_sf <- manufacture.vicinity_points(y_vicinity_points, 
                  bands_1L_dbl = x@nbr_bands_dbl, crs_nbr_dbl = x@crs_dbl, 
                  land_sf = st_profiled_sf, metres_1L_dbl = x@geomc_dist_limit_km_dbl * 
                    1000, type_1L_chr = "bands", what_1L_chr = "geometric")[[1]]
                profiled_area_bands_ls <- make_sf_ls(profiled_sf = profiled_sf, 
                  group_by_var_1L_chr = group_by_var_1L_chr)
            }
            if (!is.na(drive_time_limit_mins(x))) {
                profiled_area_bands_ls <- make_cluster_isochrones(vicinity_points_ls = list(y_vicinity_points), 
                  index_val_1L_int = 1, time_min_1L_dbl = 0, 
                  time_max_1L_dbl = x@drive_time_limit_mins_dbl, 
                  time_steps_1L_dbl = x@nbr_bands_dbl, travel_mode_1L_chr = x@travel_mode_chr)
                names(profiled_area_bands_ls) <- paste0("dt_band_", 
                  1:length(profiled_area_bands_ls))
                profiled_sf <- do.call(rbind, profiled_area_bands_ls) %>% 
                  sf::st_transform(x@crs_dbl[1]) %>% transform_to_simpler_sf()
            }
            subdivisions_chr <- make_intersecting_geometries(geometry_one_sf = st_profiled_sf, 
                geometry_two_sf = profiled_sf, crs_nbr_dbl = x@crs_dbl) %>% 
                dplyr::pull(!!rlang::sym(subdivision_var_nm_1L_chr)) %>% 
                as.vector() %>% unique()
        }
        profiled_area_objs_ls <- list(subdivisions_chr = subdivisions_chr, 
            profiled_sf = profiled_sf, profiled_area_bands_ls = profiled_area_bands_ls)
        object_xx <- profiled_area_objs_ls
    }
    if (what_1L_chr == "years") {
        model_end_year_dbl <- calculate_end_date(model_start_ymdhms_dtm = model_start_ymdhms_dtm, 
            nbr_steps_start_to_end_1L_int = nbr_steps_start_to_end_1L_int, 
            simulation_steps_ymwd_dtm = simulation_steps_ymwd_dtm) %>% 
            lubridate::year()
        year_opts_chr <- x@a_VicinityLookup@vicinity_processed_r3 %>% 
            dplyr::filter(main_feature_chr == key_var_1L_chr) %>% 
            dplyr::pull(year_end_chr)
        year_opts_chr <- year_opts_chr[stringr::str_length(year_opts_chr) == 
            4]
        year_opts_ref_dbl <- which((year_opts_chr %>% as.numeric() %>% 
            sort()) >= model_end_year_dbl) %>% min()
        model_end_year_dbl <- year_opts_chr %>% as.numeric() %>% 
            sort() %>% purrr::pluck(year_opts_ref_dbl) %>% as.character()
        years_chr <- as.character(as.numeric(x@data_year_1L_chr):as.numeric(model_end_year_dbl))
        object_xx <- years_chr
    }
    return(object_xx)
})
