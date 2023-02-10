# methods::setMethod("import_data",
#                    "VicinityLocalProcessed",write_fls_from_sp_imp_and_upd_imp_ls)
# # methods::setMethod("makeProcessed_r4",
# #                    c("ready4Arguments"), write_fls_from_local_imp)
# methods::setMethod("makeProcessed_r4",
#                    "VicinityLocal",transform_sp_local_r4_toProcessed_r4)
# methods::setMethod("save_raw",
#                    c("VicinityLocal"), write_raw_data_from_sp_local_r4)
# methods::setMethod("update_this",
#                    "VicinityLocalProcessed",
#                    update_spProcessed_r4) # NOTE, EXTENDS GENERIC FROM OTHER PACKAGE
# methods::setMethod("updateAttrDataXx",
#                    "VicinityLookup",
#                    function(x,
#                             attr_data_xx,
#                             alt_names_sf,
#                             area_names_var_str,
#                             region_short_long_vec) {
#                      attr_data_xx
#                    })
