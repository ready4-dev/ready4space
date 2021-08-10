# methods::setMethod("import_data",
#                    "ready4_sp_local_proc",write_fls_from_sp_imp_and_upd_imp_ls)
# # methods::setMethod("make_local_proc_r4",
# #                    c("ready4_script_data"), write_fls_from_local_imp)
# methods::setMethod("make_local_proc_r4",
#                    "ready4_sp_local",transform_sp_local_r4_to_local_proc_r4)
# methods::setMethod("save_raw",
#                    c("ready4_sp_local"), write_raw_data_from_sp_local_r4)
# methods::setMethod("update_this",
#                    "ready4_sp_local_proc",
#                    update_sp_local_proc_r4) # NOTE, EXTENDS GENERIC FROM OTHER PACKAGE
# methods::setMethod("updateAttrDataXx",
#                    "ready4_lookup",
#                    function(x,
#                             attr_data_xx,
#                             alt_names_sf,
#                             area_names_var_str,
#                             region_short_long_vec) {
#                      attr_data_xx
#                    })
