#' @importMethodsFrom ready4use update_this
#' @export
#' @include s4_ready4_sp_local_proc.R
methods::setMethod("update_this",
                   "ready4_sp_local_proc",
                   function(x) {
                     lookup_tbs_r4 <- x@lup_tbs_r4
                     sp_import_lup <- lookup_tbs_r4@sp_import_lup
                     ready4use::assert_single_row_tb(sp_import_lup)
                     if(sp_import_lup$data_type == "Geometry"){
                       lookup_tbs_r4 <- export_starter_sf(lookup_tbs_r4,
                                                          path_to_starter_sf_chr = x@path_to_starter_sf_chr) %>%
                         export_uid_lup()
                     }
                     #}
                     lookup_tbs_r4 %>%
                       export_data_pack_lup(template_ls = x@import_this_ls,
                                            tb_data_type = sp_import_lup$data_type,
                                            pckg_name = pckg_name)

                   }) # NOTE, EXTENDS GENERIC FROM OTHER PACKAGE
