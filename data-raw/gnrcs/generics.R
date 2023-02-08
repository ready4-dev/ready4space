# add_path_col <- function(x,
#                          ...){
#   UseMethod("add_path_col",x)
# }
# gen_param_vals <- function(x,
#                          n_its_int,
#                          jt_dist = FALSE,
#                          ...){
#   if(n_its_int < 1 | n_its_int %% 1 != 0)
#     stop("n_its_int must be a positive integer")
#   UseMethod("gen_param_vals",x)
# }
# make_data_packs <- function(x,
#                             ...){
#   UseMethod("make_data_packs",x)
# }
# make_import_object <- function(x,
#                                ...){
#   UseMethod("make_import_object",x)
# }
# make_main_intersect_tb <- function(x,
#                                    ...){
#   UseMethod("make_main_intersect_tb",x)
# }
# methods::setGeneric("makeProcessed_r4", function(x,
#                                                    ...) standardGeneric("makeProcessed_r4"))
#
# methods::setGeneric("updateAttrDataXx",
#                     function(x,...,verbose=TRUE) standardGeneric("updateAttrDataXx"),
#                     signature = "x")
