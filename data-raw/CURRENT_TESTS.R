devtools::load_all()
hss_vicinity_points <- utils::read.csv(file = "/Users/mahamilton/Documents/WIP/ready4/Insight/Analysis/vicinity_tests/headspace_points.csv", stringsAsFactors = F) %>% make_r3_from_csv_tb(vicinity_points)
#hss_vicinity_points$service_name_chr %>% sort()
hss_vicinity_points <- dplyr::filter(hss_vicinity_points,
                                     service_name_chr %in% c("Craigieburn", "Glenroy", "Melton", "Sunshine", "Werribee"))
# hss_vicinity_points <- dplyr::mutate(hss_vicinity_points, cluster_name_chr = c(rep("A",2),rep("B",3)))

