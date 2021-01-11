cent_as_cols <- function(polygonx, names = c("centlat", "centlng")){
  centroids_plus <- do.call(rbind, st_centroid(polygonx$geometry)) %>% 
    tibble::as_tibble() %>% stats::setNames(c(names[1],names[2])) %>% dplyr::bind_cols(polygonx, .)
  return(centroids_plus)
}