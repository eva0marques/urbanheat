#' Create prediction grid for BHM in a polygon
#' @param polygon a POLYGON sfc 
create_grid <- function(polygon) {
  stopifnot(
    "polygon is not a sf, sfc" =
      inherits(x, c("sf", "sfc"))
  )
  if (inherits(x, c("sf"))) {
    sf <- polygon[, attributes(polygon)$sf_column]
  } else {
    sf <- polygon
  }
  geom <- sf::st_geometry_type(sf, by_geometry = FALSE)
  stopifnot(
    "polygon is not a POLYGON or MULTIPOLYGON" =
      geom %in% c("POLYGON", "MULTIPOLYGON")
  )
  
}