# Regenerate the bundled public-domain Natural Earth 1:110m world map.
# Development-only dependencies: sf and rnaturalearthdata.
library(sf)
world <- rnaturalearthdata::countries110
world <- st_wrap_dateline(st_geometry(world), options = "WRAPDATELINE=YES")
project <- function(lon, lat) {
  theta <- asin(sqrt(3) / 2 * sin(lat * pi / 180))
  t2 <- theta^2
  denominator <- 1.340264 + 3 * -0.081106 * t2 +
    t2^3 * (7 * 0.000893 + 9 * 0.003796 * t2)
  x <- 2 * sqrt(3) * (lon * pi / 180) * cos(theta) / (3 * denominator)
  y <- theta * (1.340264 - 0.081106 * t2 + t2^3 * (0.000893 + 0.003796 * t2))
  cbind(250 + 85 * x, 135 - 85 * y)
}
ring_path <- function(ring) {
  xy <- project(ring[, 1], ring[, 2])
  paste0("M", paste(sprintf("%.2f,%.2f", xy[, 1], xy[, 2]), collapse = "L"), "Z")
}
polygons <- st_cast(world, "POLYGON")
land <- paste(vapply(polygons, function(polygon) {
  paste(vapply(polygon, ring_path, character(1)), collapse = "")
}, character(1)), collapse = "")
edge <- rbind(cbind(-180, seq(-90, 90, by = 1)),
              cbind(180, seq(90, -90, by = -1)))
asset <- c(ring_path(edge), land)
writeLines(asset, file.path("inst", "extdata", "equal-earth-world-path.txt"), useBytes = TRUE)
