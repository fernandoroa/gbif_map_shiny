box::use(
  leaflet[...],
  glue[glue]
)

#' @export
capitalize_first <- function(string) {
  sub("([[:alpha:]]){1}(.*)", "\\U\\1\\L\\2", string, perl = TRUE)
}

#' based on https://data-blog.gbif.org/post/gbif-maps-api-using-r-and-leaflet/
#' @export
make_map_from_selection <- function(taxonID) {
  epsg4326 <- leafletCRS(
    crsClass = "L.CRS.EPSG4326", code = "EPSG:4326",
    proj4def = "+proj=longlat +datum=WGS84 +no_defs",
    resolutions = 2^(10:0),
    origin = c(0, 0)
  )

  projection <- "4326" # must use this projection code for custom maps
  style <- "gbif-geyser"
  tile_raster <- glue("https://tile.gbif.org/{{projection}}/omt/{z}/{x}/{y}@1x.png?style={{style}}", .open = "{{", .close = "}}")

  api_url <- "https://api.gbif.org/v2/map/occurrence/adhoc/{z}/{x}/{y}@1x.png?"

  style <- "classic.poly" # style of polygons

  tile_polygons <- glue("{api_url}style={style}&taxonKey={taxonID}")

  leaflet(options = leafletOptions(crs = epsg4326)) |>
    setView(
      lng = 0, lat = 0,
      zoom = 01
    ) |>
    addTiles(urlTemplate = tile_raster) |>
    addTiles(urlTemplate = tile_polygons)

}
