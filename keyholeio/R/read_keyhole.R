#' Read simple features from kml or kmz files
#'
#' @param file a file path to a valid .kml or .kmz file
#'
#' @return an sf object when the layer is successfully read.
#' @export
#'
#' @examples
#' read_keyhole(system.file('extdata', 'routes.kml', package = 'keyholeio'))

read_keyhole <- function(file) {
  # get file extension
  ext <- strsplit(basename(file), split = '\\.')[[1]][-1]

  # if kml
  if (ext == 'kml') {
    layers <- sf::st_layers(file)$name

    if (length(layers) > 1) {
      return(Reduce('rbind', lapply(layers, sf::read_sf, dsn = file)))
    }

    return(sf::read_sf(file))
  } else if (ext == 'kmz') {
    target_file <- '.temp.kml.zip'

    fs::file_copy(file, target_file, overwrite = T)
    unzip(target_file, overwrite = T)

    sf_out <- sf::read_sf('doc.kml')

    fs::file_delete(target_file)
    fs::file_delete('doc.kml')

    return(sf_out)
  } else {
    stop('file must be a kml or kmz file')
  }
}
