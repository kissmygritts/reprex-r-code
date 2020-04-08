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
  # parse filename
  filename <- strsplit(basename(file), split = '\\.')

  # initiate conditions
  is_kml <- filename[[1]][-1] == 'kml'
  is_kmz <- filename[[1]][-1] == 'kmz'

  # check that input is kml or kmz, throw error otherwise
  if(!(any(is_kml, is_kmz))) stop('file must be a kml or kmz file')
}
