library(sf)
library(fs)
library(magrittr)
library(leaflet)

# read .kml ----
routes_sf_1 <- read_sf('data/routes.kml')
routes_sf_1

# multilayer .kml ----
# this throws Warning
routes_sf_2 <- read_sf('data/routes-multi.kml')
routes_sf_2

# lets try and read all the layers into R
input_file <- 'data/routes-multi.kml'

(kml_layers <- st_layers(input_file))

# now iterate over the layers and load them?
routes_list <- lapply(kml_layers$name, function(x) {
  read_sf(input_file, layer = x)
})

# then rbind
routes_sf_2 <- Reduce('rbind', routes_list)

# check for equality between routes
assertthat::are_equal(routes_sf_1, routes_sf_2)

## lapply in a single line
Reduce('rbind', lapply(kml_layers$name, read_sf, dsn = input_file))

# read .kmz ----
input_file <- 'data/routes.kmz'
read_sf(input_file)

# workaround
target_file <- 'data/.temp.kml.zip'
fs::file_copy(input_file, target_file)
unzip(target_file, )

# read as kml now
(routes_sf_3 <- read_sf('doc.kml'))

# check for equality between routes
assertthat::are_equal(routes_sf_1, routes_sf_3)

# cleanup the temp files
fs::file_delete(target_file)
fs::file_delete('doc.kml')

# encapsulate it ----
read_keyhole <- function(file) {
  # get file extension
  ext <- strsplit(basename(file), split = '\\.')[[1]][-1]
  
  if (ext == 'kml') {
    # if kml
    layers <- st_layers(file)$name
    
    if (length(layers) > 1) {
      # if multi-layer
      return(Reduce('rbind', lapply(layers, sf::read_sf, dsn = file)))
    }
    
    return(read_sf(file))
  } else {
    #if kmz
    target_file <- '.temp.kml.zip'
    
    fs::file_copy(file, target_file, overwrite = T)
    unzip(target_file, overwrite = T)
    
    sf_out <- read_sf('doc.kml')
    
    fs::file_delete(target_file)
    fs::file_delete('doc.kml')
    
    return(sf_out)
  }
}

read_keyhole('data/routes.kml')
read_keyhole('data/routes-multi.kml')
read_keyhole('data/routes.kmz')

# generate the map in the post ----
read_keyhole('data/routes.kml') %>% 
  st_zm() %>% 
  leaflet(options = list(zoomControl = F, attributionControl = F)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addProviderTiles(providers$OpenMapSurfer.Hillshade, 
                   options = providerTileOptions(opacity = 0.25)) %>%
  addPolylines(color = '#34E084', opacity = .6)
