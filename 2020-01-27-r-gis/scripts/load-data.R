library(sf)
library(here)

# load data ----
counties <- read_sf(here('data', 'nv-counties.gpkg'))
roads <- read_sf(here('data', 'nv-roads.gpkg'))
features <- read_sf(here('data', 'nv-features.gpkg'))

# wrangle data ----
# aoi <- counties[counties$CNTYNAME %in% c('White Pine', 'Lincoln'), ]
# roads_aoi <- roads[aoi, ]
# 
# wp_county <- aoi[aoi$CNTYNAME == 'White Pine', ]
# wp_roads <- roads_aoi[wp_county, ]


