library(sf)
library(leaflet)

counties <- read_sf('nv_counties/NV_Admin_Counties.shp') %>% 
  sf::st_transform(crs = 4326)
gnis <- readr::read_delim('NV_Features.zip', delim = "|")
features <- st_as_sf(
  x = gnis[complete.cases(gnis[, c('PRIM_LAT_DEC', 'PRIM_LONG_DEC')]) &
             gnis$PRIM_LAT_DEC != 0 & gnis$PRIM_LONG_DEC != 0, ],
  coords = c('PRIM_LONG_DEC', 'PRIM_LAT_DEC'),
  crs = 4326
)
nv_major_roads <- read_sf('nv-major-roads/NV_Rds_MajorRoads.shp') %>% 
  st_transform(4326)

# write features ----
write_sf(features, 'nv-features.gpkg')
write_sf(counties, 'nv-counties.gpkg')
write_sf(nv_major_roads, 'nv-roads.gpkg')


# start work ----
counties <- read_sf('nv-counties.gpkg')
features <- read_sf('nv-features.gpkg')
nv_roads <- read_sf('nv-roads.gpkg')

## plot ----
plot(st_geometry(features), pch = 20, col = scales::alpha('purple', .1))
plot(st_geometry(counties), border = 'white', col = NA, lwd = 3, add = T)

# aoi ---
aoi <- counties[counties$CNTYNAME %in% c('White Pine', 'Lincoln'), ]
aoi <- dplyr::filter(counties, CNTYNAME %in% c('White Pine', 'Lincoln'))
plot(aoi['CNTYNAME'])

# roads, redo ----
plot(st_geometry(nv_roads))
nv_roads

roads_aoi <- nv_roads[aoi, ]
plot(st_geometry(aoi))
plot(st_geometry(roads_aoi), col = 'purple', add = T)

## roads: hwy 93 & 50
hwy_93 <- st_geometry(roads_aoi[roads_aoi$NAME == '93', ])
hwy_50 <- st_geometry(roads_aoi[roads_aoi$NAME == '50', ])

plot(hwy_93, col = 'purple', lwd = 2)
plot(hwy_50, col = 'orange', lwd = 2, add = T)

### road things ----
wp_county <- aoi[aoi$CNTYNAME == 'White Pine', ]
roads_aoi <- nv_roads[aoi, ]
wp_roads <- roads_aoi[wp_county, ]

plot(st_geometry(wp_county))
plot(st_geometry(wp_roads), col = 'purple', add = T)

hwy_93 <- wp_roads[wp_roads$NAME == '93', ]

plot(st_geometry(wp_county))
plot(st_geometry(hwy_93), col = 'purple', lwd = 2, add = T)

hwy_50 <- wp_roads[wp_roads$NAME == '50', ]

plot(st_geometry(wp_county))
plot(st_geometry(hwy_93), col = 'purple', lwd = 2, add = T)
plot(st_geometry(hwy_50), col = 'orange', lwd = 2, add = T)

### why are there 4 points?
intersection <- st_intersection(
  st_geometry(hwy_93), 
  st_geometry(hwy_50)
)
intersection <- as.data.frame(st_coordinates(intersection)[c(1, 3), ])

plot(st_geometry(wp_county))
plot(st_geometry(hwy_93), col = 'purple', lwd = 2, add = T)
plot(st_geometry(hwy_50), col = 'orange', lwd = 2, add = T)
points(x = intersection$X, y = intersection$Y, 
       col = 'cornflowerblue', pch = 19, cex = 2, add = T)

## isolate portion of hwy 50 that is also 93
hwy_50_coords <- as.data.frame(st_coordinates(hwy_50))
hwy_50_segment <- hwy_50_coords[hwy_50_coords$X >= min(intersection$X) &
                                  hwy_50_coords$X <= max(intersection$X), ]

plot(st_geometry(wp_county))
plot(st_geometry(hwy_93), col = 'purple', lwd = 2, add = T)
plot(st_geometry(hwy_50), col = 'orange', lwd = 2, add = T)
points(x = intersection$X, y = intersection$Y, 
       col = 'cornflowerblue', pch = 19, cex = 2)
points(x = hwy_50_segment$X, y = hwy_50_segment$Y,
       col = 'springgreen', pch = 20)

### create a line from this portion
hwy_50_line <- st_linestring(x = as.matrix(hwy_50_segment[order(hwy_50_segment$X), 1:2]))
# hwy_50_line <- st_linestring(x = as.matrix(hwy_50_segment[, 1:2]))
# hwy_50_line <- st_linestring(x = as.matrix(hwy_50_segment[, 1:2]))
plot(hwy_50_line)

### merge into hwy 93
hwy_50_line_geom <- st_geometry(st_multilinestring(list(hwy_50_line)))
st_crs(hwy_50_line_geom) <- st_crs(4326)
plot(st_geometry(hwy_93), lwd = 2)
plot(hwy_50_line_geom, col = 'green', lwd = 2, add = T)

plot(st_union(st_geometry(hwy_93)))
hwy_93_merged <- st_union(hwy_93_merged, st_union(st_geometry(hwy_93)))
hwy_93_merged

plot(hwy_93_merged)

# extract the eastern portion white pine county
wp_county <- aoi[aoi$CNTYNAME == 'White Pine', ]
buffered <- st_buffer(st_transform(hwy_93_merged, 26911), dist = 3)



split <- st_difference(aoi,
  st_buffer(st_transform(hwy_93_merged, 26911), dist = 3)
)
class(split)

# road ----
roads <- read_sf('roads/roads.shp') %>% 
  st_transform(4326)

roads_aoi <- roads[aoi, ]

plot(st_geometry(roads_aoi[grepl('Hwy 50', roads_aoi$FULLNAME), ]))

# recreate road
hwy93 <- roads_aoi[grepl('Hwy 93', roads_aoi$FULLNAME), ]
hwy50 <- roads_aoi[grepl('Hwy 50', roads_aoi$FULLNAME), ]
plot(st_geometry(hwy50))     
plot(st_geometry(hwy93), add = T)

intersection <- st_intersection(hwy93, hwy50)
plot(st_geometry(hwy50))     
plot(st_geometry(hwy93), add = T)
st_coordinates(st_geometry(intersection))

plot(st_geometry(intersection), add = T, col = 'green', lwd = 4)
