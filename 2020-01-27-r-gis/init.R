# load libraries ----
library(sf)
library(here)

# load data ----
source(here('scripts', 'load-data.R'))

# begin script ----
## base r
aoi <- counties[counties$CNTYNAME %in% c('White Pine', 'Lincoln'), ]

## dplyr
aoi <- dplyr::filter(counties, CNTYNAME %in% c('White Pine', 'Lincoln'))

## subset roads network
wp_county <- aoi[aoi$CNTYNAME == 'White Pine', ]
roads_aoi <- roads[aoi, ]
wp_roads <- roads_aoi[wp_county, ]

plot(st_geometry(wp_county))
plot(st_geometry(roads_aoi), col = 'purple', add = T)

hwy_93 <- wp_roads[wp_roads$NAME == '93', ]

plot(st_geometry(wp_county))
plot(st_geometry(hwy_93), col = 'purple', add = T)

hwy_50 <- wp_roads[wp_roads$NAME == '50', ]

plot(st_geometry(wp_county))
plot(st_geometry(hwy_93), col = 'purple', add = T)
plot(st_geometry(hwy_50), col = 'orange', add = T)

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

hwy_50_coords <- as.data.frame(st_coordinates(hwy_50))
plot(hwy_50_coords$X, hwy_50_coords$Y, asp = 1)

# extract the segment
hwy_50_segment <- hwy_50_coords[hwy_50_coords$X >= min(intersection$X) &
                                  hwy_50_coords$X <= max(intersection$X), ]

# plot it
plot(st_geometry(wp_county))
plot(st_geometry(hwy_93), col = 'purple', lwd = 2, add = T)
plot(st_geometry(hwy_50), col = 'orange', lwd = 2, add = T)
points(x = intersection$X, y = intersection$Y, 
       col = 'cornflowerblue', pch = 19, cex = 2)
# points(x = hwy_50_segment$X, y = hwy_50_segment$Y,
#        col = 'springgreen', pch = 20)
lines(x = hwy_50_segment$X, y = hwy_50_segment$Y,
       col = 'springgreen', pch = 20, lwd = 3)

hwy_50_line <- st_linestring(x = as.matrix(hwy_50_segment[, 1:2]))
plot(hwy_50_line, lwd = 2, col = 'springgreen')

## that didn't work properly
hwy_50_line <- st_linestring(
  x = as.matrix(hwy_50_segment[order(hwy_50_segment$X), 1:2])
)
plot(hwy_50_line, lwd = 2, col = 'springgreen')

