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
