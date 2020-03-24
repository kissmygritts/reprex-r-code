library(sf)
library(here)

counties <- read_sf(here('data', 'raw', 'NV_Admin_Counties.shp'))
county_names <- casefold(counties$CNTYNAME, upper = F)

for (i in seq_along(county_names)) {
  out_name <- print(here('data', paste0(county_names[i], '-county.shp')))
  
  write_sf(counties[i, ], out_name, quiet = T)
}
