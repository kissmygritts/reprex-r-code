library(sf)
library(here)
library(magrittr)

read_telemetry <- function (directory, pattern) {
  ## get all files in a directory
  files <- dir(directory)
  
  ## filter files to return those matching the pattern
  files <- files[!(is.na(stringr::str_extract(files, pattern)))]
  
  ## loop over files, read from directory with read_csv and
  ## reduce into a data frame with the rbind function
  dat <- Reduce('rbind', lapply(files, function (x) {
    ## change this to any IO function, maybe sf::read_sf, read.table, etc...
    readr::read_csv(file.path(directory, x))
  }))
}

# ----
files <- list.files('data', pattern = 'shp', full.names = T)

# read shapefiles within lapply, with read_sf
shp_list <- lapply(seq_along(files), function (i) {
  read_sf(files[i])
})

## check the items in the list if you want
shp_list[[1]]
shp_list[[2]]
#  and so on... 

# so, that is a spatial object that can't be written as a csv, 
# if you only want the attribute data, use st_drop_geometry in 
# the lapply. This will return a data.frame (or a tibble). 
# NOTE: this requires that the shapefiles have the same number
# of columns in the same order, otherwise you'll have
# errors or mismatches in the resulting data.frame
shp_list <- lapply(seq_along(files), function (i) {
  st_drop_geometry(read_sf(files[i]))
})

## check the items in the list if you want
shp_list[[1]]
shp_list[[2]]
#  and so on... 

# now, depending on how you want to output the data, either all as separate 
# csvs or a single csv with all of the data together, so...

# to write as a single csv
attr_data <- Reduce('rbind', shp_list)

readr::write_csv(attr_data, here('out', 'all-attr.csv'))

# to write each to it's own csv, modify the lapply function.
# no need to assign to a variable since the lapply function 
# will not return anything. NOTE: if you go this route, you
# don't need to worry about the shapefiles having the same 
# number of columns and those columns in the same order

lapply(seq_along(files), function (i) {
  ## read data
  attr_data <- st_drop_geometry(read_sf(files[i]))
  
  ## write data
  readr::write_csv(attr_data, here('out', paste0(attr_data$CNTYNAME, ' attr.csv')))
})
