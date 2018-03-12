library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)    # for fortifying shapefiles
library(mapproj)


# First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# extension as arguments
shapefile <- readOGR(dsn = "maps3", layer = "tl_2016_us_aiannh")


# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(shapefile)

# Get Oregon map

all_states <- map_data("state")
oregonmap <-  subset(all_states, region %in% c( "oregon" ) )

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
map <- ggplot() +
     geom_path(data = oregonmap, 
               aes(x = long, y = lat, group = group),
               color = 'gray', fill = 'white', size = .2) +
     geom_path(data = shapefile_df, 
               aes(x = long, y = lat, group = group),
               color = 'gray', fill = 'white', size = .2) 

print(map) 

# Using the ggplot2 function coord_map will make things look better and it will also let you change
# the projection. But sometimes with large shapefiles it makes everything blow up.
map_projected <- map +
     coord_map()

print(map_projected)


