#### A BRIEF OVERVIEW OF THE PACKAGE SF ####
# sf is a the successor to the package sp, headed by the same developer
# sf is faster than sp, generally more predictable, and is under active development



#### Reading files ####
## Shapefiles and feature classes are generally read in with sf::st_read()
# Note: Basically all sf functions start with "st_" thanks to PostGIS
input_filepath <- "C:/Users/Nelson/Documents/Projects/thiessen_polygon_estimates/simulations/continuous/unstratified/intensified/intensification_continuous/output/spatial"

# Make the call to sf::st_read()
# Note: The layer argument (filename or feature class name) doesn't require a file extension
polygon1 <- sf::st_read(dsn = input_filepath,
                        layer = "aoi_1")
polygon2 <- sf::st_read(dsn = input_filepath,
                        layer = "aoi_2")
polygon3 <- sf::st_read(dsn = input_filepath,
                        layer = "aoi_3")

# Reading in with sf::st_read() creates an sf object, which is a fancy data frame
class(polygon1)

# A way to check to see if an object is an sf object in your code is to ask if
# "sf" is in the results of class() because, depending on the class of the object
# in question (including sf objects), class() will return a vector of variable length
if ("sf" %in% class(polygon1)) {
  message("polygon1 is an sf object")
}

# sf objects know what kind of geometry they hold
# It *is* possible for an sf object to hold multiple related geometry types,
# e.g. both POLYGON and MULTIPOLYGON
sf::st_geometry_type(polygon1)

# The type of feature isn't important, just that it's a feature class
# so there's no difference between reading polygons and reading points
points <- sf::st_read(dsn = input_filepath,
                      layer = "points_1")

# We don't need all the points or all the variables, so we can trim
# exactly like we would with a data frame
points <- points[points$smpl_sd == 1, "value"]

class(points)
sf::st_geometry_type(points)

## If you happen to have an sp object that you need to convert, use sf::st_as_sf()



#### Plotting features ####
# sf objects actually play nice with ggplot!
library(ggplot2)

# The simplest plot: a single call to geom_sf() which will handle any sf object
ggplot() +
  geom_sf(data = polygon1)

# Plotting multiple sf objects requires multiple calls (and order matters!)
# You can mix-and-match sf objects as long as they all share a projection/CRS
ggplot() +
  geom_sf(data = polygon1) +
  geom_sf(data = polygon2) +
  geom_sf(data = polygon3) +
  geom_sf(data = points)

# Aesthetic arguments work just like you'd expect from ggplot
ggplot() +
  geom_sf(data = polygon1,
          alpha = 0.5,
          fill = "#685b7fff") +
  geom_sf(data = polygon2,
          alpha = 0.5,
          fill = "#4a8b9fff") +
  geom_sf(data = polygon3,
          alpha = 0.5,
          fill = "#f5bb57ff") +
  geom_sf(data = points)

# They play nice with rasters too!
raster1 <- raster::raster(x = paste0(input_filepath, "/raster_1.tif"))
raster_plotting_df <- data.frame(raster::rasterToPoints(raster1))

ggplot() +
  geom_raster(data = raster_plotting_df,
              aes(x = x,
                  y = y,
                  fill = raster_1)) +
  geom_sf(data = polygon1,
          alpha = 0.25) +
  geom_sf(data = polygon2,
          alpha = 0.25) +
  geom_sf(data = polygon3,
          alpha = 0.25) +
  geom_sf(data = points)




#### Projecting and reprojecting ####
# Projections are a common source of heartburn when dealing with spatial data
# sf arsenal for dealing with projections includessf::st_transform() and sf::st_crs()

# sf::st_crs() is used to either retrieve the current coordinate reference system (CRS)
# OR specify a CRS but WITHOUT TRANSFORMING THE DATA
# Only use sf::st_crs() to give a feature a CRS is it's already projected into that CRS
sf::st_crs(polygon1)
crs_polygon1 <- sf::st_crs(polygon1)

# sf::st_transform() will let you take an sf object that already has a defined CRS
# and project it into a different CRS. This will change nothing if the new CRS matches
# the current CRS, thankfully
# sf::st_transform() will take either a CRS object from sf::st_crs() OR you can
# use a proj4 string like you would with sp objects, e.g. "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
polygon2 <- sf::st_transform(x = polygon2,
                             crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

polygon2 <- sf::st_transform(x = polygon2,
                             crs = crs_polygon1)




#### Getting just data ####
# To get geometry, you use sf::st_geometry()
points_geometry <- sf::st_geometry(points)

polygon1_geometry <- sf::st_geometry(polygon1)

# This can also be used to remove geometry
points_df <- points
sf::st_geometry(points_df) <- NULL

# You can also easily grab coordinates
points_coords <- sf::st_coordinates(points)
points_coords_df <- as.data.frame(points_coords)



#### Creating a point feature from coordinates ####
# Use sf::st_as_sf() to create a data frame from a matrix or data frame of coordinates
new_points_df <- data.frame(point_id = c("alpha", "beta", "gamma"),
                            x = sample(x = 1:10,
                                       size = 3),
                            y = sample(x = 1:10,
                                       size = 3))

# So we supply the data frame, the names of the x and y variables in the data frame
# and a CRS. In this case we're just using the CRS we grabbed from polygon1 because
# we know that for these data that's the correct projection. We'd specify another
# if we know these coordinates were in a different CRS.
new_points <- sf::st_as_sf(x = new_points_df,
                           coords = c("x", "y"),
                           crs = sf::st_crs(polygon1))

ggplot() +
  geom_sf(data = new_points)




#### Manipulating features ####
##### Combining #####
# Like with sp objects and plain data frames, you can use rbind() with sf objects
# as long as they have the same variables
polygons <- rbind(polygon1,
                  polygon2,
                  polygon3)

ggplot() +
  geom_sf(data = polygons)



##### Getting area #####
# Use sf::st_area() to calculate the area of polygons
# This will use the units of the projection/CRS
polygons_area <- sf::st_area(polygons)
class(polygons_area)

# If you want it to not be a special units object, use as.numeric()
polygons_area <- as.numeric(polygons_area)




##### Intersections #####
# Intersections are done with sf::st_intersection(), but there are two different
# behaviors for the function
# Intersecting an x object with a y object returns only the intersection
intersection_test1 <- sf::st_intersection(x = polygon1,
                                          y = polygon2)

ggplot() +
  geom_sf(data = intersection_test1)

# Intersecting JUST an object with multiple polygons will return all the unique
# combinations of polygons with information about the number of overlaps (n.overlaps)
# AND the indices of the polygons that overlapped (origins)
intersection_test2 <- sf::st_intersection(x = polygons)

ggplot() +
  geom_sf(data = intersection_test2,
          aes(fill = n.overlaps))

# When I want a unique ID for each unique combination of source polygons, I
# collapse the vectors in the variable origin with a sapply()
intersection_uids <- sapply(X = intersection_test2$origins,
                            FUN = function(X) {
                              paste(X,
                                    collapse = "-")
                            })

# Then they can just be written in!
intersection_test2$uid <- intersection_uids

# As a note, be careful when changing names for an sf object's variables because
# you can break the object if you rename the "geometry" variable accidentally
# since unlike with an sp object the geometry isn't hidden away outside the data frame
names(intersection_test2)

ggplot() +
  geom_sf(data = intersection_test2,
          aes(fill = uid))

# Note the results of st::geometry_type() here
# There are two multipolygons and one polygon because they're "dissolved" by origin
# This *shouldn't* cause trouble, but you never know
sf::st_geometry_type(intersection_test2)




##### Casting #####
# You can use sf::st_cast() to convert between geometry types when needed,
# but it's not magic
# POLYGON to MULTIPOLYGON works more or less invisibly
multipoly_cast <- sf::st_cast(x = intersection_test2,
                              to = "MULTIPOLYGON")

sf::st_geometry_type(multipoly_cast)

ggplot() +
  geom_sf(data = multipoly_cast,
          aes(fill = uid))

# MULTIPOLYGON to POLYGON loses data though!
poly_cast <- sf::st_cast(x = intersection_test2,
                         to = "POLYGON")

ggplot() +
  geom_sf(data = poly_cast,
          aes(fill = uid))




##### Unions (dissolving) #####
# sf::st_union() is the equivalent of a dissolve in Arc
polygons_union <- sf::st_union(x = polygons)

# Note that that produces a list though! This is an sfg or "simple feature geometry" object
sf::st_geometry_type(polygons_union)

# So we need to convert it into an sf object
# Note that sfg objects retain no information from the source data frame!!!
polygons_union <- sf::st_sf(polygons_union)

ggplot() +
  geom_sf(data = polygons_union)




##### Joins #####
# Joins are very simple if all you want to do is a spatial join (based on intersection)
join_test <- sf::st_join(x = points,
                         y = intersection_test2[, c("uid")])

# The output is the x argument but with attributes inherited from the y argument
sf::st_geometry_type(join_test)

ggplot() +
  geom_sf(data = intersection_test2) +
  geom_sf(data = join_test,
          aes(color = uid))

# By default, sf::st_join() performs a "left" join, returning everything from x
# with NA values for observations in x that didn't intersect y
# Using the argument left = FALSE will perform an "inner" join and return only
# the observations from x that intersect y

# Supposedly, you can specify different join types with the argument join which defaults
# to st_intersects, but I've never used it successfully
