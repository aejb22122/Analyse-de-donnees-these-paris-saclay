# Analyses des projets de développement local géo-référencés dans le PMA typique analysé
# Spacial analysis of the LED projects in the communes 
# ploting the geo data 

# First we need to install some required packages :
install.packages("ggmap")
install.packages("mapproj")

library(readxl)

# Make sure that the variables are numeric = c("numeric pour la premiere colone", "numeric pour la deuxieme", etc)
geodata <- read_excel("~/OneDrive/Documents/Data/geodataled.xlsx", col_types = c("numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
str(geodata)

# Loading the ggplot package and ggmap, if not done allready running
library(ggplot2)
library(ggmap)

# Ploting the data
ggplot(geodata, aes(x= geodata$Longitude, geodata$Latitude)) + geom_point()

# To stop the overlay of points : in the geom_point() inter the arguments posiion_jitter
ggplot(geodata, aes(x= geodata$Longitude, geodata$Latitude)) + geom_point(position = position_jitter(w = 0.3, h = 0.3)) + xlab("Longitude") + ylab("Latitude")

# Just to keep the theme going, we'll just put the points in blue (colour = 'blue', size = 2) in the 
# geom_point() arguments
ggplot(geodata, aes(x= geodata$Longitude, geodata$Latitude)) + geom_point(position = position_jitter(w = 0.3, h = 0.3), colour = 'blue', size = 2) + xlab("Longitude") + ylab("Latitude")

# Coordonnées de Haiti (centrée sur Hinche), que l'on met dans un vecteur sur r:
haiti <- c(lon = -72.01667, lat = 19.15)

# Plot map at zoom level 5
map_ht5 <- get_map(location = haiti, zoom = 5, scale = 1)
ggmap(map_ht5)

# plot map at zoom level 9
map_ht9 <- get_map(location = haiti, zoom = 9, scale = 1)
ggmap(map_ht9)


# We can change the map type by adding : (maptype = "satellite") in the arguments
map_ht99 <- get_map(location = haiti, zoom = 9, scale = 1, maptype = "satellite")
ggmap(map_ht99)


# Add the plots to the map (the normal non satellite version):
# Remember to leave the color argument inside the aes() function within your geom_point(), to have
# the gradiant label
ggmap(map_ht9) + geom_point(aes(geodata$Longitude, geodata$Latitude), data = geodata)
ggmap(map_ht9) + geom_point(aes(geodata$Longitude, geodata$Latitude), data = geodata, colour = "red", alpha = 0.1, size = 5)
ggmap(map_ht9) + geom_point(aes(geodata$Longitude, geodata$Latitude), data = geodata, colour = "red", alpha = 0.3, size = 7)


# Add the plots satellite version
ggmap(map_ht99) + geom_point(aes(geodata$Longitude, geodata$Latitude), data = geodata, color = geodata$Budget)
ggmap(map_ht99) + geom_point(aes(geodata$Longitude, geodata$Latitude), data = geodata, colour = "red", alpha = 0.1, size = 7)
ggmap(map_ht99) + geom_point(aes(geodata$Longitude, geodata$Latitude), data = geodata, colour = "red", alpha = 0.3, size = 7)
ggmap(map_ht99) + geom_point(aes(geodata$Longitude, geodata$Latitude), data = geodata, colour = "red", alpha = 0.1, size = 7) + scale_fill_gradient(low = "blue", high = "red")


# Adding the budgets to the points
# This is the best one :
ggmap(map_ht9) + geom_point(aes(geodata$Longitude, geodata$Latitude, color = geodata$Budget), data = geodata, alpha = 0.5, size = 8)
# Map avec les financements
# Adding fiscal revenus to the map
ggmap(map_ht9) + geom_point(aes(geodata$Longitude, geodata$Latitude, color = geodata$Revenus_t3), data = geodata, alpha = 0.3, size = 8)

# An other form of map (toner version, black and white):
map_ht <- get_map(haiti, zoom = 9, source = "stamen", maptype = "toner")        # un peu noir et blanc
# Map avec les financements
ggmap(map_ht) + geom_point(aes(geodata$Longitude, geodata$Latitude, color = geodata$Budget), data = geodata, alpha = 0.5, size = 8)


# A quick alternative
qmplot(Longitude, Latitude, data = geodata, geom = "point", color = Budget) + facet_wrap(~ City)

# Heat map :
ggmap(map_ht9, extent = "device") + geom_density2d(data = geodata, aes(x = geodata$Longitude, y = geodata$Latitude), size = 0.5) + stat_density2d(data = geodata, aes(x = geodata$Longitude, y = geodata$Latitude, fill = ..level.., alpha = ..level..), size = 0.01,  bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)




