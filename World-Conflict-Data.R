# Violence dans le monde - données : "Uppsala Conflict Data Program, Department of Peace and Conflict Research"
# Données disponibles http://ucdp.uu.se 

# ----------------- préliminaires -----------
# First we need to install some required packages first :

# Packages for maps :
install.packages("ggmap")
install.packages("mapproj")

# Loading the ggplot package and ggmap:
library(ggplot2)
library(ggmap)

# Setting the working directory and looking at the variables :
setwd("~/OneDrive/Documents/3. Thesis Data/2017_Data_sets")
df <- read.csv("ged171.csv")
str(df)

# ------------------ Plotting the geocoded data on violence -------------
# Ploting the data
p <- ggplot(df, aes(x = df$longitude, df$latitude)) + geom_point()
p

# To stop the overlay of points : in the geom_point() inter the arguments posiion_jitter
p1 <- ggplot(df, aes(x=df$longitude, df$latitude)) + geom_point(position = position_jitter(w = 0.3, h = 0.3)) + xlab("Longitude") + ylab("Latitude")
p1

# Coordonnées de la carte (centrée sur la République Centre Africaine), que l'on met dans un vecteur sur r:
center_world <- c(lon = 20.939444, lat = 6.611111)

# Plot map at zoom level 1
world_map <- get_map(location = center_world, zoom = 1, scale = 1)
ggmap(world_map)

# Add the plots to the map (the normal non satellite version):
# Remember to leave the color argument inside the aes() function within your geom_point(), to have
# the gradiant label
p4 = ggmap(world_map) + geom_point(aes(df$longitude, df$latitude), data = df)
p4
# Heat map :
ggmap(world_map, extent = "device") + geom_density2d(data = df, aes(x = df$longitude, y = df$latitude), size = 1) + stat_density2d(data = df, aes(x = df$longitude, y = df$latitude, fill = ..level.., alpha = ..level..), size = 0.01,  bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)
