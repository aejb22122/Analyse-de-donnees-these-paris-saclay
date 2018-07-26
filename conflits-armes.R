# ---- Thèse de doctorat Annick Eudes JEAN-BAPTISTE ----
# Codes de réplication des calculs de la Carte # 2 de l'introduction
# Violence dans le monde - données : "Uppsala Conflict Data Program, Department of Peace and Conflict Research"
# Données disponibles http://ucdp.uu.se 

# ----------------- préliminaires -----------
# Cleaning the session form other the objects in the environment
remove(list=ls())
ls()

# First we need to install some required packages first :

# Packages for maps :
install.packages("ggplot2")     # ggmap is dependant on the ggplot2 package.
install.packages("ggmap")
install.packages("mapproj")

# Loading the ggplot package and ggmap:
library(ggplot2)
library(ggmap)

# Setting the working directory and looking at the variables :
setwd("~/OneDrive/Documents/2_Data_analysis_research/GitHub/Analyse-de-donnees-these-paris-saclay/datasets")

# Importing the data we will be using:
# df <- read.csv("ged171.csv") # this is the longer version 1989 to 2016
library(readxl)
df <- read_excel("ged171_2010-2016.xlsx")

str(df)

# ------------------ Plotting the geocoded data on violence -------------
# Ploting the data
p <- ggplot(df, aes(x = df$longitude, df$latitude)) + geom_point()
p

# To stop the overlay of points : in the geom_point() inter the arguments posiion_jitter
p1 <- ggplot(df, aes(x=df$longitude, df$latitude)) + geom_point(position = position_jitter(w = 0.3, h = 0.3)) + xlab("Longitude") + ylab("Latitude")
p1

# Coordonnées de la carte (centrée sur la République Centre Africaine), que l'on met dans un vecteur sur R:
center_world <- c(lon = 20.939444, lat = 6.611111)
#get_map(location = center_world, zoom = "auto", scale = "auto", maptype = c("terrain", "terrain-background", "satellite", "roadmap", "hybrid", "toner", "watercolor", "terrain-labels", "terrain-lines", "toner-2010", "toner-2011", "toner-background", "toner-hybrid", "toner-labels", "toner-lines", "toner-lite"), source = c("google", "osm", "stamen", "cloudmade"), force = ifelse(source == "google", TRUE, TRUE), messaging = FALSE, urlonly = FALSE, filename = "ggmapTemp", crop = TRUE, color = c("color", "bw"), language = "en-EN", api_key)


# Plot map at zoom level 1:
#world_map <- get_map(location = center_world, zoom = "auto", scale = "auto")
# Some times in the get_map() zoom argument, you need to tweek it a little to have the 
# desired outcome.
world_map <- get_map(location = center_world, zoom = 2, scale = 1,  
                     source = "google", maptype = "terrain") # This part will give the different map estatics.
ggmap(world_map)

# Different aestetics :
# maptype = roadmap - from google:
# world_map <- get_map(location = center_world, zoom = 2, scale = 1, source = "google", maptype = "terrain")
# maptype = stamen: terrain from stamen):
# world_map <- get_map(location = center_world, zoom = 2, scale = 1, source = "stamen", maptype = "terrain")  


# Add the plots to the map (the normal non satellite version):
# Remember to leave the color argument inside the aes() function within your geom_point(), to have
# the gradiant label
ggmap(world_map) + geom_point(aes(df$longitude, df$latitude), 
                   data = df,
                   alpha = .025, 
                   color = "red") +
        xlab("Longitude") +
        ylab("Latitude")

# Heat map :
ggmap(world_map, extent = "device") + geom_density2d(data = df, aes(x = df$longitude, y = df$latitude), size = 1) + stat_density2d(data = df, aes(x = df$longitude, y = df$latitude, fill = ..level.., alpha = ..level..), size = 0.01,  bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)
