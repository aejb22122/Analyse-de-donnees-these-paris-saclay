
# ---- Thèse de doctorat Annick Eudes JEAN-BAPTISTE ----
# Codes de réplication de la carte Chapitre # 5 - localisation des projets de DEL par régions

# ---- Préliminaires ----
# Adding the packages used in this analysis

install.packages("xlsx")                # Lire les fichers excel
install.packages("ggplot2")             # Installer le packet ggplot2
install.packages("calibrate")           # Pour ajouter les noms des points dans un scatter plot
install.packages("reshape2")            # Load le packet qui permet de faire le reshaping et le melt:
install.packages("ggpubr")              # ggpubr: 'ggplot2' Based Publication Ready Plots - 
# stat() for the Pearson correlation in the plot


# Loading the required packages : 
library("xlsx")                   
library("ggplot2")                
library(calibrate)                
library(reshape2)
library(ggpubr)                         


library("ggmap")
library("mapproj")

# Removinng the scientific notations
options(scipen=999)

# Cleaning the session form other the objects in the environment
remove(list = ls())
ls()

# setting the working directory
setwd("~/OneDrive/Documents/2_Data_analysis_research/GitHub/Analyse-de-donnees-these-paris-saclay/datasets")


# ------------------ Importing the geo coded data ----
library(readxl)
geodata <- read_excel("geodataled.xlsx")
str(geodata)
attach(geodata)


# plot map at zoom level 9
map_ht9 <- get_map(location = haiti, zoom = 9, scale = 1)
ggmap(map_ht9)

# To stop the overlay of points : in the geom_point() inter the arguments posiion_jitter
ggplot(geodata, aes(x= geodata$Longitude, geodata$Latitude)) + 
        geom_point(position = position_jitter(w = 0.3, h = 0.3)) + 
        xlab("Longitude") + 
        ylab("Latitude") + 
        facet_wrap(.~ City)

ggmap(map_ht9) + 
        geom_point(aes(geodata$Longitude, geodata$Latitude), data = geodata)

# This is to change the map layer types == downloading the map raster
# d) maptype = stamen: terrain from google):
map_ht <- get_map(haiti, zoom = 9, source = "google", maptype = "terrain") 
# e) maptype = roadmap - from google):
map_ht <- get_map(haiti, zoom = 9, source = "google", maptype = "roadmap") 
# c) maptype = stamen: terrain from stamen):
map_ht <- get_map(haiti, zoom = 9, source = "stamen", maptype = "terrain") 
# f) maptype = google: hybrid):
map_ht <- get_map(haiti, zoom = 9, source = "google", maptype = "hybrid")  

# These are the ones with the LED by regions :
# The one with the big dots ...
ggmap(map_ht) + geom_point(aes(geodata$Longitude, geodata$Latitude), 
                            position = position_jitter(w = 0.3, h = 0.3),
                            data = geodata, 
                            alpha = 0.5, 
                            size = 8)+ 
        facet_wrap(.~ City)

# The one with the red points
ggmap(map_ht) + 
        geom_point(aes(geodata$Longitude, geodata$Latitude),
                            position = position_jitter(w = 0.1, h = 0.1),
                            data = geodata,
                   color = "red") + 
        facet_wrap(.~ City)+
        xlab("Longitude") + 
        ylab("Latitude")

# ---- Figure Localisation des projets de DEL par zones d’intervention ----
# The one is with the points and the jitter sparce
ggmap(map_ht) + 
        geom_point(aes(geodata$Longitude, geodata$Latitude),
                   position = position_jitter(w = 0.16, h = 0.05),
                   data = geodata,
                   alpha = 0.5, 
                   size = 3,
                   color = "red")  +
        facet_wrap(.~City) +
        xlab("Longitude") + 
        ylab("Latitude")
