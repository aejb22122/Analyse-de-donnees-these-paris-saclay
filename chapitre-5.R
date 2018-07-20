# ---- Thèse de doctorat Annick Eudes JEAN-BAPTISTE ----
# Codes de réplication des calculs du Chapitre # 5 - Contexte empirique

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

# Removinng the scientific notations
options(scipen=999)

# Cleaning the session form other the objects in the environment
remove(list = ls())
ls()

# setting the working directory
setwd("~/OneDrive/Documents/2_Data_analysis_research/GitHub/Analyse-de-donnees-these-paris-saclay/datasets")

# ---- Graphique # 38 ----
# Figure 38. Transition de la population urbaine/rurale du cas typique
library(readxl)
df <- read_excel("rural_urbain_long.xlsx",
col_types = c("numeric", "numeric", "numeric"))
View(df)
str(df)

ggplot(df, aes(df$Date)) + 
        geom_line(aes(y = df$Urban_population, color = "Population urbaine")) + 
        geom_line(aes(y = df$Rural_population, color = "Population rurale")) +
        scale_color_discrete(name = "Couleur") +
        xlab("Années") +
        ylab("En million d'habitants")


# ---- Graphique # 47 ----
# Figure 47. Évolution des revenus de quelques municipalités et impact 
# combiné de l'élection et des mesures de la loi de finances de 2015


df <- read_excel("Revenus_fiscaux_2011_2016.xlsx")
View(Df)
View(df)

ls()
ggplot(df, aes(df$Date)) + 
        geom_line(aes(y = df$Ouanaminthe, color = "Ouanaminthe")) +
        geom_line(aes(y = df$Caracol, color = "Caracol")) +
        geom_line(aes(y = df$Acul_du_Nord, color = "Acul du Nord")) +
        geom_line(aes(y = df$Carrefour, color = "Carrefour")) +
        geom_line(aes(y = df$Limonade, color = "Limonage")) +
        geom_line(aes(y = df$Cape_Haitian, color = "Cap-Haitien")) +
        geom_line(aes(y = df$Kenscoff, color = "Kenscoff")) +
        geom_line(aes(y = df$Delmas, color = "Delmas")) +
        geom_line(aes(y = df$Saint_Marc, color = "Saint-Marc")) +
        xlab("Années") +
        ylab("En USD")


# ---- Graphique # 50 ----

# Figure 50. Cartographie des projets de développement local à financements mixtes relevés de 2013 à 2016
# Analyses des projets de développement local géo-référencés dans le PMA typique analysé
# Spacial analysis of the LED projects in the communes 
# ploting the geo data 


# Analyses des projets de développement local géo-référencés dans le PMA typique analysé
# Spacial analysis of the LED projects in the communes 

# ------------------ ploting the geo data 

# ------------------ Packages ----
# First we need to install some required packages :
install.packages("ggmap")
install.packages("mapproj")

# ------------------ Importing the geo coded data ----
library(readxl)
geodata <- read_excel("geodataled.xlsx")
View(geodata)

str(geodata)

# Loading the ggplot package and ggmap, if not done allready running
library(ggplot2)
library(ggmap)

attach(geodata)


# ------------------ Ploting the data ----
ggplot(geodata, aes(x= geodata$Longitude, geodata$Latitude)) + geom_point()

# To stop the overlay of points : in the geom_point() inter the arguments posiion_jitter
ggplot(geodata, aes(x= geodata$Longitude, geodata$Latitude)) + geom_point(position = position_jitter(w = 0.3, h = 0.3)) + xlab("Longitude") + ylab("Latitude")

# Just to keep the theme going, we'll just put the points in blue (colour = 'blue', size = 2) in the 
# geom_point() arguments
ggplot(geodata, aes(x= geodata$Longitude, geodata$Latitude)) + geom_point(position = position_jitter(w = 0.3, h = 0.3), colour = 'blue', size = 2) + xlab("Longitude") + ylab("Latitude")

# ------------------ Cartes ----
# Coordonnées de Haiti (centrée sur Hinche), que l'on met dans un vecteur sur r:
haiti <- c(lon = -72.01667, lat = 19.15)
cap_haitien <- c(lon = -72.206768, lat = 19.737036)

# Haiti - centrée sur Hinche :
# Plot map at zoom level 5 - (trop loin cela donne tout le bassin des Caraibes)
map_ht5 <- get_map(location = haiti, zoom = 5, scale = 1)
ggmap(map_ht5)

# plot map at zoom level 9
map_ht9 <- get_map(location = haiti, zoom = 9, scale = 1)
ggmap(map_ht9)


# Haiti -centrée sur Cap Haitien :
map_cap <- get_map(location = cap_haitien, zoom = 9, scale = 1)
ggmap(map_cap)

# We can change the map type by adding : (maptype = "satellite") in the arguments
map_ht99 <- get_map(location = haiti, zoom = 9, scale = 1, maptype = "satellite")
ggmap(map_ht99)

# ------------ Carte centrée sur Hinche ----
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


# ------------ Adding the budgets to the points ----
# Centrée sur Hinche :
# This is the best one :
ggmap(map_ht9) + geom_point(aes(geodata$Longitude, geodata$Latitude, 
                        color = geodata$Budget), 
                        data = geodata, 
                        alpha = 0.5, 
                        size = 8)

# Map avec les financements
# Adding fiscal revenus to the map
ggmap(map_ht9) + 
        geom_point(aes(geodata$Longitude, 
                       geodata$Latitude, 
                       color = geodata$Revenus_t3), 
                   data = geodata, 
                   alpha = 0.3, size = 8)


# Centrée sur cap-Haitien :
ggmap(map_cap) + 
        geom_point(aes(geodata$Longitude, 
                       geodata$Latitude, 
                       color = geodata$Budget), 
                   data = geodata, 
                   alpha = 0.6, size = 6)



# Adding investment budget to the map
ggmap(map_ht9) + 
        geom_point(aes(geodata$Longitude, 
                       geodata$Latitude, 
                       color = geodata$Budget), 
                   data = geodata, 
                   alpha = 0.3, size = 8)


# ---- Different types of maps ----

# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf

# 1e. Adding the maptypes --> get_map :
# 2e. Ploting the get_map in the ggmap()  

### 1e. Adding the maptypes --> get_map :
# a) maptype = toner version - black and white):
map_ht <- get_map(haiti, zoom = 9, source = "stamen", maptype = "toner") # un peu noir et blanc

# b) maptype = stamen: watercolor ):
map_ht <- get_map(haiti, zoom = 9, source = "stamen", maptype = "watercolor")  
#        scale_color_discrete(name = "Budget d'investissement") # un peu noir et blanc

# c) maptype = stamen: terrain from stamen):
map_ht <- get_map(haiti, zoom = 9, source = "stamen", maptype = "terrain")  

# d) maptype = stamen: terrain from google):
map_ht <- get_map(haiti, zoom = 9, source = "google", maptype = "terrain")  

# e) maptype = roadmap - from google):
map_ht <- get_map(haiti, zoom = 9, source = "google", maptype = "roadmap")  

# f) maptype = google: hybrid):
map_ht <- get_map(haiti, zoom = 9, source = "google", maptype = "hybrid")  


### 2e. Ploting the get_map in the ggmap()
ggmap(map_ht) + 
        geom_point(aes(geodata$Longitude, geodata$Latitude, color = geodata$Budget), 
                   data = geodata, alpha = 0.5, size = 10) +
        xlab("Longitude") +
        ylab("Latitude")

# Map avec les financements et le type de carte que l'on veut ... "map_ht"
ggmap(map_ht) + 
        geom_point(aes(geodata$Longitude, geodata$Latitude, color = geodata$Budget), 
                   data = geodata, alpha = 0.5, size = 10) +
        facet_wrap(~ City) +
        xlab("Longitude") +
        ylab("Latitude")

# -------------------- Other types of map -----
# A quick alternative :
qmplot(Longitude, Latitude, data = geodata, geom = "point", color = Budget) + facet_wrap(~ City)

# Heat map :
ggmap(map_ht9, extent = "device") + geom_density2d(data = geodata, aes(x = geodata$Longitude, y = geodata$Latitude), size = 0.5) + stat_density2d(data = geodata, aes(x = geodata$Longitude, y = geodata$Latitude, fill = ..level.., alpha = ..level..), size = 0.01,  bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)

