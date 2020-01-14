# ---- Thèse de doctorat Annick Eudes JEAN-BAPTISTE ----
# Codes de réplication des calculs du Chapitre # 4 - Analyse empirique

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
remove(list=ls())
ls()

# setting the working directory
setwd("~/OneDrive/Documents/2_Data_analysis_research/GitHub/Analyse-de-donnees-these-paris-saclay/datasets")

# ---- Graphique # 31 ----
# Figure 31. Taux d’inscription au secondaire en fonction du revenu par habitant 
#df <- read.csv("EduSecondaireGDP.csv")
library(readxl)
df <- read_excel("education-gdp.xlsx",
col_types = c("text", "numeric", "numeric",
"numeric", "numeric"))

View(df)
str(df)

ggplot(df, aes(x = df$PIB_cont_usd_2015, y = df$Edu_2015)) + 
        geom_point(color = "black") + 
        geom_smooth(method = "auto", se = FALSE) +
        stat_cor(method = "pearson", label.x = 95000, label.y = 110) +
        xlab("PIB par habitant, (USD PPA internationaux constants de 2011") +
        ylab("Inscription à l'école secondaire (pourcentage net)")



# ---- Graphique # 33 ----
# Figure 33. Dendrogramme des critères et classification des PMA

## Classification des pays les moins avancés avec les hypothèses du modèle conceptuel (chapitre 3)

# Il existe 3 méthodes de clustering :
#       1. Split into set number of clusters (e.g., kmeans);
#       2. Hierarchical : Start seperate and then combine
#       3. Dividing: Start with a single group and split

# Apres importation de la base de données
# Avec un data set, il faut harmoniser toutes les variables et enlever
library(readxl)
df <- read_excel("Cluster_PMA.xlsx",
col_types = c("text", "numeric", "numeric",
"numeric", "numeric", "numeric"))

View(df)
str(df)

# les variables "categorials" i.e. "Economy", doivent eêtre enlevées;
# On a enlever (annule) "PMA" la variable catégorique dans le data set.
# La variables categorial ne peux pas etre evaluer en tant que distance
# Quand on transforme la variables, on peut alors faire les calculs avec 
# les chiffres

Data.pma = df
Data.pma$Economy <- NULL 

# Hiearchical clustering
# Let's use a hiearchical clustering
# 1. On a besoin d'une mesure de distance
# Basée sur la base de données, nous déterminons dans quelles mesures les variables seraient
# similaires différents de chaque cas.

d <- dist(Data.pma)     # distance entre variables et classes = dist(le data set)
                        # dist est une fonction dans R, qui indique dans quelles mesures chaque points est similaire à tous les autres
                        # ressemble à une matrice de correlation.

# La seconde étape est d'utiliser la matrice de distance comme base du clustering;
clusters = hclust(d)
clusters                # Permet de voir le nombre de cluster existants   
                        # Donne l'arbre hierachique, le dendogram du cluster

# Pour faire le dendogram (graph avec les label = label = Data$Economy => la variable qu'on avait enlevée au début)
plot(clusters, labels = df$Economy)
plot(clusters, labels = df$Economy, hang = -1) # Pour aligner les labels.

# On peut indiquer le nombre de groupes
# on doit spécifier oubien la "hauteur" ou bien le nombre de groupes retenus;

g3 = cutree(clusters, k = 3) # "g3" = "Groupe 3"
                                # C'est comme si on coupait une branche de l'arbre, du dendogram        
rect.hclust(clusters, k = 3, border = "red") # On dessine des boites en fonction du nombre de clusters
                                              # ici k = 3, la bordure de la boite = "blue"



# Graphique avec les clusters regroupés dans un grand cercle 
# On utilise le packet qui s'appelle "cluster"
library("cluster", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
require(cluster)
clusplot(Data.pma, km$cluster, color = TRUE, shade = TRUE, lines = 3, labels = 2)


                                ### K-mean cluster analysis     ###
                                ### Kmean algorithm              ###


View(Data.pma)                # On vérifie que la variale categorical est effectivement absent
kmeans(Data.pma, 3)           # L'algorithm de kmean demande de specifier le nombre de cluster que l'on veut avoir.

# J'ai spécifié 3 ici apres avoir visualisé le scatter plot, et déduit qu'intuitivement il y a 3 groupements pour les PMA.

# On peut enregistrer le resultat dans un R object
Results <- kmeans(Data.pma, 3)
Results # Le rappel des r´ésultats des calculs par les K-moyennes, nous donne la moyenne des cluster;
        # le vecteur de cluster, et la somme des carrés entre clusters;

# Si on veut savoir quelles variables se trouve dans quel cluter:
table(Data$Economy, Results$cluster)

# pour 2 dimensions, x et y 
# On peut faire un graphique avec les clusters avec des couleurs différentes avec l'arguement col 
# (couleurs egale au resultat du cluster analysis) (col = Results$cluster)

plot(x = Data$X.PIB.ppp, y = Data$APD_2014, xlab = "PIB constant PPP", ylab = "Aide publique au développement recu", main = "APD recu et niveaux de revenus dans les PMA", frame = FALSE, pch = 19, col = Results$cluster)
textxy(Data$X.PIB.ppp, Data$APD_2014, Data$PMA, offset = 0.8, cex = 0.8)

# Pour un graph incluant plus de 3 variables ou dimensions

# Cluster avec le packet ggdendro - plus jolie :
install.packages('ggdendro')
library(ggdendro)

library(readxl)
df <- read_excel("Cluster_PMA.xlsx",
                 col_types = c("text", "numeric", "numeric",
                               "numeric", "numeric", "numeric"))

Data.pma <- df
Data.pma$Economy <- NULL
distMatrix = dist(Data.pma)
hc = hclust(distMatrix)
ggdendrogram(hc)                # Simple looking dendogram

# Dendogram avec le theme
ggdendrogram(hc, rotate=FALSE, size=4, theme_dendro=FALSE)

