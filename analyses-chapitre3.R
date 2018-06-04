##### Clustering des pays les moins avancés avec les hypothèses du modèle conceptuel (chapitre 2)


# Il existe 3 méthodes de clustering :
#       1. Split into set number of clusters (e.g., kmeans);
#       2. Hierarchical : Start seperate and then combine
#       3. Dividing: Start with a single group and split


# Apres importation de la base de données (Voir les fichiers R codes for data analysis)
# Avec un data set, il faut harmoniser toutes les variables et enlever

library(readxl)         # "Loading" le packet qui permet de lire les fichiers *.xlsx
Data <- read_excel("~/OneDrive/Documents/Data/Cluster_PMA.xlsx")
str(Data)               # Struture de la base de données

# les variables "categorials" i.e. "Pays", doivent etre enlevees;
# On a enlever (annule) "PMA" la variable catégorique dans le data set.
# La variables categorial ne peux pas etre evaluer en tant que distance
# Quand on transforme la variables, on peut alors faire les calculs avec 
# les chiffres
View(Data)
Data.pma = Data
Data.pma$Economy = NULL 

                                # Hiearchical clustering #


# Let's use a hiearchical clustering
# Thus on a besoin d'une mesure de distance
# Basée sur la base de données, nous déterminons dans quelles mesures les variables seraient
# similaires différents de chaque cas.

d <- dist(Data.pma)     # distance entre variables et classes = dist(le data set)
                        # dist est une fonction dans R, qui indique dans quelles mesures chaque points est similaire à tous les autres
                        # ressemble à une matrice de correlation.

# La seconde étape est d'utiliser la matrice de distance comme base du clustering;
clusters = hclust(d)
clusters                # Permet de voir le nombre de cluster existants   
                        # Donne l'arbre hierachique, le dendogram du cluster
# pour faire le dendogram (graph avec les label = label = Data$Economy => la variable qu'on avait enlevée au début)
plot(clusters, labels = Data$Economy)
plot(clusters, labels = Data$Economy, hang = -1) # Pour aligner les labels.

# On peut indiquer le nombre de groupes
# on doit spécifier oubien la "hateure" ou bien le nobmre de groupes retenus;

g3 = cutree(clusters, k = 3) # "g3" = "Groupe 3"
                                # C'est comme si on coupait une branche de l'arbre, du dendogram        
rect.hclust(clusters, k = 3, border = "blue") # On dessine des boites en fonction du nombre de clusters
                                              # ici k = 3, la bordure de la boite = "blue"



# Graphique avec les clusters regroupés dans un grand cercle 
# On utilise un packet qui s'appelle "cluster"
library("cluster", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
require(cluster)
clusplot(Data.pma, km$cluster, color = TRUE, shade = TRUE, lines = 3, labels = 2)


                                ### K-mean cluster analysis     ###
                                ### Kmean algorith              ###


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

Data = read.xlsx("Cluster PMA.xlsx", sheetIndex=1, rowIndex=NULL)
Data.pma = Data
Data.pma$Economy = NULL
distMatrix = dist(Data.pma)
hc = hclust(distMatrix)
ggdendrogram(hc)                # Simple looking dendogram

# Dendogram avec le theme
ggdendrogram(hc, rotate=FALSE, size=4, theme_dendro=FALSE)

