
### Exploring and Visualizing Data ###

### Data Analysis and Visualization Using R ###

## Getting started ###
# Get help on R
help.start()

# if you have questions about a particular R function, 
# you can access its documentation with a "?"
# Question mark followed by the function name: ?function_name_here
# Exemple : 
?plot()

# Enlever ou ajouter des packages: 
install.packages("nom du pakage")
library("nom du pakage") # Call the package que l'on veut utiliser

# Enlever toutes les objects de R
ls() # Lister toutes les objects
rm(list=ls())

# LOADING THE DATA, LOADING BASIC PACKAGES FOR DATA ANALYSIS

# En premier lieu :
# Set working directory
# load le packet qui permet de lire les fichier Excel
# Utiliser ggplot2 
# Lectures de fichiers excel sur R

#### Les codes et les packets avant de commencer 
# For each graphs, please refer to the thesis source.

setwd("~/OneDrive/Documents/Data")      # set working directory
install.packages("xlsx")                # Lire les fichers excel "
library("xlsx")                         # load le library

install.packages("ggplot2")             # Installer le packet ggplot2
library("ggplot2")                      # load le library ggplot2

install.packages("calibrate")           # Pour ajouter les noms des points dans un scatter plot
library(calibrate)                      # load the calibrate package

# Load le packet qui permet de faire le reshaping et le melt:
install.packages("reshape2")
library(reshape2)

# Remove the scientific notations
options(scipen=999)

# En francais :
Sys.setenv(LANG = "fr")


# Importer des fichiers sur Excel (1er facon de faire)
# sheetIndex=1 nous dis quelle feuille on veut lire dans le fichier xlsx
library("xlsx")

Data = read.xlsx("filename", sheetIndex=1, rowIndex=NULL)

# Importer des fichier avec Excel, (2e facon de faire)
library(readxl)
Cluster_PMA <- read_excel("~/OneDrive/Documents/Data/Cluster PMA.xlsx")
View(Cluster_PMA)


# Import *.Rdata files :


# Voir le fichier qui a ete loaded
View("le data")

# Exploratory data analysis
# Simple analysis to get a feel of the data
# Resumé des données 
# 5 number summary
summary(data$variable)
summary(data) # Gives the max, the mean, 1rst qu, median, 3rd qu and the max

                        #       GRAPHS        #
                        
                        #       BASIC PLOTS   #
                        
                        #       BOX PLOTS     #

# Simple box plots avec labels et titres :
# boxplot(Data$variable1, Data$variable2, Data$variable10, main = "le titre du graph", ylab = "titre de l'axe y", xlab = "titre de l'axe x", frame = FALSE))
# frame = FALSE c'est pour enlever l'encadrement du graphique
# Avec l'argument "names" on a les noms des categories sous les boites.
# Exemples
boxplot(Data$PVD, Data$Monde_arabe, Data$Asie_est_pacifique, Data$Europe_asie_centrale, Data$Amerique_latine_Caraibe, Data$Aise_Sud, Data$Afrique.Sub.Saharienne, Data$PMA, Data$Petites_iles_en_developpement, Data$OCDE, ylab = "HDI", xlab = "Groupes", frame = FALSE)

# Meme chose, sauf que dans ce data.frame, il n'y avait pas de "date"
boxplot(Data2, ylab = "HDI", xlab = "Groupes économiques", frame = FALSE, names)

# On peut faire des boxplots (voir plus bas pour les details)
hist(data$Government.Effectiveness_2014)
rug(data$Government.Effectiveness_2014) # The rug gives the points in your histogram

# One dimentional analysis
# On peut faire un graph signalant deux niveaux (i.e. revenus faibles, revenus moyens)
# boxplot(variable ~ variable parlequel on distigue les deux series), boxplot(x1 ~ x2)
# Le "~" indique selon quelles modalites on veut distribuer le graphique.

boxplot(Data$Cadre.legal_2014 ~ Data$Cathegories)

# Ajoutons quelques elements au graphique de base

boxplot(Data$Cadre.legal_2014 ~ Data$Cathegories) # ylab = "label y", xlabel = "label x", frame = FALSE -> sans le cadre du graph, main = "Titre du graphique"
boxplot(Data$Cadre.legal_2014 ~ Data$Cathegories, ylab = "la perception de la confiance dans le respectent des règles sociétales", xlab = "Niveaux de revenus", frame = FALSE, main = "Confiance dans le cadre légal en 2014 [-2.5;2.5.]")

# Plusieurs boxplots en fonction de 2 categories (i.e. revenu faible, revenu eleve)

# On peut faire autant avec d'autres type de plots, faut juste changer.
# la syntaxe "boxplot" change seulement.
# mar = marge, oma = other margin qui ici, est plus grand que celui par defaut :

par(mfrow = c(1, 4), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(Data, {
        boxplot(Data$Cadre.legal_2014 ~ Data$Niveau.de.revenus, main = "Cadre legal")
        boxplot(Data$Control.de.Corruption_2014 ~ Data$Niveau.de.revenus, main = "Control de la corruption")
        boxplot(Data$Efficacite.gouvernementale_2014 ~ Data$Niveau.de.revenus, main = "Efficacité governementale")
        boxplot(Data$Qualite.de.la.regulation_2014 ~ Data$Niveau.de.revenus, main = "Qualite de la regulation")
        mtext ("WGI par niveaux de revenus", outer = TRUE)
}) 
# Le probleme c'est qu'il faut arreter la fonction apres.

# Violin plots (Boxplots avec un nudge) l'argument notch=TRUE a ete ajoutee
boxplot(Data, notch=TRUE,
        +         col=(c("lightblue","lightblue1", "lightblue2", "lightblue3")),
        +         main="Balance commerciale des PVD et PMA (1971 à 2015)", frame = FALSE)


#       SCATTER PLOTS   #


# Scatter plot matrix
# plot the data un peu bouilli vidé de toutes les variables. Utile si l'on veut voir toutes les
# combinaisons de relations entre les variables.
plot(data)

# Simple scatter plots
plot(x = variable1, y = variable2, xlab = "nom axe x", ylab = "nom axe y", main = "titre du graph", frame = FALSE)
plot(x = Gov_GDP$GDP.per.capita..PPP..constant.2011.international....2013, y = Gov_GDP$Dépenses....du.PIB..2013, xlab = "PIB par habitant", ylab = "Dépenses publiques en pourcentage du PIB", main = "Dépenses publiques des PMA par niveau de revenus", frame = FALSE)

# Labels in plots #

# Pour ajouter les noms des points dans un scatter plot :
# if necessary, install the calibrate package
install.packages("calibrate")

#load the calibrate package
library(calibrate)

#use the textxy() function to add labels to the preexisting plot's points
#add labels for the total enrollment
textxy(variable1$data, variable2$data, variable_nom$data)

#generate a plot using the plot(x,y) function
#plot year on the x axis and unemployment rate on the y axis
plot(enrollmentData$YEAR, enrollmentData$UNEM)


# Plot simple
plot(x = Data$X.PIB.ppp, y = Data$APD_2014, xlab = "PIB constant PPP", ylab = "Aide publique au développement recu", main = "APD recu et niveaux de revenus dans les PMA", frame = FALSE, pch = 20, col = "blue")
# On peut ajouter des valeurs aux points (des labels i.e. les pays).
library(calibrate) # Le packet qui permet de mettre des noms dans les points.

# Avec les donnees de Data et le plot fait ceci ... voila ce que l'on dit a R.
with(Data, plot(x = Data$X.PIB.ppp, y = Data$APD_2014, xlab = "PIB constant PPP", ylab = "Aide publique au développement recu", main = "APD recu et niveaux de revenus dans les PMA", frame = FALSE, pch = 20, col = "blue"))
textxy(Data$X.PIB.ppp, Data$APD_2014, Data$PMA, offset = 1.2) # offset = distance du label par rapport au point.

# pch = 20, change le symbole des points en open circle a un solid cercle
with(Dt_dep_pib_22, plot(x = Dt_dep_pib_22$log_PIB_per_capita, y = Dt_dep_pib_22$log_dep_pub_en.PIB, xlab = "PIB per capita constant en PPP (en log)", ylab = "Dépenses publiques en % du PIB (en log)", main = "Dépenses des gouvernements en fonction des niveaux de revenus", frame.plot = FALSE, col = "blue", pch = 20))
textxy(Dt_dep_pib_22$log_PIB_per_capita, Dt_dep_pib_22$log_dep_pub_en.PIB, Dt_dep_pib_22$Pays, offset = 1.2)


# if necessary, install the calibrate package
install.packages("calibrate")

#load the calibrate package
library(calibrate)

# use the textxy() function to add labels to the preexisting plot's points
# add labels for the total enrollment
textxy(enrollmentData$YEAR, enrollmentData$UNEM, enrollmentData$ROLL)

# Modèle dans le graph avec intervalle de confiances #

# on ajoute un model qui sera dans le graph apres
model = lm(Dt_dep_pib_22$log_dep_pub_en.PIB ~ Dt_dep_pib_22$log_PIB_per_capita, Dt_dep_pib_22)

# abline ajoute le model au graphique
abline(model, lwd = 2)


### SCATTER PLOT ET SCATTER PLOT AVEC INTERVALLE DE CONFIANCE

Data = read.csv("IDE_PIB_const.csv")
str(Data)

# Data = le data base
Dt <- Data
# Basic scater plot avec GGPLOT2, on prepare le graph, ensuite on ajoute le layer
# le layer c'est le ...+ geom_point()

p1 <- ggplot(Dt, aes(x = PIB_const_ppp, y = IDE_Entrants_2014))
# Print plot with default points
p1 + geom_point()

# Add lines to scatterplot
p2 = p1 + geom_point(color="blue") + geom_smooth(method = "lm", se = TRUE)  # add regression line
p2 + geom_point()
p2 + geom_point(size = 3)               # On peut changer la taille des points

# On peut mettre les labels pour les points dans le scatter plot
p2 = ggplot(Data, aes(x = PIB_const_ppp, y = IDE_Entrants_2014, label=PMA))
+     geom_point() + geom_text(check_overlap = TRUE) # Check overlap, pour empecher que les points s'entre chevauchent.
p3 = p2 + geom_smooth(method = "lm", se = TRUE) # se = FALSE, then there is no interval de confiance
p3


# Scatter plot avec différenciation des points selon leurs facteurs
# Basic scatterplot
p1 <- ggplot(Data, aes(x = GDP.per.capita..constant.2005.US.._2014, y = CPI_2014))
p1 + geom_point()
p2 = p1 + geom_point(aes(color=factor(Categorie)))
p2


# Basic scatterplot - gros points et couleurs differents par categories
p1 <- ggplot(Data, aes(x = GDP.per.capita..constant.2005.US.._2014, y = CPI_2014, label = Pays)) + geom_point() + geom_text(check_overlap = TRUE)
p1
# Basic scatterplot
p1 <- ggplot(Data, aes(x = GDP.per.capita..constant.2005.US.._2014, y = CPI_2014, label = Pays))+ geom_point(size = 4) + geom_point() + geom_text(check_overlap = TRUE)
p2 = p1 + geom_point(aes(color=factor(Categorie))) # Set the points relative to there categories...i.e. revenu, male/female, PMA/PVD
p2


### BOXPLOT AVEC GGPLOT2 ###

# Là, je l'ai fait avec des catégories comme facteurs
p <- ggplot(Data, aes(factor(Categorie), CPI_2014))
p + geom_boxplot()


#### Box plot avec des facteurs, installer le packet reshape2
# If the data is not in the right format, melt it
### 1er simple Boxplot with ggplot 2:

# 1er chose à faire, elever la colonne "date" et "reshape" les variables pour les
# mettre dans le format adéquat :
Data.pma = Data
Data.pma$Date = NULL

# Load le packet qui permet de faire le reshaping et le melt:
install.packages("reshape2")
library(reshape2)

# Shaping the data
melted_data = melt(Data.pma)
View(melted_data)

# Change the names to make it less confusing if necessary
colnames(melted_data) = c("Pays", "va.agr.PIB")

# ploting with ggplot, x = variable and y = value (one can add cathegories)
ggplot(melted_data) + geom_boxplot(aes(x = Pays, y = va.agr.PIB))


# Ajouter des valeurs sur les axes x et y
ggplot(melted_data) + geom_boxplot(aes(x = Pays, y = va.agr.PIB)) + xlab("Groupes economiques") + ylab("Agriculture, valeur ajoutée (% du PIB)")


#### 2e facon de faire avec les catégories ; melt the data, with the reshape funtion :
# load the reshape library
library(reshape2)

# Melt le data to rearange/reshape the data :
mm = melt(Data, id=c('Pays','Niveau.de.revenus')) 
# Boxplot en fonction des revenus pour chaque variables
ggplot(mm)+geom_boxplot(aes(x=paste(variable,Niveau.de.revenus,sep="_"), y=value))
# Boxplot en fonction des revenus pour chaque variables mais goupées en fonction du facteur
ggplot(mm)+geom_boxplot(aes(x=variable, y=value)) + facet_grid(.~Niveau.de.revenus)


# Plot par panels 

library(readxl)
data1 <- read_excel("~/OneDrive/Documents/Data/IDE_categies.xlsx")
str(data1)

sp <- ggplot(data1, aes(GDP_current_US_2014, FDI_current_US_2014)) + geom_point(shape=1)
sp
sp + facet_grid(categorie ~ .) # Facets horizontal
sp + facet_grid(. ~ categorie) # Facets vertical
sp2 <- ggplot(data1, aes(GDP_current_US_2015, FDI_current_US_2015)) + geom_point(shape=1)
sp2+ facet_grid(. ~ categorie)



# Data visualisation avec qplot et ggplot 2
# Avec les examples data set
# Package MASS (for Modern Applied Statistics with S) into memory. 
library(MASS)
data("mpg")

str(mpg)


## BASICS DE GGPLOT2

# Premiere graph avec ggplot2
library(ggplot2)

# Simple scatter plots

# qplot(x, y, data = data set)
qplot(displ, hwy, data = mpg)


# Faire un scatter plot et ajouter le nom des points:
qplot(x = dep_pib$PIB.per.capita..constant.2005.US.._2011, y = dep_pib$Depenses.publiques.en....of.GDP._2011, data = dep_pib, color = dep_pib$Niveau.de.revenus, xlab="PIB per capita PPP2005", ylab = "Depenses publiques en %PIB", label = dep_pib$Pays)


# Storer le plot dans un object, remarque que le label est la à la fin de la formule:
p = qplot(x = dep_pib$PIB.per.capita..constant.2005.US.._2011, y = dep_pib$Depenses.publiques.en....of.GDP._2011, data = dep_pib, color = dep_pib$Niveau.de.revenus, xlab="PIB per capita PPP2005", ylab = "Depenses publiques en %PIB", label = dep_pib$Pays)
p + geom_text(check_overlap = TRUE)   # geom_text(check_overlap = TRUE) c'est pour que les noms ne soient pas dans les points entre-mellés
p + geom_text(size = 10)              # Changer la taille du texte si necessaire, mais le overlap va partir.


# On peut specifier selon des categories
# Les legendes sont fait automatiquement : qplot (x, y, data = Dt, color = "la catégorie choisie")
qplot(displ, hwy, data = mpg, color = drv)


# On peut ajouter un "smoother" => low S qui
# smooths les donnees et donne l'intervalle de confiance (geom = trend)
# On peut specifier selon des categories
qplot(displ, hwy, data = mpg, geom = c("point", "smooth")) # Avec qplot

# Avec ggplot
ggplot(data, aes(x, y)) + geom_point() 
ggplot(data, aes(x, y)) + geom_point() + geom_smooth() # smooth avec intervalle de confiance
ggplot(data, aes(x, y)) + geom_point() + geom_smooth(method = lm) # si l'on desire que la pense soit de type y = ax+b

ggplot(data, aes(x, y)) + geom_point() + geom_smooth(method = lm) + facet_grid(.~ categorie) # Pour les facets, voir plus bas l'explication

# Histogrammes avec des catégories différentes
qplot(hwy, data = mpg, fill = drv)


# Hystogrammes avec des planels et facets

# (Par exemple facteurs : "Male" d'un cote, "Female" d'un autre ou facreurs revenus : "Revenus faibles", "Revenus eleves" d'un autre coté)
# Facets (panels) : subset of your variables indicated by the factor variable;
# Les "facets", c'est le "Titre" de chaque pannels; autant de "facteurs", autants de facets
# On fait ce genre de graphs, si on ne veut pas mettre trop de points distingués 
# par des couleurs ... ce qui risque d'etre illisible
# qplot(x, y, data = Dt, facets = .~ la catégorie)
qplot(displ, hwy, data = mpg, facets = .~ drv)


# La variable spécifiée a droite sert de colonne ou de ligne pour les facetes
# C'est la meme chose, mais contrerement au graph precédent, hwy est avant
qplot(hwy, data = mpg, facets = drv ~., binwidth = 2)


# Multiple lines dans un graphique

# MULTIPLE LINE GRAPHS 
# Il faut utiliser les graphs un à un:

ggplot(test_data, aes(date)) + 
        geom_line(aes(y = var0, color = "var0")) + 
        geom_line(aes(y = var1, color = "var1")) + 
        
        # Exemple 
        ggplot(Data, aes(Data$Date)) + 
        geom_line(aes(y = Regions.plus.developpees, color = "Pays plus developpes")) + 
        geom_line(aes(y = Pays.Moins.Avances, color = "PMA")) + 
        geom_line(aes(y = Pays.à.revenu.élevé, color = "Pays à revenu élevé")) + 
        geom_line(aes(y = Pays.à.revenu.moyen.supérieur, color = "Pays à revenu moyen supérieur")) + 
        geom_line(aes(y = Pays.plus.faibles.à.revenu.intermédiaire, color = "Pays plus faibles à revenu intermédiaire")) + 
        geom_line(aes(y = Pays.à.faible.revenu , color = "Pays à faible revenu"))

### Graphiques de la thèse

#### Graphique # 3 de la thèse
ggplot(dt, aes(dt$Annees)) + geom_line(aes(y = Pays_Developpes, color = "Pays développés")) + geom_line(aes(y = PVD, color = "PVD")) + geom_line(aes(y = PVD_sans_les_PMA, color = "PVD sans les PMA")) + geom_line(aes(y = PMA_Afrique_Haiti, color = "PMA Afrique et Haiti")) + geom_line(aes(y = PMA_Asie, color = "PMA d'Asie")) + xlab("Années") + ylab("Croissance du PIB par habitant (% annuel)")

#### Graphique # 8 de la thèse
library(readxl)
Data <- read_excel("~/OneDrive/Documents/Data/prix_produits_de_base.xlsx", 
                                    col_types = c("numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric"))
str(Data)
ggplot(data = Data, aes(Data$Annees)) + geom_line(aes(y = Data$`Crude-oil-average($/bbl)`, color = "Crude-oil-average($/bbl)")) + geom_line(aes(y = Data$`Maize-($/mt)`, color = "Maize-($/mt)")) + geom_line(aes(y = Data$`Sorghum-($/mt)`, color = "Sorghum-($/mt)")) + geom_line(aes(y = Data$`Rice_Thai_5%_($/mt)`, color = "Rice_Thai_5%_($/mt)")) + geom_line(aes(y = Data$`Wheat_US_HRW-($/mt)`, color = "Wheat_US_HRW-($/mt)")) + xlab("Années") + ylab("Prix en dollars US")

#### Figure 9 Situation du commerce externe des PMA
library(readxl)
Data <- read_excel("~/OneDrive/Documents/Data/balance_commerciale3.xlsx",
col_types = c("numeric", "numeric", "numeric",
"numeric"))
str(Data)

ggplot(Data, aes(Data$Date)) + geom_line(aes(y = PVD_sans_les_PMA, color = "PVD sans les PMA")) + geom_line(aes(y = PMA, color = "Pays Moins Avancés")) + geom_line(aes(y = PMA_Afrique_Haiti, color = "PMA Afrique + Haiti")) + geom_line(aes(y = PMA_Asie, color = "PMA d'Asie")) + ylab(label="Balance commerciale") +  xlab("Date")


#### Figure 12 : IDE
Data <- read_excel("~/OneDrive/Documents/Data/IDE_GDP.xlsx", col_types = c("numeric", "numeric", "numeric","numeric", "numeric"))
str(Data)
melted_data = melt(Data.ide)
Data.pma = Data
melted_data = melt(Data.pma)
View(melted_data)
# Change the names to make it less confusing if necessary
colnames(melted_data) = c("Pays", "IDE")
ggplot(melted_data) + geom_boxplot(aes(x = Pays, y = IDE))
ggplot(melted_data) + geom_boxplot(aes(x = Pays, y = IDE)) + xlab("Groupe de pays") + ylab("En dollars US courants")


# Figure 14 Ressources financières fournies au secteur privé 
Data <- read_excel("~/OneDrive/Documents/Data/Credit_PIB.xlsx", col_types = c("text", "numeric", "numeric","numeric", "text"))
str(Data)
ggplot(Data, aes(x = Data$PIB_par_habitant, y = Data$Credit_int)) + geom_point(aes(color = factor(Data$Categories))) + xlab("PIB par habitant, ($ PPA internationaux courants)") + ylab("Crédit intérieur fourni au secteur privé (% du PIB)")

# Figure 21 Dépenses publiques en fonction des niveaux de revenus
library(readxl)
Data <- read_excel("~/OneDrive/Documents/Data/Dep_en % PIB_PIB_per_capita2011.xlsx",
col_types = c("text", "text", "numeric","numeric", "numeric", "numeric"))
View(Data)
str(Data)
ggplot(Data, aes(x = Data$log_PIB_per_capita, y = Data$`log_dep_pub_en%PIB`)) + geom_point(aes(color = Data$`Niveau de revenus`)) + xlab("PIB per capita (constant 2005 US$) (en log)") + ylab("Depenses publiques en (% of GDP) (en log)")


#### Figure remittances aide publique au developpement
Data <- read_excel("~/OneDrive/Documents/Data/APD_Transferts_migrants.xlsx", col_types = c("text", "text", "numeric", "numeric"))
str(Data)
Data.pma = Data
melted_data = melt(Data.pma)
View(melted_data)
colnames(melted_data) = c("Pays", "Niveau de revenu", "Sources", "Montant")
View(melted_data)
ggplot(melted_data) + geom_boxplot(aes(x = melted_data$Sources, y = melted_data$Montant)) + facet_grid(.~melted_data$`Niveau de revenu`) + xlab("Remittances recus") + ylab("En dollars US courants")

### Graphique population rural urbaine
library(readxl)
Data <- read_excel("~/OneDrive/Documents/Data/rural_urbain_long.xlsx",
col_types = c("numeric", "numeric", "numeric"))
View(Data)
str(Data)
ggplot(data = Data, aes(Data$Date)) + geom_line(aes(y = Data$Urban_population, color = "Population urbaine")) + geom_line(aes(y = Data$Rural_population, color = "Population rurale")) + xlab("Années") + ylab("En million")

# Figure 37 APD et PIB ~ kmoyenne
library(readxl)
Data <- read_excel("~/OneDrive/Documents/Data/PIB_const vs AID.xlsx", col_types = c("text", "numeric", "numeric"))
str(Data)
ggplot(Data, aes(x = Data$`PIB ppp`, y = Data$APD_2014, label=Data$PMA)) + geom_point() + geom_text(aes(label=PMA), check_overlap = TRUE) + stat_ellipse()
ggplot(Data, aes(x = Data$`PIB ppp`, y = Data$APD_2014, label=Data$PMA)) + geom_point() + geom_text(aes(label=PMA), check_overlap = TRUE) + stat_ellipse() + xlab("PIB par habitant, (PPA international constant)") + ylab("Aide Publique au Développement (en dollars us)")


### Fig12 IDE entrants
Data <- read_excel("~/OneDrive/Documents/Data/IDE2.xlsx",
col_types = c("blank", "numeric", "numeric",
"numeric", "numeric"))
View(Data)
melted_data = melt(Data)
View(Data)
View(melted_data)
colnames(melted_data) = c("Agregats", "IDE")
ggplot(melted_data) + geom_boxplot(aes(x = Agregats, y = IDE)) + xlab("Groupe de pays") + ylab("Investissements étrangers directs
(en milliards de USD courants)
")




# Graphique revenu municialites
Data <- read_excel("~/OneDrive/Documents/Data/Revenus_fiscaux_2011_2016.xlsx",
col_types = c("numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric"))
str(Data)
ggplot(data = Data, aes(Data$Date)) + geom_line(aes(y = Acul_du_Nord, color = "Acul du Nord")) + geom_line(aes(y = Caracol, color = "Caracol")) + geom_line(aes(y = Limonade, color ="Limonade")) + geom_line(aes(y = Ouanaminthe, color ="Ouanaminthe")) + geom_vline(xintercept = 2015) + xlab("Années") + ylab("En dollars US")



# On peut modifier le graph ... placer la legende en bas: si p=le graph en entier
p + theme(legend.position="bottom")

# On peut changer la taille de la légende 
p + theme(legend.text = element_text(size = 20))


# GGPLOT SCATTER PLOT AVEC DEUX TRACES DE L'INTERVALLE DE CONFIANCE

ggplot(Data, aes(x = Data$log_PIB_per_capita, y = Data$`log_dep_pub_en%PIB`, color = Data$`Niveau de revenus`)) + geom_point() + geom_smooth(method = lm)

#### HISTOGRAMS WITH GGPLOT

library(readxl)
Data <- read_excel("~/OneDrive/Documents/Data/Remittence_APD.xlsx")
View(Data)
ggplot(Data, aes(x = Data$`Remittances recus 2014`)) + geom_histogram()
# Use `stat_bin()` using `bins = 30`. Pick better value with `binwidth`. que l'on peut modifier
# geom_histogram(bins = 15) ou geom_histogram(bins = 10) etc.
ggplot(Data, aes(x = Data$`Remittances recus 2014`)) + geom_histogram(bins = 30)

## Facets in hystogram divided (~) by a certain category : (~niveau) 
ggplot(Data, aes(x = Data$`Remittances recus 2014`)) + geom_histogram(bins = 30) + facet_wrap(~ Data$Niveau)

# Fill atributes dans les donnees ; c'est comme si on avait deux histogrammes superposes :
# fait dans estetics : aes(data, variable, et fill = categorie choisie)
ggplot(Data, aes(x = Data$`APD nette recue 2014`, fill = Niveau)) + geom_histogram(bins = 30)


### Fig 18
library(readxl)
Data <- read_excel("~/OneDrive/Documents/Data/WGI_new.xlsx",
col_types = c("blank", "text", "text",
"numeric"))
View(Data)
colnames(Data) = c("Niveau de revenu", "WGI", "Valeur")
colnames(Data) = c("Niveau de revenu", "WGI", "Valeur")
View(Data)
ggplot(Data) + geom_boxplot(aes(x = Data$WGI, Data$Valeur)) + facet_grid(.~Data$`Niveau de revenu`) + xlab("Indicateurs de gouvernance") + ylab("Valeur") + theme(axis.text.x = element_text(angle = 90, hjust = 1, nrow(4)))

