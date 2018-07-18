# ---- Thèse de doctorat Annick Eudes JEAN-BAPTISTE ----
# Codes de réplication des calculs du Chapitre # 1 -augmentés d'arguments et d'autres calculs

# ---- Préliminaires ----
# Cleaning the session form other the objects in the environment
remove(list=ls())
ls()

# setting the working directory
setwd("~/OneDrive/Documents/2_Data_analysis_research/GitHub/Analyse-de-donnees-these-paris-saclay/datasets")

# Adding the packages used in this analysis

install.packages("xlsx")                # Lire les fichers excel
install.packages("ggplot2")             # Installer le packet ggplot2
install.packages("calibrate")           # Pour ajouter les noms des points dans un scatter plot
install.packages("reshape2")            # Load le packet qui permet de faire le reshaping et le melt:
install.packages("ggpubr")              # ggpubr: 'ggplot2' Based Publication Ready Plots - 
                                        # stat() for the Pearson correlation in the plor


# Loading the required packages : 
library("xlsx")                   
library("ggplot2")                
library(calibrate)                
library(reshape2)
library(ggpubr)                         

# Removinng the scientific notations
options(scipen=999)

# En francais :
# Sys.setenv(LANG = "fr") 
# Ou en anglais Sys.setenv(LANG = "en")

# ---- Graphique # 3 ----
# Importer le ficher excel contenu dans le repertoire :"dataset"
df <- read_excel("Taux_de_croissance_PD PVD_PMA.xlsx")
str(df)
View(df)

# Figure 3. Taux de croissance du PIB per capita par 
# catégories de revenus de 1971 à 2015 en USD constant en 2010

ggplot(df, aes(df$Date)) + geom_line(aes(y = df$`Pays Developpes`, 
                                           color = "Pays développés")) + 
        geom_line(aes(y = df$PVD, color = "PVD")) + 
        geom_line(aes(y = df$`PVS sans les PMA`, color = "PVD sans les PMA")) + 
        geom_line(aes(y = df$`PMA : Afrique et Haiti`, color = "PMA Afrique et Haiti")) + 
        geom_line(aes(y = df$`PMA : Asie`, color = "PMA d'Asie")) + 
        xlab("Années") + 
        ylab("Croissance du PIB par habitant (% annuel)")

# ---- Graphique # 4 ----
# Figure 4. Contribution du secteur agricole à la valeur ajoutée (en USD constant 2010)
library(readxl)
df <- read_excel("Contribution_VA_PIB.xlsx")
View(df)

# 1er chose à faire, elever la colonne "date" et "reshape" les variables pour les
# mettre dans le format adéquat :
df.pma = df
df.pma$Date <- NULL

# Load le packet qui permet de faire le reshaping et le melt:
library(reshape2)

# Shaping the data
melted_data = melt(df.pma)
View(melted_data)

# Change the names to make it less confusing if necessary
colnames(melted_data) = c("Pays", "va.agr.PIB")

# ploting with ggplot, x = variable and y = value (one can add cathegories)
ggplot(melted_data) + geom_boxplot(aes(x = Pays, y = va.agr.PIB))

# Ajouter des valeurs sur les axes x et y
ggplot(melted_data) +
        geom_boxplot(aes(x = Pays, y = va.agr.PIB)) +
        xlab("Groupes économiques") + 
        ylab("Agriculture, valeur ajoutée (% du PIB)")

# ---- Graphique # 8 ----
# Importer le ficher excel contenu dans le repertoire :"dataset"
library(readxl)
df <- read_excel("Commodity_prices.xlsx",
sheet = "Data")
View(df)

# Figure 8. Évolution du prix de quelques produits de base à forte demande dans les PMA
ggplot(df, aes(df$Annees)) + 
        geom_line(aes(y = df$`Moyenne en pétrole brut ($ / bbl)`, color = "Moyenne en pétrole brut (USD/baril)")) +
        geom_line(aes(y = df$`Maïs - ($ / mt)`, color = "Maïs (USD/tonnes métriques)")) +
        geom_line(aes(y = df$`Sorgho - ($ / mt)`, color = "Sorgo (USD/tonnes métriques)")) +
        geom_line(aes(y = df$`Riz Thai 5% ($ / mt)`, color = "Riz Thai 5% (USD/tonnes métriques)")) +
        geom_line(aes(y = df$`Blé US HRW ($ / mt)`, color = "Blé US (USD/tonnes métriques)")) +
        scale_fill_discrete(name="Experimental\nCondition") +
        xlab("Années") +
        ylab("Prix en USD")

# ---- Graphique # 9 ----
# Figure 9. Solde du commerce de marchandises dans les PMA (1971 à 2016)
#### Figure 9 Situation du commerce externe des PMA
library(readxl)
df <- read_excel("balance_commerciale3.xlsx",
                   col_types = c("numeric", 
                                 "numeric", 
                                 "numeric", 
                                 "numeric"))
str(df)

ggplot(df, aes(df$Annee)) + 
        geom_line(aes(y = df$Afrique_et_Haiti, color = "PMA Afrique + Haiti")) + 
        geom_line(aes(y = df$PMA_Asie, color = "PMA d'Asie")) +
        geom_line(aes(y = df$PMA_iles, color = "PMA îles")) +
        ylab(label="En millions de USD") +  
        xlab("Années")

# ---- Graphique # 11 ----
# Figure 11. Perception de la corruption dans les économies par niveau de revenue
library(readxl)
df <- read_excel("CorruptionIndex_GDP_per_capita.xlsx",
col_types = c("text", "text", "numeric",
"numeric"))
View(df)
str(df)

# Modèle dans le graph avec intervalle de confiances 
# on ajoute un model qui sera dans le graph après
model = lm(df$CPI_2014 ~ df$`GDP per capita (constant 2005 US$)_2014` + df$Categorie)

# abline ajoute le model au graphique
abline(model, lwd = 2)

df.m <- df
# Basic scater plot avec GGPLOT2, on prepare le graph, ensuite on ajoute le layer
# le layer c'est le ...+ geom_point()
p1 <- ggplot(df.m, aes(x = df.m$`GDP per capita (constant 2005 US$)_2014`, y = df.m$CPI_2014, label=df.m$Pays)) +
        geom_point(color="blue") +
        geom_smooth(method = "lm", se = TRUE) +
        geom_text(check_overlap = TRUE) +
        geom_point(aes(color=factor(df.m$Categorie))) +
        #scale_fill_discrete(name="Niveau de revenu") +          # If you wish to modify the legend.
        xlab("PIB per Capita 2014 (Prix en USD constant 2005)") +
        ylab("Indice de perception de la corruption 2014") +
        scale_color_discrete(name = "Niveau de revenu") +
        stat_cor(method = "pearson", label.x = 7500, label.y = 75)  # Add correlation coefficients (label.x and lable.y are the positions in the grid of the plot)
        #theme(legend.position="none")                           # If you wish to remove the legend
        #aes(fill = df.m$Categorie)
p1

# ---- Graphique # 12 ----
# Figure 12. Investissements Directs Etrangers rentrants 
# dans quelques zones économiques (1971 à 2016)

library(readxl)
df <- read_excel("IDE_1971_2016.xlsx",
col_types = c("numeric", "numeric", "numeric",
"numeric", "numeric"))
View(df)

df$Date <- NULL

df.m <- df
melted_data <- melt(df.m)
View(melted_data)

# Change the names to make it less confusing if necessary
colnames(melted_data) = c("Pays", "IDE")

ggplot(melted_data) + geom_boxplot(aes(x = Pays, y = IDE)) + 
        xlab("Groupe de pays") + 
        ylab("IDE (en milliards de USD courants")


# ---- Graphique # 13 ----
# Figure 13. Incidences de la violence sur les coûts des entreprises
# par niveaux de développement économique (2015)


# ---- Graphique # 14 ----
# Figure 14. Crédit intérieur au secteur privé par niveaux de revenus en 2015 

library(readxl)
df <- read_excel("Credit_PIB.xlsx",
col_types = c("text", "numeric", "numeric",
"numeric", "text"))
str(df)

ggplot(df, aes(x = df$`PIB par habitant, ($ PPA internationaux courants)_2015` , y = df$`Crédit intérieur fourni au secteur privé (% du PIB)_2015` )) + 
        geom_point(aes(color = factor(df$Categories))) + 
        #geom_smooth(method = "lm", se = TRUE) +
        scale_color_discrete(name = "Groupe de pays") +
        stat_cor(method = "pearson", label.x = 150000, label.y = 225) +
        xlab("PIB par habitant, (USD internationaux courants en PPA)") + 
        ylab("Crédit intérieur fourni au secteur privé (en pourcentage du PIB)")

# ---- Graphique # 17 ----
# Figure 17. Ressources internes par groupes de pays

# ---- Graphique # 18 ----
# Figure 18. Indicateurs de gouvernance des économies en 2015, par niveaux de revenus
library(readxl)
df <- read_excel("WGI_new.xlsx", col_types = c("text",
"text", "text", "numeric"))
str(df)
colnames(df) = c("Pays", "Niveau de revenu", "WGI", "Valeur")
View(df)        # To verify that all is ok
ggplot(df) + 
        geom_boxplot(aes(x = df$WGI, df$Valeur)) + 
        facet_grid(.~df$`Niveau de revenu`) + 
        xlab("Indicateurs de gouvernance") + 
        ylab("Valeur") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, nrow(4)))


# ---- Graphique # 19 ----
# Figure 19. Dépenses publiques en fonction des niveaux de revenus en 2011 
# Figure 21 Dépenses publiques en fonction des niveaux de revenus

library(readxl)
df <- read_excel("Dep_en % PIB_PIB_per_capita2011.xlsx",
col_types = c("text", "text", "numeric",
"numeric", "numeric", "numeric"))
View(df)
str(df)

ggplot(df, aes(x = df$log_PIB_per_capita, y = df$`log_dep_pub_en%PIB`)) + 
        #geom_point(aes(color = df$`Niveau de revenus`)) + 
        scale_color_discrete(name = "Groupe de pays") +
        geom_smooth(method = "lm", se = T) +
        stat_cor(method = "pearson", label.x = 3.6, label.y = 1.7) +
        xlab("PIB per capita (USD constant 2005) (log)") + 
        ylab("Dépenses publiques en pourcentage du PIB (log)") +
        geom_jitter(aes(color = df$`Niveau de revenus`), position=position_jitter(width=.1, height=0))


# ---- Graphique # 26 ----
# Figure 26. L’aide publique au développement et l’envoie de fonds des migrants dans les Pays en Voie 
# de Développement et dans les Pays les Moins Avancés (en USD courants, 2014)
library(readxl)
df <- read_excel("APD_Transferts_migrants.xlsx",
col_types = c("text", "text", "numeric",
"numeric"))
str(df)

df.pma = df
melted_data = melt(df.pma)
View(melted_data)

colnames(melted_data) = c("Pays", "Niveau de revenu", "Sources", "Montant")
View(melted_data)

ggplot(melted_data) + 
        geom_boxplot(aes(x = melted_data$`Niveau de revenu`, y = melted_data$Montant)) +
        facet_grid(.~melted_data$Sources) + 
        xlab("Niveaux de revenus") + 
        ylab("En millions de USD à prix courants et à PPA")

