# Importation
wine <- read.csv("C:/Users/DELL/Documents/winequality-red.csv",
                
                 sep = ";")
install.packages(c("dplyr", "tidyr",
                   "ggplot2", "reshape2", "plotly"))

# Aperçu
head(wine)
str(wine)
summary(wine)
############################################## 
# 2. NETTOYAGE & MANIPULATIONS DE BASE
##############################################
# Vérifier les valeurs manquantes

colSums(is.na(wine))

# Exemple : remplacer NA par la moyenne dans une colonne numérique
wine$alcohol[is.na(wine$alcohol)] <- mean(wine$alcohol, na.rm = TRUE)
############################################## 
# 3. FILTRAGE, RECODAGE ET AJOUT DE COLONNES
##############################################
# Filtrer : garder seulement les vins avec acidité fixe > 7
wine_filtre <- subset(wine, fixed.acidity > 7)
wine_filtre
# Recode : créer une variable catégorielle "qualite_cat" # qualité 0–4 : faible, 5–6 : moyenne, 7–10 : haute
wine$qualite_cat <- ifelse(wine$quality <= 4,
                           "faible", ifelse(wine$quality <= 6, "moyenne", "haute"))
head(wine$qualite_cat)
wine$qualite_cat
# Nouvelle colonne dérivée : rapport sucre / alcool
wine$ratio_sucre_alcool <- wine$residual.sugar / wine$alcohol
head(wine$ratio_sucre_alcool)
wine$type_vin <- ifelse(wine$ratio_sucre_alcool < 0.5, "sec", "doux")
head(wine$type_vin)
#########score chimique ############
score_chimique <- function(df) {
  return(rowMeans(df[, c("fixed.acidity", "volatile.acidity", "citric.acid")]))
}
wine$score <- score_chimique(wine)
head(score_chimique(wine))
##############################################
# 4. STRUCTURES CONDITIONNELLES (IF / ELSE) 
##############################################
# Exemple simple : message selon la qualité moyenne du dataset
moyenne_qualite <- mean(wine$quality)
if (moyenne_qualite > 6) {
  print("La qualité moyenne est élevée.")
} else {
  print("La qualité moyenne est normale ou faible.")
}
##############################################
# 5. BOUCLE FOR : CRÉER UNE PETITE STAT POUR CHAQUE VARIABLE
##############################################
# Afficher la moyenne de quelques colonnes numériques

colonnes <- c("fixed.acidity", "volatile.acidity", "alcohol")
for (col in colonnes) {
  cat("Moyenne de", col, ":", mean(wine[[col]]), "\n")
}
# Fonction qui calcule acidité totale 
wine$acidite_totale <- wine$fixed.acidity + wine$volatile.acidity
##############################################
# 6. FONCTION PERSONNALISÉE + APPLICATION
##############################################
# Fonction qui calcule l'écart inter-quartile (IQR)
ecart_quartiles <- function(x) {
  return(quantile(x, 0.75) - quantile(x, 0.25))
}
ecart_quartiles(wine$alcohol)
IQR(wine$alcohol)

###### Méthode lente : calcul manuel des corrélations#####
wine_num <- wine[sapply(wine, is.numeric)]

# Créer la matrice vide
slow_cor <- matrix(0,
                   ncol = ncol(wine_num),
                   nrow = ncol(wine_num))

colnames(slow_cor) <- colnames(wine_num)
rownames(slow_cor) <- colnames(wine_num)

# Remplir manuellement la matrice de corrélation
for (i in 1:ncol(wine_num)) {
  for (j in 1:ncol(wine_num)) {
    slow_cor[i, j] <- cor(wine_num[[i]], wine_num[[j]])
  }
}

slow_cor
###### methode optimisée######
wine_num <- wine[sapply(wine, is.numeric)]
fast_cor <- cor(wine_num)
fast_cor

#############################
#      VISUALISATIONS       #
#        (ggplot2)          #
#############################

library(ggplot2)

# Histogramme : distribution du taux d'alcool
# Ce graphique permet de visualiser comment les valeurs du taux d'alcool
# sont réparties dans le dataset. Chaque barre regroupe un intervalle de valeurs.
ggplot(wine, aes(x = alcohol)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Distribution de l'alcool dans les vins") +
  xlab("Alcool") + ylab("Nombre de vins")


# Relation entre alcool et qualité (nuage de points)
# Chaque point représente un vin, avec son taux d'alcool (x) et sa qualité (y).
# Ce graphique permet d’observer s’il existe une tendance entre les deux variables.
ggplot(wine, aes(x = alcohol, y = quality)) +
  geom_point(color = "darkred") +
  ggtitle("Relation Alcool vs Qualité") +
  xlab("Alcool") + ylab("Qualité")



# Boxplot : comparaison de la qualité selon le type de vin
# Ce graphique montre la distribution des valeurs de qualité pour chaque type de vin.
# Il permet de comparer la médiane, la dispersion et les valeurs extrêmes entre les catégories.
ggplot(wine, aes(x = type_vin, y = quality)) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Qualité des vins selon le type") +
  xlab("Type de vin") + ylab("Qualité")



#############################
#   MATRICE DE CORRÉLATION  #
#        (heatmap)          #
#############################

library(reshape2)
library(ggplot2)

# Sélection des colonnes numériques pour calculer les corrélations
wine_num <- wine[sapply(wine, is.numeric)]

# Calcul de la matrice de corrélation
wine_cor <- cor(wine_num)

# Transformation en format long pour ggplot
melted_cor <- melt(wine_cor)

# Heatmap des corrélations
# Les cases rouges indiquent une corrélation positive forte,
# les cases bleues une corrélation négative forte, et les blanches une corrélation faible.
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value,2)), size = 2) +   # affichage des valeurs
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) +
  ggtitle("Matrice de corrélation (améliorée)")



#############################
#   GRAPHIQUE INTERACTIF    #
#         (Plotly)          #
#############################

install.packages("plotly")
library(plotly)

# Création d’un nuage de points interactif
# Les points sont colorés selon le type de vin et affichent des informations supplémentaires au survol.
p <- ggplot(wine, aes(x = alcohol, y = quality, color = type_vin, text = paste("pH:", pH))) +
  geom_point() +
  ggtitle("Relation Alcool vs Qualité (interactif)") +
  xlab("Alcool") + ylab("Qualité")

# Transformation du graphique ggplot en graphique interactif
ggplotly(p)



#############################
#  HEATMAP (VERSION SIMPLE) #
#############################

library(reshape2)

# Recalcul de la matrice de corrélation (sécurité si reprise du code)
wine_num <- wine[sapply(wine, is.numeric)]
wine_cor <- cor(wine_num)
melted_cor <- melt(wine_cor)

# Version simple du heatmap de corrélation
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value,2)), size = 2) +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=8),
        axis.text.y = element_text(size=8)) +
  ggtitle("Matrice de corrélation")
