# EXERCICE 1

df = read.csv(file = "velov.csv",
              header = TRUE,
              sep = ";", 
              dec = "," )

summary(df)
class(df$status)
class(df$CodePostal)

df$status = as.factor(df$status)
df$CodePostal = as.factor(df$CodePostal)

# CREER EOLONNE (KO) SI BORNES INDISPONIBLES A UNE STTAION (OK) SINON
df$bornes = ifelse(df$capacity != (df$bikes + df$stands), "KO" , "OK")
tables(df$bornes)
#en réalité, c'est aussi peut-être car la station est fermée OU que des usagers ont déposé leur vélo pile au moment de l'extraction.



# EXERCICE 2
# HIST

# HISTOGRAMME DE DISTRIBUTION DE CAPACITY
# MAIN = TITRE
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations")

# 6 CLASSES
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     breaks = 6)

# COULEUR ROUGE
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     breaks = 6,
     col = "red")

# RENOMMER LES AXE DES ABSCISSES "CAPACITY"
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     breaks = 6,
     col = "red",
     xlab = "Capacity")


# CREE UNE LIGNE HORIZONTALE BLEUE AVEC ORDONNEE 100
# S'AJOUTE AU GRAPHIQUE EN COURS DE LECTURE
abline(h = 100, col = "blue", lty = 2)


# HISTOGRAMME EN DENSITE PLUTOT QUE EFFECTIFS
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red"
     probability = TRUE,
     xlab = "Capacity")


# AJOUTER COURBE DENSITE DISTRIBUTION
lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 4) # = TAILLE


# POUR AVOIR LA COURBE DE DENSITE EN ENTIER, MODIFIER LES BORNES DE L'AXE DES ORDONNEES
hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity",
     ylim = c(0,0.08))

lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 2)


# EXERCICE 3
# BOITE A MOUSTACHE


# BOITE A MOUSTACHE DE DISTRIBUTION CAPACITY
boxplot(x = df$capacity, 
        main = "Boxplot de \n la capacité des stations"?
        horizontal = TRUE, # PIVOTER HORIZONTALEMENT
        outline = FALSE) # AFFICHER LES VALEURS ATYPIQUES


# POINT SUPPLEMENTAIRE = MOYENNE DE LA SERIE (EX GROS CARRE ROUGE)
points(moy, col = "red", pch = 15, cex = 2)


# PAR
# DIVISER LA FENETRE GRAPHIQUE EN DEUX PUIS CONSTRUIRE UN BOXPLOT
par(mfrow=c(1,2)) #fenêtre sur 1 ligne et 2 colonnes

#7ème
df7 = subset(df, CodePostal == "69007")
boxplot(x = df7$bikes, 
        main = "Boxplot nb vélos \n 69007",
        ylim = c(0,40))
#8ème
df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, 
        main = "Boxplot nb vélos \n 69008",
        ylim = c(0,40))
#C'est plus simple d'analyser les deux graphiques si la borne des ordonnées est la même.


# ANALYSER NB VELOS EN FONCTION DE BONUS
par(mfrow=c(1,1)) #fenêtre sur 1 ligne et 1 colonne
# Tracer le graphique boxplot
boxplot(formula = bikes ~ bonus,
        data = df, 
        main = "Dispo vélos vs Stations Bonus")


# CALCULER LES MOYENNES DE CHAQUE GROUPE
means <- tapply(X = df$bikes, 
                INDEX = df$bonus, 
                FUN = function(X) mean(X))
print(means)
# Ajouter les moyennes de chaque groupe au graphique
points(means, col = "red", pch = 19)



# EXERCICE 4
# DIAGRAMME EN BARR

# DIAGRAMME EN BARRE DE LA REPARTITION DU NB DE STATION BONUS
effectif = table(df$bonus)
barplot(height = effectif,
        main = "Répartition du nombre \n de station bonus"?
        horiz = TRUE) # PIVOTER HORIZONTALEMENT

# DIAGRAMME PRECEDENT EN POURCENTAGE
frequence = prop.table(effectif)
barplot(height = frequence,
        main = "Répartition en % du nombre \n de station bonus",
        horiz = TRUE)

# DIAGRAMME BIVARIE AVEC REPARTITION NB STATIONS BONUS EN FONCTION DU NB DE STATION AVEC UN TERMINAL DE PAIEMENT
effectif = table(df$banking, df$bonus)
print(effectif)
barplot(height = effectif,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?")
#On remarque qu'on ne sait pas distinguer les deux modalités car il n'y a pas de légende.


# AJOUTER LEGENDE ET COULEURS POUR DISTINGUER

frequence = prop.table(x = effectif) #Calcul des pourcentages
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels = colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)

# POURCENTAGE COLONNE
frequence = prop.table(x = effectif, margin = 2)#Calcul des pourcentages colonnes
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)


# BESIDE
# DIAGRAMME BIVARIE NON EMPILE
#Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"),
        beside = TRUE)

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)


# PIE
# DIAGRAMME CIRCULAIRE DE LA REPARTITION DU NB DE STATION BONUS
pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"))

# AJOUTER LES ETIQUETTES
etiquette = paste(rownames(effectif),"\n",effectif)
pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"),
    labels = etiquette)



# TOP 10 DANS UN DIAGRAMME EN BARRE 
effectif = table(df$CodePostal)
top10 = sort(effectif, decreasing = TRUE)[1:10]
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = palette(),
        las = 2)  # Rotation des étiquettes à 90 degrés
#On remarque que les deux premières couleurs se répetent.
print(palette()) # la fonction `palette()` ne dispose que de 8 couleurs

# COLORS POUR AVOIR PLUS DE COULEURS
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = colors(),
        las = 2)  # Rotation des étiquettes à 90 degrés

print(colors())

#EXPORTER LE GRAPHIQUE EN COURS DE LECTURE EN FORMAT .PNG
dev.print(device = png, file = "export.png", width = 600)


# EXERCICE 5
# PLOT

# CONSTRUIRE UN NUAGE DE POINTS
df$bornes = as.factor(df$bornes)
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité"?
     xlim = c(0,60), # ABSCISSES DE 0 A 60
     ylim = c(0,60), # ORDONNEE DE 0 A 60
     pch=19,# POINT AVEC FOND NOIR
     col = df$bornes) # COULEURS

# AJOUTER UNE LEGENDE
legend("topright", legend = levels(df$bornes),
       col = palette(), pch = 19)

# CHOISIR SES COULEURS
myColors <- c("red", "blue", "green")
col = myColors[df$bornes]

# REPRESENTER UN CERRE VERT SUR LE GRAPHIQUE REPRESENTANT LA MOYENNE DU NB DE PLACES DISPONIBLS ET LA CAPACITE DES STATIONS
moy_stands = mean(df$stands)
moy_capacity = mean(df$capacity)
points(x = moy_stands,y = moy_capacity, 
       pch = 15,
       col = myColors[3],
       cex = 2)




# EXERCICE 6
# CARTOGRAPHIE


# Librairies nécessaires
library(leaflet)
library(dplyr)
library(ggplot2)

# Créer une carte Leaflet
maCarte <- leaflet(df) %>% 
  addTiles() %>% 
  addMarkers(~position_longitude, 
             ~position_latitude, 
             popup = ~address)

# Afficher la carte
maCarte