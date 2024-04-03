# EXERCICE1

# METTRE EN PLACE ENVIRONNEMENT TRAVAIL CTRL + SHIFT + H
getwd()
setwd(dir = "C:/Users/Anthony/Documents/asardell/IUT_SD1/dataset")
getwd()


bodies_karts = read.csv(file = "bodies_karts.csv", header = TRUE, sep = ";", dec = ",")
tires = read.csv(file = "tires.csv", header = TRUE, sep = "\t", dec = ",")
gliders = read.csv(file = "gliders.csv", header = TRUE, sep = "|", dec = ".")
drivers = read.csv(file = "drivers.csv", header = TRUE, sep = ";", dec = ",")

dim(bodies_karts)
dim(tires)
dim(gliders)
dim(drivers)


# EXERCICE 2

summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)

# REPRESENTER EN NUAGE DE POINTS LE LIEN ENTRE STATISTIQUES DES DRIVERS SUR WEIGHT ET ACCELERATION
plot(x = drivers$Weight,
     y = drivers$Acceleration, 
     main = "Drivers : Weight / Acceleration")
#Il semble que les deux variables soient corrélées négativement
#Il y a autant de points mais ils sont superposés car certains drivers ont les mêmes statistiques

# CALCULER COEFFICIENT DE CORRELATION
cor(x = drivers$Weight,
    y = drivers$Acceleration)

covXY = cov(x = drivers$Weight, # CALCUL MANUEL
    y = drivers$Acceleration)
sX = sd(drivers$Weight)
sY = sd(drivers$Acceleration)
print(covXY / (sX*sY))

# CALCUL COEFFICIENT DE DETERMINATION MEME RELATION
coefCorr = cor(x = drivers$Weight,
    y = drivers$Acceleration)
coefDeter = coefCorr^2
print(coefDeter)


# MATRICE CORRELATION DES VARIABLES QUANT STAT DES DRIVERS
# AFFICHER CETTE MATRICE
matriceCor = cor(drivers[ , - 1])
matriceCor = round(matriceCor , 2)
View(matriceCor)
#Toutes les variables semblent fortement corrélées entre elles.


# INSTALLER PACKAGE POUR MEILLEURE VISUALISATION
install.packages("corrplot") # UNE SEULE COMPILATION

# CREATION CORRELOGRAMME
library(corrplot) #je charge mon package pour pouvoir utiliser ses fonctionalités
corrplot(matriceCor, method="circle")

# TIRES
matriceCor = round(cor(tires[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
         )

# BODIES
matriceCor = round(cor(bodies_karts[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
         )

# GLIDERS
matriceCor = round(cor(gliders[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
         )



# EXERCICE 3

# CREER SOUSDATABASE AVEC SEULEMENT QUELQUES COLONNES
resultat = drivers[ , c("Driver" , "Weight")]
View(resultat)

# CREER SOUSDATABASE AVEC SEULEMENT QUELQUES COLONNES ET 10 PREMIERES LIGNES
resultat = drivers[ 1:10 , c("Driver" , "Acceleration")]
View(resultat)

# CREER SOUSDATABASE SANS COLONNES 5 7 ET 9
resultat = drivers[ , -c(5,7,9)]
View(resultat)

# CREER SOUSDATABASE SANS COLONNES WEIGHT ET ACCELERATION
resultat = drivers[ , -c("Weight","Acceleration")] #cela fonctionne uniquement sur des index numériques.
resultat = drivers[ , -c(2,3)]

# CREATION SOUSDATABASE SEULEMENT COLONNES DRIVER ACCELERATION ET WEIGHT
resultat = drivers[ , c("Driver", "Acceleration", "Weight")]
View(resultat)
#Les colonnes sont dans l'ordre défini par le vecteur.

# CREER SOUSDATABASE AVEC SEULEMENT LES DRIVERS 3 12 ET 32 DANS CET ORDRE PUIS 32 3 12
resultat = drivers[ c(3,12,32) , ]
View(resultat)
resultat = drivers[ c(32,3,12) , ]
View(resultat)
#Les lignes sont dans l'ordre défini par le vecteur.


# CREER SOUSDATABASE AVEC SEULEMENT COLONNES DRIVER ET WEIGHT EN TRIANT CONDUCTEURS DU + AU - LEGER
rang = order(drivers$Weight)
resultat = drivers[ rang  , c("Driver", "Weight") ]
View(resultat)

# CREER SOUSDATABASE AVEC SEULEMENT COLONNES DRIVER ET ACCELERATION EN TRIANT CONDUCTEURS DU + AU - RAPIDE
rang = order(drivers$Acceleration, decreasing = TRUE)
resultat = drivers[ rang  , c("Driver", "Acceleration") ]
View(resultat)

# CREER SOUSDATABASE AVEC COLONNES DRIVER WEIGHT ET ACCELERATION EN TRIANT CONDUCTEURS DU + AU - RAPIDE PUIS DU + AU - LEGER
rang = order(drivers$Acceleration, drivers$Weight, decreasing = c(TRUE,FALSE))
resultat = drivers[ rang  , c("Driver", "Acceleration","Weight") ]
View(resultat)


# EXERCICE 4

# CONDUCTEURS AVEC PLUS GRANDE ACCELERATION
help(subset)
topDriver = subset(x = drivers,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Driver","Acceleration"))

# GLIDER AVEC PLUS GRANDE ACCELERATION
topGlider = subset(x = gliders,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Glider","Acceleration"))

# TIRES AVEC PLUS GRANDE ACCELERATION
topTires = subset(x = tires,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Tire","Acceleration"))

# BODY AVEC PLUS GRANDE ACCELERATION
topBody = subset(x = bodies_karts,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Body","Acceleration"))