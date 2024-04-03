# EXERCICE 1

a <- 10
b = 5

resultat = a * b
print(resultat)

# A != a
A = 7.2
b = 10.1

# nouveau calcul de resultat ecrase ancien resultat
resultat <- A + B
print(resultat)

# SUPPRIMER TOUTES VARIABLES
rm(a,A,b,B,resultat)
rm(list = ls())


# EXERCICE 2

vecteur <- c(1, 2, 3, 4, 5)
class(vecteur)
vecteur[3]

# VECTEUR NB DE 1 A 5
v1 <- 1:5

# AJOUTE 3 A CHAQUE ELEM DE V1
v2 <- v1 + 3

# VECTEUR NB DE 1 A 6
v3 <- 1:6

v4 <- v3^2

v5 <- v4 / 2

vecteur <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
class(vecteur)
# AFFICHER 2E ET 7E ELEM
vecteur[c(2,7)]

vecteur <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
class(vecteur)

vecteur <- c(1.2, 2.5, 3.8, 4.1, 5.6)
class(vecteur)
# AFFICHER TOUT SAUF 3EME ELEM
vecteur[-3]

vecteur <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
class(vecteur)
vecteur[c(1,2,3)]

vecteur <- c(-1, -2, -3, -4, -5)
class(vecteur)
vecteur[c(5,1)]

vecteur <- c("Pomme", "Banane", "Orange", "Fraise", "Ananas")
class(vecteur)
# AFFICHER TOUT SAUF LES DEUX PREMIERS ELEM
vecteur[-c(1,2)]

vecteur <- c(1, 2, NA, 4, 5)
class(vecteur)

# SEQUENCE DE NOMBRES
ma_sequence <- seq(from = 1, to = 10)
length(ma_sequence)

ma_sequence <- seq(from = 2, to = 20, by = 2)
length(ma_sequence)

ma_sequence <- seq(from = 0, to = -5)
length(ma_sequence)

ma_sequence <- seq(from = 5, to = 50, by = 5)
length(ma_sequence)

# STEP NEGATIF POSSIBLE
ma_sequence <- seq(from = 10, to = 1, by = -1)
length(ma_sequence)

# STEP DECIMAL POSSIBLE
ma_sequence <- seq(from = 0, to = 1, by = 0.1)
length(ma_sequence)

ma_sequence <- seq(from = 5, to = -5, by = -1)
length(ma_sequence)

ma_sequence <- seq(from = 1, to = 10, by = 2)
length(ma_sequence)

# REPETER LA VALEUR "3" 5 FOIS
vecteur <- rep(3, times = 5)
# REPETER PATERN 3 FOIS
vecteur <- rep(c('A', 'B', 'C'), times = 3)

vecteur <- rep(1:3, times = 3)

vecteur <- rep(c(TRUE, FALSE), times = 4)

rm(vecteur)


# EXERCICE 3

# CREATION 5 NOMBRES ALEATOIRES ENTRE 0 ET 1
vecteur <- runif(n = 5, min = 0, max = 1)
vecteur
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

vecteur <- runif(n = 10, min = -5, max = 5)
vecteur
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

vecteur <- runif(n = 100, min = 10, max = 20)
vecteur
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

vecteur <- runif(n= 15, min = 50, max = 100)
vecteur
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

# CREATION 20 NOMBRES ALEATOIRES SELON LOI NORMALE
echantillon <- rnorm(n = 20, mean = -2, sd = 3)
moyenne <- mean(echantillon)
ecart_type <- sd(echantillon)
print(paste("Moyenne : ", moyenne))
print(paste("Écart-type : ", ecart_type))
hist(echantillon)

echantillon <- rnorm(n = 2000, mean = -2, sd = 3)
moyenne <- mean(echantillon)
ecart_type <- sd(echantillon)
print(paste("Moyenne : ", moyenne))
print(paste("Écart-type : ", ecart_type))
hist(echantillon)

echantillon <- rnorm(n = 2000, mean = 0, sd = 1)
moyenne <- mean(echantillon)
ecart_type <- sd(echantillon)
print(paste("Moyenne : ", moyenne))
print(paste("Écart-type : ", ecart_type))
hist(echantillon)

# CALCUL QUARTILES 20% 50% 75%
quantile(echantillon, probs = c(0.25))
quantile(echantillon, probs = c(0.50))
quantile(echantillon, probs = c(0.75))

quantile(echantillon, probs = seq(0.25,75,0.25))

# CALCUL DECILE 10% 20% 30% 40% 50% 60% 70% 80% 90%
quantile(echantillon, probs = seq(0,1,0.1))

# CALCUL CENTILES 1% 2% 3% 4% 5% ... 98% 99% 100%
quantile(echantillon, probs = seq(0,1,0.01))


# MOYENNE ECART TYPE ECHANTILLON NB ALEATOIRE
echantillon <- rnorm(n = 3000, mean = 2400, sd = 300)
moyenne <- mean(echantillon)
ecart_type <- sd(echantillon)
print(paste("Moyenne : ", moyenne))
print(paste("Écart-type : ", ecart_type))

# ARRONDIR A X DECIMALES
echantillon = round(echantillon, 2)

# MASSE SALARIALE
masse_salariale <- sum(echantillon)
print(paste("Masse salariale : ", masse_salariale))

# MEDIANNE
salaire_median <- median(echantillon)
print(paste("Salaire médian : ", salaire_median))

# INTERPRETER QUANTILE 99%
quantile(echantillon, probs = 0.99)
quantile(echantillon, probs = 0.2) # QUANTILE 20%


# SIMULER LANCER DE DE
sample(x = c(1,2,3,4,5,6), size = 1)

# ENREGISTRER 12 SIMULATIONS DE LANCERS DANS UN VECTEUR
simulation <- sample(x = c(1,2,3,4,5,6), size = 12, replace = TRUE)
print(simulation)

# UNIQUE
unique(simulation)

# TABLE NB D'APPARITIONS POUR CHAQUE ELEM
table(simulation)

# CALCULER NB OCCURENCES FACE
prop.table( table(simulation) )

# SIMULATION DE 100 000 DES
simulation <- sample(x = c(1,2,3,4,5,6), size = 100000, replace = TRUE)
print(simulation)

# REPARTITION DU NOMBRE D'APPARITIONS DE CHAQUE FACE EN POURCENTAGE
simulation <- sample(x = c(1,2,3,4,5,6), size = 100000, replace = TRUE)
frequence <- prop.table( table(simulation) )
sort(frequence, decreasing = TRUE)