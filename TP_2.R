# EXERCICE 1

# IMPORTER JEU DE DONNEES
df = read.csv("fao.csv", sep=";", dec=",", header = TRUE)

nrow(df)
summary(df)


# EXERCICE 2

mean(df$Dispo_alim, na.rm=TRUE)

sum(df$Population, na.rm=TRUE)

sd(df$Export_viande, na.rm=TRUE)
sd(df$Import_viande, na.rm=TRUE)

median(df$Prod_viande, na.rm=TRUE)

quantile(df$Dispo_alim)

#Centiles
quantile(df$Import_viande, seq(0,1,0.01))


# EXERCICE 3

# FLOP 5 POPULATION
rang = order(df$Population)
resultat = head(df[ rang , ], n = 5)
View(resultat)

# TOP 5 POPULATION
rang = order(df$Population, decreasing = TRUE)
resultat = head(df[ rang , ], n = 5)
View(resultat)

# TOP 5 VIANDE
rang = order(df$Prod_viande, decreasing = TRUE)
resultat = head(df[ rang , ], n = 5)
View(resultat)

rang = order(df$Import_viande, decreasing = TRUE)
resultat = head(df[ rang , ], n = 5)
View(resultat)

# EXTRAIRE SI CONDITION
resultat = subset(df, Dispo_alim>=2300)
View(resultat)

# EXTRAIRE SI ENS
resultat = subset(df, Dispo_alim > 3500  & Import_viande > 1000)
View(resultat)

# EXTRAIRE SI FR OU BE
resultat = subset(df, $Nom %in% c("France","Belgique"))
View(resultat)


# EXERCICE 4

# AJOUT COLONNE CALCULEE
df$Part_export = df$Export_viande/df$Prod_viande

df$Dispo_alim_pays<-df$Dispo_alim*df$Population

# EXPORTER DATAFRAME VERS FICHIER CSV
# DATASET TO CSV
write.table(x = df, file = "ExportTp2.csv")

# NA.RM GESTION NULL
dispo_alim_mondiale = sum(df$Dispo_alim_pays, na.rm=TRUE)
dispo_alim_mondiale

dispo_alim_mondiale/2300



# EXERCICE 5

# NUAGE DE POINTS LIEN ENTRE DEUX VARIABLES
plot(x = df$Prod_viande,
     y = df$Export_viande, 
     main = "Pays : Prod_viande / Export_viande")

# COEFFICIENT DE CORRELATION
cor(x = df$Prod_viande,
    y = df$Export_viande)

# MATRICE DE CORRELATIONS VARIABLES QUANTI
matriceCor = cor(df[ , - 1] , use = "complete.obs")
matriceCor = round(matriceCor , 2)
View(matriceCor)


# INSTALLER PACKAGE POUR MIEUX VISUALISER
# A executer une seule fois
install.packages("corrplot")
library(corrplot) #je charge mon package pour pouvoir utiliser ses fonctionalités

# CONSTRUIRE CORRELOGRAMME
corrplot(matriceCor, method="circle")