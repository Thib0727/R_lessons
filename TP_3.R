# EXERCICE A

# IMPORTER DONNEES LOCALES

setwd("C:/Users/Anthony/Documents/asardell/TP3")
df = read.csv(file = "quiz.csv", sep = "\t", dec = ",")

nrow(df)
ncol(df)
summary(df)

# TRANSFORMER EN TYPE FACTOR
df$Identifiant = as.factor(df$Identifiant)
df$TD = as.factor(df$TD)
df$TP = as.factor(df$TP)
class(df$Identifiant)
class(df$TD)
class(df$TP)


# EXERCICE B

# MOYENNE
mean(df$Quiz1, na.rm = TRUE)
mean(df$Quiz2, na.rm = TRUE)
mean(df$Quiz3, na.rm = TRUE)

# MAX
max(df$Quiz3, na.rm = TRUE)

# MEDIANNE
median(df$Quiz2, na.rm = TRUE)
# INTERPRETATION -> 50% ont plus de 11.05/20

# DECILE
quantile(df$Quiz3, probs = seq(from = 0.1,
                               to = 0.9, 
                               by =0.1), 
         na.rm = TRUE)
# INTERPRETATION -> 40% ont eu 11 ou plus.

# ECART TYPE
sd(df$Quiz1, na.rm = TRUE)
sd(df$Quiz2, na.rm = TRUE)
sd(df$Quiz3, na.rm = TRUE)
# INTERPRETATION -> notes heterogenes

# REPARTITION EN EFFECTIF D'UNE COLONNE
table(df$TP)

round(prop.table(table(df$TP)), digits = 2) # (pour les pourcentages)


# EXERCICE C

# EXTRACTION SI
resultat = subset(df, Quiz3 > 10)
resultat = resultat[ , c("Identifiant","Quiz3")]
nrow(resultat)
View(resultat)

resultat = subset(df, TD == 1)
resultat = resultat[ , c("Identifiant","TD")]
nrow(resultat)
View(resultat)

resultat = subset(df, TP != 21)
resultat = resultat[ , c("Identifiant","TP")]
nrow(resultat)
View(resultat)

resultat = subset(df, Quiz3 >= 5 & Quiz3 <= 10)
resultat = resultat[ , c("Identifiant","Quiz3")]
nrow(resultat)
View(resultat)

resultat = subset(df, Identifiant %in% c(92655100,
                                         85947666,
                                         75752354,
                                         172596215,
                                         111505723,
                                         42690322,
                                         20972010,
                                         43177455))
nrow(resultat)

resultat = subset(df, !Identifiant %in% c(92655100,
                                         85947666,
                                         75752354,
                                         172596215,
                                         111505723,
                                         42690322,
                                         20972010,
                                         43177455))
nrow(resultat)

resultat = subset(df, Quiz1 >= 15 | Quiz2 >= 15 | Quiz3 >= 15)
resultat = resultat[ , c("Identifiant","Quiz1","Quiz2","Quiz3")]
nrow(resultat)
View(resultat)

# COMPTER LES NULL DANS QUIZZ1
resultat = subset(df, is.na(Quiz1))
resultat = resultat[ , c("Identifiant","Quiz1")]
nrow(resultat)
View(resultat)

# NON NULL
resultat = subset(df, !is.na(Quiz1) & !is.na(Quiz2) & !is.na(Quiz3))
resultat = resultat[ , c("Identifiant","Quiz1","Quiz2","Quiz3")]
nrow(resultat)
View(resultat)


# EXERCICE D

# COLONNE MOYENNE
df$Moyenne = round((df$Quiz1 + df$Quiz2 + df$Quiz3) / 3, digits = 1)
head(df)

# FLOP 3
rang = order(df$Moyenne, decreasing = FALSE)
resultat = df[ rang, c("Identifiant","Moyenne")]
head(resultat, n = 3)

# TOP 3
rang = order(df$Moyenne, decreasing = TRUE)
resultat = df[ rang, c("Identifiant","Moyenne")]
head(resultat, n = 3)

# ORDER BY TRIER PLUSIEURS CRITERES
rang = order(df$TP, df$Moyenne, decreasing = c(FALSE,TRUE))
resultat = df[ rang, c("Identifiant","TP", "Moyenne")]
head(resultat, n = 5)

# MOYENNE GROUP BY TP
aggregate(Moyenne ~ TP,
          data = df, 
          FUN = function(x) c(moy = mean(x) ) )

# TABLEAU GROUP BY (min, moyenne, max, effectifs)
resultat = aggregate(Moyenne ~ TD + TP,
          data = df, 
          FUN = function(x) c(min = min(x),
                              moy = mean(x),
                              max = max(x),
                              eff = length(x) ) )



# EXERCICE E

# REMPLACER CHAQUE NULL/NA PAR LA MOYENNE DU QUIZZ CONCERNE
df$Quiz1 = ifelse(test = is.na(df$Quiz1), 
                  yes = mean(df$Quiz1, na.rm = TRUE),
                  no = df$Quiz1)

df$Quiz2 = ifelse(test = is.na(df$Quiz2), 
                  yes = mean(df$Quiz2, na.rm = TRUE),
                  no = df$Quiz2)

df$Quiz3 = ifelse(test = is.na(df$Quiz3), 
                  yes = mean(df$Quiz3, na.rm = TRUE),
                  no = df$Quiz3)

df$Moyenne = round((df$Quiz1 + df$Quiz2 + df$Quiz3) / 3, digits = 1)

# TRANCHES [min, Quart1] / [Quart1, Quart2] / [Quart2, Quart3] / [Quart3, max]
df$Moyenne_Classe = cut(df$Moyenne,
                        breaks = quantile(df$Moyenne),
                        include.lowest = TRUE)
summary(df$Moyenne_Classe)


# EXPORTER DATABASE TO CSV
write.table(x = df, file = "exam_export.csv", sep = ";", dec = ",", row.names = FALSE)