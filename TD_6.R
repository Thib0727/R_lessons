
# ERREURS 1

df <- read.csv(file = "nba2014_2015.csv", sep = ";",
               header = FALSE, dec = ";")
> nrow(nba)
> ncol(nba)
> colname(df)
> srt(df)
> df$Period <- as.factor(df$Period)
> df$PTSTYPE -> as.factor(df$PTSTYPE)
> df$SHOOTER = as.factor(df$shooter)



# CORRECTION 1

df = read.csv(file = "nba2014_2015.csv", sep = ",",
               header = TRUE, dec = ".")
> nrow(df)
> ncol(df)
> colnames(df)
> df$PERIOD = as.factor(df$PERIOD)
> df$PTS_TYPE = as.factor(df$PTS_TYPE)
> df$SHOOTER = as.factor(df$SHOOTER)



# ERREURS 2

> lenght(level(df$Period))
> lenght(df$PTSTYPE)
> lenght(df$SHOTER)
> summary(ddf)
> sd(DF$SHOT_DIST
> sd[df$SHOT_CLOCK]

#combien de tirs manqués/réussis
table(df[ "SHOT_RESULTS" , ])
#les quartiles
quantile(df$SHOT_CLOCK, probs = 4)
#les déciles
quantiles(df$CLOSE_DIST, probs = 10)
#nombre de matches différents
liste_game <- unique(df$GAME_ID))
length(listegame)
#nombre de joueurs différents
df$SHOOTER <- as_factor(df$SHOOTER)
nlevel(df$SHOOTER
#conversion de la variable SHOT_DIST en mètre pour que les européens comprennent nos chiffres
nba$SHOT_DIST_METRE == SHOT_DIST * 0.30
#nombre de points qu'a rapporté la tentative (0,2 ou 3)  
df$PTS_MARQUES <- ifelse(df$SHOT_RESULT = "made", yes = df$PTS_TYPE, 0)
#On supprime la variable GAME_RESULT car elle n'est pas utile
df$GAME_RESULT <- NUL
   
#création d'un objet sans la première colonne GAME_ID
df2 <- df[ -1  ,  ]



# CORRECTION 2

length(level(df$PERIOD))
length(df$PTS_TYPE)
length(df$SHOOTER)
summary(df)
sd(DF$SHOT_DIST)
sd(df$SHOT_CLOCK)

table(df$SHOT_RESULTS)
quantile(df$SHOT_CLOCK, probs = seq(0.25,0.75,0.25))
liste_game = unique(df$GAME_ID)
length(listegame)

df$SHOOTER = as.factor(df$SHOOTER)
nlevels(df$SHOOTER)

df$SHOT_DIST_METRE = df$SHOT_DIST * 0.3
df$PTS_MARQUES = ifelse(df$SHOT_RESULT == "made", yes = df$PTS_TYPE, no = 0)
df = df[,-c("GAME_RESULT")]
df2 = df[,-1]







# ERREURS 3

#Les 100 tirs réussis ou manqués les plus loin
rang <- order(df$SHOT_DIST, decreasing = FALSE)
df3 <- df[, rang]
df3 <- df[ 1 : 100 ; ]

#Les 100 tirs réussis les plus loin
df4 = subset(df3, SHOT_RESULT = made)
df4 <- df[ 1 : 100 ; ]

#Combien de tirs à 3 points a réussi Kobe Bryant ?
df_kobe = subset(df,SHOT_RESULT = made &
                 PTS_TYPE = 3 & 
                 SHOOTER = "Kobe BRYANT")

dim(df_kobe)

#Le TOP5 des joueurs qui ont marqués le plus de points dans la saison
df_total <- aggregate(PTS_MARQUES ~ SHOOTER, data = df, FUN = sum)
df_total_tri <- df_total[-order(df_total$PTS_MARQUES)]
df_top5 <-  df_total_tri[  5  ,  ]




# CORRECTION 3

#Les 100 tirs réussis ou manqués les plus loin
rang = order(df$SHOT_DIST, decreasing = TRUE)
df3 = df[rang,]
df3 = df3[ 1 : 100 , ]

#Les 100 tirs réussis les plus loin
rang=order(df$SHOT_DIST, decreasing = TRUE)
df4 = df[rang,]
df4 = subset(df4, SHOT_RESULT == "made")
df4 = df4[ 1 : 100 , ]

#Combien de tirs à 3 points a réussi Kobe Bryant ?
df_kobe = subset(df,SHOT_RESULT == "made" &
                   PTS_TYPE == 3 & 
                   SHOOTER == "kobe bryant")

dim(df_kobe)

#Le TOP5 des joueurs qui ont marqués le plus de points dans la saison
df$PTS_MARQUES = ifelse(df$SHOT_RESULT == "made", yes = df$PTS_TYPE, 0)
df_total = aggregate(PTS_MARQUES ~ SHOOTER, data = df, FUN = function(x)sum(x))
df_total_tri = df_total[order(df_total$PTS_MARQUES,decreasing = TRUE),]
df_top5 = df_total_tri[  5  ,  ]






# ERREURS 4

#Des graphiques adaptés selon le type de variable

#construction de la fonction
build_graph <- function(une_colonne, nom_colonne) {
  if(is.numeric(une_colonne)) {
    print(boxplot(une_colonne, main = nom_colonne))
  }
  else if (as.factor(une_colonne)) {
    tri <- table(une_colonne)
    print(barplot(tri, main = nom_colonne))
  }

#on déroule la fonction sur chaque colonne du data frame.

for (colonne in colnames(df) {
  build_graph(une_colonne = df[colonne , ] , nom_colonne = colone)
}
}



# CORRECTION 4

#Des graphiques adaptés selon le type de variable

#construction de la fonction
build_graph <- function(une_colonne, nom_colonne) {
  if(is.numeric(une_colonne)) {
    print(boxplot(une_colonne, main = nom_colonne))
  }  else if (is.factor(une_colonne)) {
    tri <- table(une_colonne)
    print(barplot(tri, main = nom_colonne))
  }
  
  #on déroule la fonction sur chaque colonne du data frame.
  
  for (colonne in colnames(df)) {
    build_graph(une_colonne = df[,colonne  ] , nom_colonne = colone)
  }
}

build_graph()