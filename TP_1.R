# EXERCICE 1

iris
class(iris)
View(iris)
nrow(iris)
ncol(iris)
colnames(iris)
summary(iris)
# AFFICHER SEULEMENT CERTAINES COLONNES
iris[ , c("Sepal.Length","Species")]
# AFFICHER SEULEMENT CES 3 LIGNES OU 50 PREMIERES LIGNES
iris[ c(100,103,105) , ]
iris[ 50:100 , ]

mean(iris$Sepal.Length)
median(iris$Sepal.Length)
sd(iris$Petal.Length)
quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by =0.1))



# EXERCICE 2

dfManga <- read.csv(".../.../.../manga.csv", header = TRUE, sep = ",", dec = ".")
dfAnime <- read.csv(".../.../.../anime.csv", header = TRUE, sep = ",", dec = ".")
class(dfManga)
class(dfAnime)

View(dfManga)
#puis
View(dfAnime)

dim(dfManga)
dim(dfAnime)

mean(dfManga$Score)
mean(dfAnime$Score)

sum(dfManga$Vote)
sum(dfAnime$Vote)

sd(dfManga$Score)
sd(dfAnime$Score)

quantile(dfManga$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1))
quantile(dfAnime$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1))


# COMBIEN ONT UNE NOTE SUPERIEUR A 9/10
extraction1 <- subset(dfManga, Score > 9)
nrow(extraction1)

# COMBIEN DE MANGA ONT >= 200 000 VOTES
extraction2 <- subset(dfManga, Vote >= 200000)
nrow(extraction2)

# COMBIEN DE MANGA ONT > 20 000 VOTES ET + 8/10
extraction3 <- subset(dfManga, Vote >= 200000 & Score >= 8)
nrow(extraction3)

#COMBIEN DE MANGA NOTE COMPRISE ENTRE 7 ET 8
extraction4 <- subset(dfManga, Score >= 7 & Score <= 8)
nrow(extraction4)


# CALCULER EFFECTIFS DE VARIABLE RATING
effectifRating <- table(dfAnime$Rating)
print(effectifRating)
length(effectifRating)
prop.table(effectifRating)

# COMBIEN D'ANIME CONCERNES PAR LE PEGI 17
extraction2 <- subset(dfAnime, Rating == "R - 17+ (violence & profanity)")
nrow(extraction2)


# COMBIEN D'ANIME CONCERNES PAR LE PEGI 17 ET UNE NOTE >= 8/10
extraction3 <- subset(dfAnime, Rating == "R - 17+ (violence & profanity)" &
                                 Score >= 8)
nrow(extraction3)

# COMBIEN NE CORRESPONDENT PAS AU RATING 17
extraction4 <- subset(dfAnime, Rating != "R - 17+ (violence & profanity)")
nrow(extraction4)

# COMBIEN CORRESPONDENT AU RATING ALL AGES
extraction5 <- subset(dfAnime, Rating %in% c("PG - Children","G - All Ages"))
nrow(extraction5)

# COMBIEN NE CORRESPONDENT PAS AU RATING ALL AGES
extraction6 <- subset(dfAnime, !Rating %in% c("PG - Children","G - All Ages"))
nrow(extraction6)

# COMBIEN ONT UNE NOTE > 9/10 OU ONT +400 000 VOTES
extraction7 <- subset(dfAnime, Score >= 9 | Vote > 400000)
nrow(extraction7)



# SOUSDATABASE EXTRACTION
dfAnime <- dfAnime[ , c("Title","Score","Vote","Ranked")]
dfManga <- dfManga[ , c("Title","Score","Vote","Ranked")]

# CREATION COLONNE
dfAnime$Type <- "Anime"
dfManga$Type <- "Manga"

# FUSIONNER RBIND
dfConcat <- rbind(dfManga,dfAnime)
View(dfConcat)

# EXPORTER LE DATAFRAME VERS UN CSV
write.table(x = dfConcat, file = ".../.../.../ExportTp1.csv",
            sep = ";",row.names = FALSE)