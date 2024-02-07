#1
iris
class(iris)

#2
View(iris)

#3
nrow(iris)

#4
ncol(iris)

#5
colnames(iris)

#6
summary(iris)

#7
# AFFICHER COLONNES
iris[,c("Sepal.Length","Species")]

#8


#9
# AFFICHER LIGNES
iris[c(100,103,105),]

#10
iris[seq(50,100,1),]
iris[50:100,]

#11
# MOYENNE COLONNE
mean(iris[,c("Sepal.Length")])
mean(iris$Sepal.Length)

#12
median(iris$Sepal.Width)

#13
sd(iris$Sepal.Length)

#14
quantile(iris$Petal.Length,seq(0.1,0.9,0.1))



#1
exemple = read.csv(file=file.choose())
dfManga = read.csv("L:/BUT/SD/Promo 2023/tmuller/BUT SD/R/TP1/manga.csv")
dfAnime = read.csv("L:/BUT/SD/Promo 2023/tmuller/BUT SD/R/TP1/anime.csv")

class(dfManga)
class(dfAnime)

#2
View(dfManga)
View(dfAnime)

#3
dim(dfManga)
dim(dfAnime)

#4
mean(dfAnime[,c("Score")])
mean(dfManga$Score)
mean(dfAnime$Score)

#5
sum(dfManga$Vote)
sum(dfAnime$Vote)

#6
sd(dfManga$Score)
sd(dfAnime$Score)

#7
quantile(dfManga$Score,seq(0.1,0.9,0.1))
quantile(dfAnime$Score,seq(0.1,0.9,0.1))



#1
extraction1 = subset(dfManga,Score > 9)
nrow(extraction1)

#2
extraction2 = subset(dfManga,Vote >= 20000)
nrow(extraction2)

#3
extraction3 = subset(dfManga, Vote >= 20000 & Score > 8)
nrow(extraction3)

#4
extraction4 = subset(dfManga, Score >= 7 & Score <= 8)
nrow(extraction4)




#1
effectifRating = table(dfAnime$Rating)
print(effectifRating)
length(effectifRating)
prop.table(effectifRating)

#2
extraction2 = subset(dfAnime, Rating == "R - 17+ (violence & profanity)")
nrow(extraction2)

#3
extraction3 = subset(dfAnime, Rating == "R - 17+ (violence & profanity)"& Score >= 8)
nrow(extraction3)

#4
extraction4 = subset(dfAnime, Rating != "R - 17+ (violence & profanity)")
nrow(extraction4)

#5
extraction5 = subset(dfAnime, Rating =="PG - Children" | Rating == "G - All Ages" )
extraction5 <- subset(dfAnime, Rating %in% c("PG - Children","G - All Ages"))
nrow(extraction5)

#6
extraction5 = subset(dfAnime, Rating!= "PG - Children" & Rating != "G - All Ages")
extraction5 = subset(dfAnime, !Rating %in% c("PG - Children","G - All Ages") )
nrow(extraction5)

#7
extraction6 = subset(dfAnime, Score >= 9 | Vote > 400000)
nrow(extraction6)



#1
dfAnime = dfAnime[,c("Title","Score","Vote","Ranked")]
dfManga = dfAnime[,c("Title","Score","Vote","Ranked")]

#2
dfAnime$Type = "Anime"
dfManga$Type = "Manga"

#3
dfConcat = rbind(dfManga,dfAnime)
View(dfConcat)

#4
write.table(dfConcat,"L:/BUT/SD/Promo 2023/tmuller/BUT SD/R/TP1/Res_TP1.csv",sep =";",FALSE)
