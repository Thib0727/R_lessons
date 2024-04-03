# EXERCICE 1

setwd("C:/Users/Anthony/Documents/asardell/IUT_SD1/dataset/nba")
getwd()
# CTRL + SHIFT + H

################### IMPORTER AUTOMATISATION

# LISTER TOUS LES FICHIERS D'UN DOSSIER
fichiers <- list.files(path = getwd(),
                       pattern = ".csv$", # DOLLARS NON OBLIGATOIRE
                       full.names = TRUE)



# RETROUVER TOUS LES NOMS DES FICHIERS DANS UN DOSSIER SANS LEUR EXTENSION
library(tools)
print(fichiers[1])
nom_fichier = basename(path = fichiers[1])
nom_fichier_sans_extension = file_path_sans_ext(x = nom_fichier)
print(nom_fichier_sans_extension)

# IMPORTER UNE LISTE DE DATASET
# Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
assign(x = nom_fichier_sans_extension, 
       value = read.csv(fichiers[1],
                        sep = ",",
                        dec = "."))
#un dataframe vient d'être créé avec comme nom d'objet le nom du fichier sans extension.


# EXEMPLE BOUCLE FOR
# Boucle pour lire chaque fichier CSV
for (fichier in fichiers) {
  # Extraire le nom du fichier sans extension
  nom_objet <- file_path_sans_ext(basename(fichier))
  
  # Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
  start_time <- Sys.time()
  assign(nom_objet, read.csv(fichier, 
                                sep = ",",
                                dec = "."))
  end_time <- Sys.time()
  # Calcul du temps écoulé
  execution_time <- end_time - start_time
  cat("Importation : ",nom_objet, "=" , execution_time , "\n")
}


# EXERCICE 2 

# # # # # # # # # # # # # # #
# # # # # JOINTURES # # # # #
# # # # # # # # # # # # # # #

# JOINTURE NB SI LIEU = LOS ANGELES
df_x = subset(team, city == "Los Angeles", select = c("id", "city"))
df_y = subset(game, select = c("game_id", "team_id_home"))
dfJoin = merge(x = df_x, y = df_y, 
               by.x = "id", 
               by.y = "team_id_home", 
               all.x = TRUE)
nrow(dfJoin)
View(dfJoin)

# JOINTURE MOYENNE AFFLUENCE
df_x = dfJoin
df_y = subset(game_info, select = c("game_id", "attendance"))
dfJoin = merge(x = df_x, y = df_y, 
               by = "game_id",
               all.x = TRUE)
mean(dfJoin$attendance, na.rm = TRUE)
View(dfJoin)

# JOINTURE NB TOTAL ARBITRES EN SAISON 2020
df_x = subset(game_summary, season == 2020,
              select = c("game_id", "season"))
dfJoin = merge(x = df_x, y = officials, 
               by = "game_id",
               all.x = TRUE)
length(unique(dfJoin$official_id))
View(dfJoin)

# JOINTURE NB MATCH OFFICIES PAR DICK BAVETTE
df_x = subset(game_summary,
              select = c("game_id", "season"))
df_y = subset(officials, first_name == "Dick" & last_name == "Bavetta")
dfJoin = merge(x = df_x, y = df_y, 
               by = "game_id",
               all.y = TRUE)
View(dfJoin)
table(dfJoin$season)


# EXERCICE 3

# INSTALLER PACKAGE
library(DBI)
library(RSQLite)
mydb <- dbConnect(SQLite(), "nbaDb.sqlite")

# LISTER TABLES DE LA DATABASE
dbListTables(mydb)

# 5 PREMIERES LIGNES DE LA TABLE TEAM
dbGetQuery(mydb, 'SELECT * FROM team LIMIT 5')

# JOINTURE PACKAGE
dfJoin = dbGetQuery(mydb, '....')

# CREATION NOUVELLE TABLE
dbWriteTable(mydb, "nom_table", dfJoin)
dbListTables(mydb)

# FERMER CONNEXION
dbDisconnect(mydb)