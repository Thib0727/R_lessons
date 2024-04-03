# EXERCICE 1

# IMPORTER PACKAGES
library(readxl)
pokemon <- read_excel(path = "pokemon.xlsx",sheet = "pokemon")

dim(pokemon)
ncol(pokemon)
nrow(pokemon)
summary(pokemon)

# TRANSFORMATION TYPE FACTOR : NUM QUALITATIFS
pokemon$is_legendary <-as.factor(pokemon$is_legendary)
pokemon$generation <-as.factor(pokemon$generation)
pokemon$type <-as.factor(pokemon$type)

summary(pokemon)


# EXERCICE 2

# CREATION COLONNE SI ATTACK > MEDIANNE (+) SINON (-)
med = median(pokemon$attack)
pokemon$attack_group = ifelse(pokemon$attack >= med, "attack+","attack-")
pokemon$attack_group <-as.factor(pokemon$attack_group)
summary(pokemon$attack_group)

# CREATION COLONNE SI TYPE FEU OU EAU
pokemon$water_fire = ifelse(pokemon$type %in% c("water","fire"), "yes","no")
pokemon$water_fire <-as.factor(pokemon$water_fire)
summary(pokemon$water_fire)

# CREATION COLONNE POKEMONS STATS PSEUDO LEGENDARY (QUARTILE 75%)
q3_attack = quantile(pokemon$attack, probs = 0.75)
q3_defense = quantile(pokemon$defense, probs = 0.75)
q3_speed = quantile(pokemon$speed, probs = 0.75)
pokemon$best = ifelse(pokemon$attack > q3_attack &
                      pokemon$defense > q3_defense &
                      pokemon$speed > q3_speed , "yes","no")
pokemon$best <-as.factor(pokemon$best)
summary(pokemon$best)



# IS.NA

# CREATION SOUSDATABASE POUR LES POKEMON AVEC WEIGHT NON NULL/NA
requete = subset(pokemon, is.na(weight_kg))
View(requete)

# CREATION SOUSDATABASE POUR LES POKEMON AVEC WEIGHT NULL/NA
requete = subset(pokemon, !is.na(weight_kg))
View(requete)

# REMPLACER LES NULL/NA PAR LA MEDIANNE
med_weight_kg = median(pokemon$weight_kg, na.rm = TRUE)
pokemon$weight_kgNa = ifelse(is.na(pokemon$weight_kg) , 
                                 med_weight_kg ,
                                 pokemon$weight_kg)

med_height_m = median(pokemon$height_m, na.rm = TRUE)
pokemon$height_mNA = ifelse(is.na(pokemon$height_m) , 
                                 med_height_m ,
                                 pokemon$height_m)



# CUT

# SEPARER EN 3 CATEGORIES LEGER MOYEN LOURD QUANTILE 3
pokemon$weight_group = cut(pokemon$weight_kg,
                           breaks = 3,
                           labels = c("léger","moyen","lourd"))

# PAREIL POUR LA TAILLE MAIS DERNIER TRANCHE 3 ET +
pokemon$height_m_group = cut(pokemon$height_m,
                             breaks = c(0,1,2,3,
                                        max(pokemon$height_m,
                                            na.rm = TRUE)))


# 5 TRANCHES AVEC MIN-Q1 / Q1-Q2 / Q2-Q3 / Q3-MAX
pokemon$defense_group = cut(pokemon$defense,
                             breaks = quantile(pokemon$defense,
                                               na.rm = TRUE),
                            include.lowest = TRUE)
summary(pokemon$defense_group)




# EXERCICE 3
# CALCULER MOYENNE VARIABLE EN FONCTION D'UNE AUTRE

# CALCUL LA MOYENNE D'ATTACK PAR TYPE
aggregate(x = attack ~ type, 
          data = pokemon,
          FUN = function(x) mean(x))

# CALCUL LA MEDIANE D'ATTACK PAR GENERATION ET TYPE
aggregate(x = attack ~ generation + type,
          data = pokemon, 
          FUN = function(x) median(x))

# CALCULER EFFECTIF PAR VARIBLE
# CALCULER EFFECTIF PAR TYPE
aggregate(x = pokedex_number ~ type,
          data = pokemon,
          FUN = function(x) length(x))


# CALCULER MOYENNE ET MEDIANNE DE LA STAT SPEED POUR CHAQUE GEN ET TYPE
# AFFICHER EGALEMENT LES EFFECTIFS DE CHAQUE GRP
aggregate(speed ~ generation + type,
          data = pokemon, 
          FUN = function(x) c(moy = mean(x),
                              med = median(x),
                              eff = length(x) ) )