# EXERCICE 1

# CREER UN GRAPHIQUE VIDE AVEC COMME BORNE D'ABSCISSE [-5,5] ET D'ORDONNEES [0,1]
# Créer une toile de fond vide pour le graphique
par(mfrow = c(1,1))
plot(NA, xlim=c(-5, 5), ylim=c(0, 1), xlab="X", 
     ylab="Densité de probabilité", 
     main="Densités de probabilité \n de lois normales")


# A L'AIDE D'UNE BOUCLE FOR, AJOUTER UNE COURBE DENSITE ISSUE D'UNE DES 4 COMBINAISONS DE PARAMETRE DE LOI NORMALE DONNES
# Tracer la densité de probabilité pour chaque simulation
moyennes <- c(0, 0, 0, -2)
sigmas <- c(0.45, 1, 2.25, 0.7)
colors <- c("red", "blue", "green", "orange")
legend_labels <- c()
for (i in 1:length(moyennes)) {
  serie = rnorm(n = 1000, 
                mean = moyennes[i], 
                sd = sigmas[i])
  lines(density(serie), col = colors[i])
  legend_labels <- c(legend_labels, paste("m =", moyennes[i], ",", "s =", sigmas[i]))
}

# Ajouter une légende
legend("topright", legend=legend_labels, col=colors, lwd=2, cex=0.8)


# SIMULER UNE LOI NORMALE N(MU=0, o=1) DE TAILLE 10 000
serie = rnorm(n = 1e4, mean = 0, sd = 1)

# CONSTRUIRE L'HISTOGRAMME DE LA DISTRIBUTION DE LA SERIE AVEC SA COURBE DENSITE
hist(serie, main = "loi normal centrée-réduite",
     probability = TRUE)
lines(density(serie))

median(serie)
quantile(serie)
quantile(serie, 
         probs = seq(from = 0, 
                     to = 1, by = 0.01))

quantile(serie, 
         probs = 0.95)


# QNORM VALEUR THEORIQUE, PNORM VALEUR FONTION RECIPROQUE
qnorm(p = 0.95, mean = 0, sd = 1)
pnorm(q = 1.644854, mean = 0, sd = 1)

# QUELLE VALEUR DE P(X > x) = 0.975
qnorm(p = 0.975, mean = 0, sd = 1)

# QUELLE EST LA PROBABILITE THEORIQUE POUR P(X >= 1.96) = p
1 - pnorm(q = 1.96, mean = 0, sd = 1)


########## EXERCICE 2
########## CONSTRUIRE LA TABLE DE LA LOI NORMALE

########## CONSTRUIRE UN VECTEUR AVEC LES PROBA DE LA PREMIERE COLONNE DE LA TABLE DE LOI NORMALE AVEC PRECISION 4 DECIMALES
indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#on crée un vecteur vide pour ajouter les probas au fur et à mesure
all_probas = c()
#On parcourt les indices lignes
for (i in indices_lignes){
  proba = pnorm(q = i, mean = 0, sd = 1)
  #on ajoute la nouvelle proba au vecteur existant
  all_probas = c(all_probas,proba)
  all_probas = round(all_probas,digits = 4)
}


########## CONSTRUIRE LA TABLE DE LOI NORMALE
indices_colones = seq(from = 0.00, to = 0.09, by = 0.01)
indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#On crée un objet résultat vide.
resultat = NULL
#On parcourt les indices colonnes
for (j in indices_colones) {
  #on crée un vecteur vide pour ajouter les probas au fur et à mesure
  all_probas = c()
  #On parcourt les indices lignes
  for (i in indices_lignes){
    quantile = i + j
    proba = pnorm(q = quantile, mean = 0, sd = 1)
    #on ajoute la nouvelle proba au vecteur existant
    all_probas = c(all_probas,proba)
    all_probas = round(all_probas,digits = 4)
  }
  #On ajoute une colonne au resultat
  resultat = cbind(resultat,all_probas)
}


class(resultat)
table = data.frame(resultat)
colnames(table) = indices_colones
rownames(table) = indices_lignes
View(table)




# EXERCICE 3
# Stocker les tailles d'une population simulée de 10.000.000 de français répartis suivant une loi normale N(171,9)
moyenne_pop<-171
sd_pop<-9
population<-rnorm(n = 1e7, 
                  mean=moyenne_pop, 
                  sd=sd_pop)

mean(population)
sd(population)
hist(population)
#on observe une courbe en cloche
is.data.frame(population)


# COMBIEN DE PERSONNE ONT UNE TAILLE INFERIEUR A 190CM
#observé
pop190 = population[population < 190]
length(pop190)
length(pop190) / length(population)

#en théorie
pnorm(q = 190, mean=moyenne_pop, sd=sd_pop)*1e7


# COMBIEN DE PERSONNE ONT UNE TAILLE SUPERIEUR A 200CM
#observé
pop200 = population[population >= 200]
length(pop200)
length(pop200) / length(population)

#en théorie
#proba de P( X < 200cm)
proba_inf_200 = pnorm(q = 200, mean=moyenne_pop, sd=sd_pop)
#proba de P( X >= 200cm)
1 - proba_inf_200



# EXERCICE 4
# ESTIMATION TAILLE MOYENNE PAR ECHANTILLON

# TIRER UN ECHANTILLON DE TAILLE 100
taille_ech<-100
echantillon<-sample(x = population, 
                    size = taille_ech, 
                    replace = TRUE)
mean(echantillon)
sd(echantillon)

# A PARTIR DE L'ECART TYPE ESTIME CALCULER LA LARGEUR DU DEMI INTERVALLE DE CONFIANCE, PUIS LES BORNES INF ET SUP DE L'INTERVALLE DE CONFIANCE (95%)
largeur<-qnorm(p = 0.975,mean=0,sd=1)*sd_pop/sqrt(taille_ech)
borne_inf<-moyenne_pop-largeur
borne_sup <-moyenne_pop+largeur


# REPLICATE
# A L'AIDE DE REPLICATE TIREZ 1000 ECHANTILLONS DE TAILLE 100
taille_ech<-100
nb_replicat<-1000
echantillons<-replicate(n = nb_replicat,
                        expr =  sample(population,
                                       taille_ech, 
                                       replace = TRUE))

moyennes<-apply(X = echantillons,
                MARGIN = 2,
                FUN = function(x) mean(x))
ecart_types<-apply(echantillons,
                   MARGIN = 2,
                   FUN = function(x) sd(x))

hist(moyennes)
mean(moyennes)
sd(moyennes)


# COMBIEN D'ECHANTILLONS MOYENNE > 172,8CM
#observé
moy172 = moyennes[moyennes > 172]
length(moy172)
length(moy172) / length(moyennes)

#en théorie
#proba de P( X < 172cm)
proba_inf_172 = pnorm(q = 172, 
      mean=moyenne_pop, 
      sd=sd_pop/sqrt(taille_ech))
#proba de P( X >= 172cm)
1 - proba_inf_172


# POUR CHAQUE ECHANTILLON CALCULER LA LARGEUR DU DEMI INTERVALLE DE CONFIANCE
largeur<-apply(X = echantillons,
                   MARGIN = 2,
                   FUN = function(x) pnorm(0.975)*sd(x)/taille_ech)

borne_inf_IC<-moyennes-largeur
borne_sup_IC<-moyennes+largeur


# DATAFRAME AVEC CES 3 VECTEURS
resultat = data.frame(largeur,borne_inf_IC,borne_sup_IC)
View(resultat)



# EXERCICE 5
# SAMPLE = SELECTIONNE UN ECHANTILLON DANS X DE TAILLE  SIZE AVEC OU SANS REMISE (REPLACE)

# TIRER UN ECHANTILLON DE TAILLE 100
moyenne_echantillon<-function(V,n) {
  return(mean(sample(x = V,size = n, replace=TRUE)))
}

# REPLICATE = REPETE L'EVALUATION D'UNE EXPRESSION EXPR, N FOIS

moyennes_20<-replicate(n = nb_replicat, 
                       expr = moyenne_echantillon(V = population,
                                                                   n = 20))
moyennes_30<-replicate(n = nb_replicat, 
                       expr = moyenne_echantillon(V = population,
                                                                   n = 30))
moyennes_50<-replicate(n = nb_replicat, 
                       expr = moyenne_echantillon(V = population,
                                                                   n = 50))
moyennes_100<-replicate(n = nb_replicat, 
                        expr = moyenne_echantillon(V = population,
                                                                    n = 100))
moyennes_500<-replicate(n = nb_replicat, 
                        expr = moyenne_echantillon(V = population,
                                                                    n = 500))


par(mfrow=c(2,3))
hist(moyennes_20, xlim=c(161,181), main="20")
hist(moyennes_30, xlim=c(161,181), main="30")
hist(moyennes_50, xlim=c(161,181), main="50")
hist(moyennes_100, xlim=c(161,181), main="100")
hist(moyennes_500, xlim=c(161,181), main="500")                                                                    

population<-runif(n = 1e7, min = 0, max = 1)
moyennes_20<-replicate(nb_replicat, moyenne_echantillon(population,20))
moyennes_30<-replicate(nb_replicat, moyenne_echantillon(population,30))
moyennes_50<-replicate(nb_replicat, moyenne_echantillon(population,50))
moyennes_100<-replicate(nb_replicat, moyenne_echantillon(population,100))
moyennes_500<-replicate(nb_replicat, moyenne_echantillon(population,500))
par(mfrow=c(2,3))
hist(moyennes_20, xlim=c(0,1), main="20")
hist(moyennes_30, xlim=c(0,1), main="30")
hist(moyennes_50, xlim=c(0,1), main="50")
hist(moyennes_100, xlim=c(0,1), main="100")
hist(moyennes_500, xlim=c(0,1), main="500")