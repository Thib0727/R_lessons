# EXERCICE 1

#1
a <- 10
b <- 5
#2
resultat <- a*b
print(resultat)
#3
A = 7.2
B = 10.1

#4
resultat = A+B

#5
rm(a,A,b,B,resultat)


# EXERCICE 2

#1.1
a = c(1,2,3,4,5)
class(a)
print(a[3])

#1.2
v1 =c(1,2,3,4,5)

#2
v2 = v1 + 3

#3
v3 = c(1,2,3,4,5,6)

#4
v4 = v3^2

#5
v5 = v4 / 2

#6
v6 = c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche")
print(class(v6))
print(v6[2])
print(v6[7])
print(v6[c(2,7)])

#7
v7 = c(TRUE,FALSE,TRUE)
print(class(v7))

#8
v8 = c(1.1,1.2,1.3,1.4,1.5)
print(class(v8))
print(v8[-3])
print(v8[c(1,2,4,5)])

#9
v9 = c("Janvier","Fevrier","Mars","Avril","Mai","Juin","Juillet","Aout","Septembre","Octobre","Novembre","Decembre")
print(v9[c(1,2,3)])

#10
v10 =c(-1,-2,-3)
print(class(v10))

#11
v11 =  c("Pomme","Poire","Scoubidoubidou")
print(v11[-c(1,2)])

#12
v12=c(1,2,NA,4)
print(class(v12))


#1
a= seq(1,10)
a = c(1,2,3,4,5,6,7,8,9,10)
print(length(a))

#2
b = seq(2,20,2)
print(length(b))

#3
c = seq(0,-5,-1)
length(c)

#4
d =seq(5,50,5)
length(d)

#5
e = seq(10,1,-1)
length(e)

#6
f = seq(0,1,0.1)
length(f)

#7
g = seq(5,-5,-1)
length(g)

#8
h = seq(1,10,2)
length(h)

#1
A = rep(3,5)

#2
B = rep(c('A','B','C'),3)

#3
C = rep(c(1,2,3),3)

#4
D = rep(c(TRUE,FALSE),4)

#5
rm(vecteur)


# EXERCICE 3


#1
vecteur = runif(5,0,1)
mean(vecteur) # avg()
median(vecteur)
min(vecteur) 
max(vecteur)

#2

#3

#4

#5

#6

#7

#8

#9






