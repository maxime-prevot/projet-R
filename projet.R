# Chargement des donnees
projet <- read.csv("Data Projet.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)

#Split des données en aprentissage et test
projet_EA <- projet[1:4000,]
projet_ET <- projet[4001:6400,]

# Manipulations de base
names(projet)
summary(projet)

#Selection des lignes où reponse = oui
projet[projet$RESPONSE=="Oui", ]

#Affichage de la distribution  
table(projet$RESPONSE)

require("ggplot2")
# Installation/m-a-j des librairies
install.packages("rpart")
install.packages("C50")
install.packages("tree")

# Activation des librairies
library(rpart)
library(C50)
library(tree)


#Graphique sectoriel des classes
qplot(RESPONSE, data=projet, fill=RESPONSE)

#Histogramme par réponse d'age et de revenus
qplot(INCOME, data=projet, fill=RESPONSE,bins=5)
qplot(AGE, data=projet, fill=RESPONSE,bins=5)


#Nuage de point en fonction de l'age et des revenus par reponse
qplot(AGE, INCOME, data=projet, main="Nuage de point de Revenus et Age", xlab="Valeur de Age", 
      ylab="Valeur de Revenus", color=RESPONSE)

#Construction de l'arbre de décision 
tree1 <- rpart(RESPONSE~., projet_EA, method="class",,control = rpart.control(minsplit = 1, minbucket = 1,cp = 0.001))
print(tree1)

#Affichage de l'arbre décision 
plot(tree1)
text(tree1, pretty=0)

#Prediction sur data projet new en utilisant l'arbre de décision
test_tree <- predict(tree1, projet_ET, type="class")

#Affichage des prédcitions sur les classes
table(test_tree)

#Ajout d'une colonne prediction dans le jeu de données test
projet_ET$PREDICTION <- test_tree
View(projet_ET)

#Affichage des données correctement prédites
View(projet_ET[projet_ET$RESPONSE==projet_ET$PREDICTION, ])

#Calcul du nombre de succes
nb_succes <-length(projet_ET[projet_ET$RESPONSE==projet_ET$PREDICTION, "AGE"])
nb_succes

#Calcul du taux de succes
taux_succes <- nb_succes/nrow(projet_ET)
taux_succes

#Calcul du nombre d'erreurs
nb_erreur <- length(projet_ET[projet_ET$RESPONSE!=projet_ET$PREDICTION,"AGE"])
nb_erreur

#Calcul du taux d'erreurs
taux_erreur <- nb_erreur/nrow(projet_ET)
taux_erreur
