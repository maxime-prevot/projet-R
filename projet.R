#-----------------------------------------#
# CHARGEMENT DES DONNEES #
#-----------------------------------------#


# Chargement des donnees
projet <- read.csv("Data Projet.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)

#Split des données en aprentissage et test
projet_EA <- projet[1:4000,]
projet_ET <- projet[4001:6400,]

#-----------------------------------------#
# DIFFERENTS AFFICHAGE DU JEU DE DONNEES #
#-----------------------------------------#

# Manipulations de base
names(projet)
summary(projet)

#Selection des lignes où reponse = oui
projet[projet$RESPONSE=="Oui", ]

#Affichage de la distribution  
table(projet$RESPONSE)

#Graphique sectoriel des classes
qplot(RESPONSE, data=projet, fill=RESPONSE)

#Histogramme par réponse d'age et de revenus
qplot(INCOME, data=projet, fill=RESPONSE,bins=5)
qplot(AGE, data=projet, fill=RESPONSE,bins=5)


#Nuage de point en fonction de l'age et des revenus par reponse
qplot(AGE, INCOME, data=projet, main="Nuage de point de Revenus et Age", xlab="Valeur de Age", 
      ylab="Valeur de Revenus", color=RESPONSE)

#-----------------------------------------#
# Installation & activation des librairies #
#-----------------------------------------#


require("ggplot2")
# Installation/m-a-j des librairies
install.packages("rpart")
install.packages("C50")
install.packages("tree")
install.packages("arules")


# Activation des librairies
library(rpart)
library(C50)
library(tree)
library(arules)


#-----------------------------------------#
# Extraction des règles d'association  #
#-----------------------------------------#

# Representation au format transactionnel
projetTransaction = as(projet, "transactions")

#Affichage de l'histogramme de fréquence
itemFrequencyPlot(projetTransaction, support = 0.1, cex.names=0.8)

#Generation de regle d'association
rules <- apriori(projet, parameter = list(supp = 0.15, conf = 0.5, target="rules", minlen=2))

#Affichage des règles
inspect(rules)
options(digits=2)
inspect(rules)

rules <-sort(rules, by="support", decreasing = TRUE)
inspect(rules[1:5])

# Tri par confiance
rules <- sort(rules, by="confidence", decreasing=TRUE)
inspect(rules)

# Tri par lift
rules <- sort(rules, by="lift", decreasing=TRUE)
inspect(rules)


# Afficher les 5 premières règles seulement
inspect(rules[1:5])

# AFFICHAGE GRAPHIQUE DES REGLES

# Librairie arulesViz
install.packages("arulesViz")
library(arulesViz)

# Affichage interactif sous forme de graphe
plot(rules, method="graph", engine='interactive', shading=NA)

#Insertion des données
projetResponse <- read.csv("Data Projet.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)


projetResponse$AGE <- as.factor(projetResponse$AGE)
projetResponse$ADDRESS <- as.factor(projetResponse$ADDRESS)
projetResponse$INCOME <- as.factor(projetResponse$INCOME)
projetResponse$CAR <- as.factor(projetResponse$CAR)
projetResponse$EMPLOY <- as.factor(projetResponse$EMPLOY)
projetResponse$RESIDE <- as.factor(projetResponse$RESIDE)

#Création des régles d'associations
rules2 <- apriori(projetResponse, parameter = list(supp = 0.15, conf = 0.5, target ="rules", minlen=2, maxlen=20, maxtime=20))

#Transformation en dataframe pour manipulations
df_rules2 <- DATAFRAME(rules2)

#Suppresion des regles avec lift <= 1 
df_rules2 <- df_rules2[df_rules2$lift>1,]

#Selection lignes avec RESPONSE = oui en consequence
df_rules2[df_rules2$RHS=="{RESPONSE=Oui}",]

#Selection lignes avec RESPONSE = non en consequence
df_rules2[df_rules2$RHS=="{RESPONSE=Non}",]

#Selection lignes avec RESPONSE = oui en antécédant
df_rules2[df_rules2$LHS=="{RESPONSE=Oui}",]

#Selection lignes avec RESPONSE = non en antécédant
df_rules2[df_rules2$LHS=="{RESPONSE=Non}",]
#-----------------------------------------#
# Création de l'arbre de décision #
#-----------------------------------------#
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
