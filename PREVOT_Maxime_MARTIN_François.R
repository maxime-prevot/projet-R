#-----------------------------------------#
# CHARGEMENT DES DONNEES #
#-----------------------------------------#


# Chargement des donnees
projet <- read.csv("Data Projet.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)


#-----------------------------------------#
# DIFFERENTS AFFICHAGE DU JEU DE DONNEES #
#-----------------------------------------#

# Manipulations de base
names(projet)
summary(projet)

#Selection des lignes oÃ¹ reponse = oui
projet[projet$RESPONSE=="Oui", ]

#Affichage de la distribution  
table(projet$RESPONSE)

#Graphique sectoriel des classes
qplot(RESPONSE, data=projet, fill=RESPONSE)

#Histogramme par rÃ©ponse d'age et de revenus
qplot(INCOME, data=projet, fill=RESPONSE,bins=5)
qplot(AGE, data=projet, fill=RESPONSE,bins=5)


#Nuage de point en fonction de l'age et des revenus par reponse
qplot(AGE, INCOME, data=projet, main="Nuage de point de Revenus et Age", xlab="Valeur de Age", 
      ylab="Valeur de Revenus", color=RESPONSE)

#-----------------------------------------#
# Installation & activation des librairies #
#-----------------------------------------#



# Installation/m-a-j des librairies
install.packages("ggplot2")
install.packages("rpart")
install.packages("C50")
install.packages("tree")
install.packages("arules")
install.packages("cluster")
install.packages("fpc")
install.packages("arulesViz")
install.packages("randomForest")
install.packages("kknn")
install.packages("ROCR")
install.packages("nnet")
install.packages("svm")
install.packages("naivebayes")

# Activation des librairies
library(rpart)
library(C50)
library(tree)
library(arules)
library(cluster)
library(ggplot2)
library(fpc)
library(randomForest)
library(kknn)
library(ROCR)
library(arulesViz)
library(nnet)
library(e1071)
library(naivebayes)
#-----------------------------------------#
# Extraction des rÃ¨gles d'association  #
#-----------------------------------------#

# Representation au format transactionnel
projetTransaction = as(projet, "transactions")

#Affichage de l'histogramme de frÃ©quence
itemFrequencyPlot(projetTransaction, support = 0.1, cex.names=0.8)


# Affichage interactif sous forme de graphe
plot(rules, method="graph", engine='interactive', shading=NA)

#Insertion des donnÃ©es
projetResponse <- read.csv("Data Projet.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)


projetResponse$AGE <- as.factor(projetResponse$AGE)
projetResponse$ADDRESS <- as.factor(projetResponse$ADDRESS)
projetResponse$INCOME <- as.factor(projetResponse$INCOME)
projetResponse$CAR <- as.factor(projetResponse$CAR)
projetResponse$EMPLOY <- as.factor(projetResponse$EMPLOY)
projetResponse$RESIDE <- as.factor(projetResponse$RESIDE)

#Creation des regles d'associations


rules2 <- apriori(projetResponse, parameter = list(supp = 0.05, conf = 0.5, target ="rules", minlen=2, maxlen=20, maxtime=20))*
#tri par lift
rules2 <- sort(rules2, by="lift", decreasing=TRUE)

#Transformation en dataframe pour manipulations
df_rules2 <- DATAFRAME(rules2)

#Retrait des lignes ou lift <= 1
df_rules2 <- df_rules2[df_rules2$lift>1,]

#Selection lignes avec RESPONSE = oui en consequence
df_rules2[df_rules2$RHS=="{RESPONSE=Oui}",]

#Selection lignes avec RESPONSE = non en consequence
df_rules2[df_rules2$RHS=="{RESPONSE=Non}",]

#Selection lignes avec RESPONSE = oui en antecedant
df_rules2[df_rules2$LHS=="{RESPONSE=Oui}",]

#Selection lignes avec RESPONSE = non en antecedant
df_rules2[df_rules2$LHS=="{RESPONSE=Non}",]

#---------------------#
# MATRICE DE DISTANCE #
#---------------------#

#Calcul de la matrice de distance
dmatrix <- daisy(projet)


#-------------#
# CLUSTERING  #
#-------------#

#K-means
km <- kmeans(dmatrix, 4)

# Répartition des classes RESPONSE=Oui/Non
table(km$cluster, projet$RESPONSE)


# Histogramme des effectifs des clusters avec la classe en couleur
qplot(km$cluster, data=projet, fill=RESPONSE)

# Variable catégorielle cluster
km.clust <- as.factor(km$cluster)


# Histogramme des effectifs des clusters avec la classe en couleur
qplot(as.factor(km.clust), data=projet, fill=RESPONSE)

# Clustering hiérarchique par fonction diana()
dia <- diana(dmatrix)

# Clustering par fonction dbscan()
dbs <- dbscan(dmatrix, eps=0.125, minPts = 3)

# Affichage du dendrogramme
plot(dia, which.plots = 2)



#-----------------------------------------#
# Classification supervisée               #
#-----------------------------------------#




#Split des donnees en aprentissage et test
projet_EA <- projet[1:4000,]
projet_ET <- projet[4001:6400,]


#-----------------------------------------#
# Choix de l'arbre de décision            #
#-----------------------------------------#


#Construction de l'arbre de decision rpart 
tree1 <- rpart(RESPONSE~., projet_EA, method="class",control = rpart.control(minsplit = 1, minbucket = 1,cp = 0.001))

#Construction de l'arbre de decision C5.0 
tree2 <- C5.0(RESPONSE~., projet_EA)

#Construction de l'arbre de decision tree 
tree3 <- tree(RESPONSE~., projet_EA)


#-----------------------------------------#
# Génération des prédictions              #
#-----------------------------------------#

#Prediction sur data projet new en utilisant l'arbre de decision rpart
test_tree1 <- predict(tree1, projet_ET, type="class")


# Matrice de confusion pour 'tree1'
mc_tree1 <- table(projet_ET$RESPONSE, test_tree1)
print(mc_tree1)
# Rappel
mc_tree1[2,2]/(mc_tree1[2,2]+mc_tree1[2,1])
# Specificite
mc_tree1[1,1]/(mc_tree1[1,1]+mc_tree1[1,2])
# Precision 
mc_tree1[2,2]/(mc_tree1[2,2]+mc_tree1[1,2])
# Taux de Vrais Negatifs 
mc_tree1[1,1]/(mc_tree1[1,1]+mc_tree1[2,1])


#Prediction sur data projet new en utilisant l'arbre de decision c5.0
test_tree2 <- predict(tree2, projet_ET, type="class")


#Prediction sur data projet new en utilisant l'arbre de decision tree
test_tree3 <- predict(tree3, projet_ET, type="class")


#Affichage des prédictions sur les classes
table(test_tree1)
table(test_tree2)
table(test_tree3)

#---------------------------#
# CALCUL DES TAUX DE SUCCES #
#---------------------------#

# Ajout des predictions de 'tree1'
projet_ET$Tree1 <- test_tree1
# Ajout des predictions de 'tree2' 
projet_ET$Tree2 <- test_tree2
# Ajout des predictions de 'tree3'
projet_ET$Tree3 <- test_tree3


# Calcul du taux de succes : nombre de succes sur nombre d'exemples de test
taux_succes1 <- length(projet_ET[projet_ET$RESPONSE==projet_ET$Tree1,"AGE"])/nrow(projet_ET)
taux_succes1
taux_succes2 <- length(projet_ET[projet_ET$RESPONSE==projet_ET$Tree2,"AGE"])/nrow(projet_ET)
taux_succes2
taux_succes3 <- length(projet_ET[projet_ET$RESPONSE==projet_ET$Tree3,"AGE"])/nrow(projet_ET)
taux_succes3

#-----------------------------------------#
# Evaluation des classifiers              #
#-----------------------------------------#


#-----------------------------------------#
# ARBRE DE DECISION RPART                 #
#-----------------------------------------#

# Definition de la fonction d'apprentissage, test et evaluation par courbe ROC
test_rpart <- function(arg1, arg2, arg3, arg4){

  # Apprentissage du classifeur
  dt <- rpart(RESPONSE~., projet_EA, parms = list(split = arg1), control = rpart.control(minbucket = arg2))
  
  # Tests du classifieur : classe predite
  dt_class <- predict(dt, projet_ET, type="class")
  
  # Matrice de confusion
  print(table(projet_ET$RESPONSE, dt_class))
  
  # Tests du classifieur : probabilites pour chaque prediction
  dt_prob <- predict(dt, projet_ET, type="prob")
  
  # Courbes ROC
  dt_pred <- prediction(dt_prob[,2], projet_ET$RESPONSE)
  dt_perf <- performance(dt_pred,"tpr","fpr")
  plot(dt_perf, main = "Arbres de décision rpart()", add = arg3, col = arg4)
  
  # Calcul de l'AUC et affichage par la fonction cat()
  dt_auc <- performance(dt_pred, "auc")
  cat("AUC = ", as.character(attr(dt_auc, "y.values")))
  
  # Return sans affichage sur la console
  invisible()
}

#----------------#
# RANDOM FORESTS #
#----------------#

# Definition de la fonction d'apprentissage, test et evaluation par courbe ROC
test_rf <- function(arg1, arg2, arg3, arg4){
  # Apprentissage du classifeur
  rf <- randomForest(RESPONSE~., projet_EA, ntree = arg1, mtry = arg2)
  
  # Test du classifeur : classe predite
  rf_class <- predict(rf,projet_ET, type="response")
  
  # Matrice de confusion
  print(table(projet_ET$RESPONSE, rf_class))
  
  # Test du classifeur : probabilites pour chaque prediction
  rf_prob <- predict(rf, projet_ET, type="prob")
  
  # Courbe ROC
  rf_pred <- prediction(rf_prob[,2], projet_ET$RESPONSE)
  rf_perf <- performance(rf_pred,"tpr","fpr")
  plot(rf_perf, main = "Random Forests randomForest()", add = arg3, col = arg4)
  
  # Calcul de l'AUC et affichage par la fonction cat()
  rf_auc <- performance(rf_pred, "auc")
  cat("AUC = ", as.character(attr(rf_auc, "y.values")))
  
  # Return sans affichage sur la console
  invisible()
}

#---------------------#
# K-NEAREST NEIGHBORS #
#---------------------#

# Definition de la fonction d'apprentissage, test et evaluation par courbe ROC
test_knn <- function(arg1, arg2, arg3, arg4){
  # Apprentissage et test simultanes du classifeur de type k-nearest neighbors
  knn <- kknn(RESPONSE~., projet_EA, projet_ET, k = arg1, distance = arg2)
  
  # Matrice de confusion
  print(table(projet_ET$RESPONSE, knn$fitted.values))
  
  # Courbe ROC
  knn_pred <- prediction(knn$prob[,2], projet_ET$RESPONSE)
  knn_perf <- performance(knn_pred,"tpr","fpr")
  plot(knn_perf, main = "Classifeurs K-plus-proches-voisins kknn()", add = arg3, col = arg4)
  
  # Calcul de l'AUC et affichage par la fonction cat()
  knn_auc <- performance(knn_pred, "auc")
  cat("AUC = ", as.character(attr(knn_auc, "y.values")))
  
  # Return sans affichage sur la console
  invisible()
}

#-------------------------#
# SUPPORT VECTOR MACHINES #
#-------------------------#

# Definition de la fonction d'apprentissage, test et evaluation par courbe ROC
test_svm <- function(arg1, arg2, arg3){
  # Apprentissage du classifeur
  svm <- svm(RESPONSE~., projet_EA, probability=TRUE, kernel = arg1)
  
  # Test du classifeur : classe predite
  svm_class <- predict(svm, projet_ET, type="response")
  
  # Matrice de confusion
  print(table(projet_ET$RESPONSE, svm_class))
  
  # Test du classifeur : probabilites pour chaque prediction
  svm_prob <- predict(svm, projet_ET, probability=TRUE)
  
  # Recuperation des probabilites associees aux predictions
  svm_prob <- attr(svm_prob, "probabilities")
  
  # Courbe ROC 
  svm_pred <- prediction(svm_prob[,1], projet_ET$RESPONSE)
  svm_perf <- performance(svm_pred,"tpr","fpr")
  plot(svm_perf, main = "Support vector machines svm()", add = arg2, col = arg3)
  
  # Calcul de l'AUC et affichage par la fonction cat()
  svm_auc <- performance(svm_pred, "auc")
  cat("AUC = ", as.character(attr(svm_auc, "y.values")))
  
  # Return sans affichage sur la console
  invisible()
}


#-------------#
# NAIVE BAYES #
#-------------#

# Definition de la fonction d'apprentissage, test et evaluation par courbe ROC
test_nb <- function(arg1, arg2, arg3, arg4){
  # Apprentissage du classifeur 
  nb <- naive_bayes(RESPONSE~., projet_EA, laplace = arg1, usekernel = arg2)
  
  # Test du classifeur : classe predite
  nb_class <- predict(nb, projet_ET, type="class")
  
  # Matrice de confusion
  print(table(projet_ET$RESPONSE, nb_class))
  
  # Test du classifeur : probabilites pour chaque prediction
  nb_prob <- predict(nb, projet_ET, type="prob")
  
  # Courbe ROC
  nb_pred <- prediction(nb_prob[,2], projet_ET$RESPONSE)
  nb_perf <- performance(nb_pred,"tpr","fpr")
  plot(nb_perf, main = "Classifieurs bayésiens naïfs naiveBayes()", add = arg3, col = arg4)
  
  # Calcul de l'AUC et affichage par la fonction cat()
  nb_auc <- performance(nb_pred, "auc")
  cat("AUC = ", as.character(attr(nb_auc, "y.values")))
  
  # Return sans affichage sur la console
  invisible()
}

#-----------------#
# NEURAL NETWORKS #
#-----------------#

# Definition de la fonction d'apprentissage, test et evaluation par courbe ROC
test_nnet <- function(arg1, arg2, arg3, arg4, arg5){
  # Redirection de l'affichage des messages intermédiaires vers un fichier texte
  sink('output.txt', append=T)
  
  # Apprentissage du classifeur 
  nn <- nnet(RESPONSE~., projet_EA, size = arg1, decay = arg2, maxit=arg3)
  
  # Réautoriser l'affichage des messages intermédiaires
  sink(file = NULL)
  
  # Test du classifeur : classe predite
  nn_class <- predict(nn, projet_ET, type="class")
  
  # Matrice de confusion
  print(table(projet_ET$RESPONSE, nn_class))
  
  # Test des classifeurs : probabilites pour chaque prediction
  nn_prob <- predict(nn, projet_ET, type="prob")
  
  # Courbe ROC 
  nn_pred <- prediction(nn_prob[,1], projet_ET$RESPONSE)
  nn_perf <- performance(nn_pred,"tpr","fpr")
  plot(nn_perf, main = "Réseaux de neurones nnet()", add = arg4, col = arg5)
  
  # Calcul de l'AUC
  nn_auc <- performance(nn_pred, "auc")
  cat("AUC = ", as.character(attr(nn_auc, "y.values")))
  
  # Return ans affichage sur la console
  invisible()
}

# Arbres de decision
test_rpart("gini", 10, FALSE, "red")
test_rpart("gini", 5, TRUE, "blue")
test_rpart("information", 10, TRUE, "green")
test_rpart("information", 5, TRUE, "orange")

# Forets d'arbres decisionnels aleatoires
test_rf(300, 3, FALSE, "red")
test_rf(300, 5, TRUE, "blue")
test_rf(500, 3, TRUE, "green")
test_rf(500, 5, TRUE, "orange")

# K plus proches voisins
test_knn(10, 1, FALSE, "red")
test_knn(10, 2, TRUE, "blue")
test_knn(20, 1, TRUE, "green")
test_knn(20, 2, TRUE, "orange")

# Réseaux de neurones nnet()
test_nnet(50, 0.01, 100, FALSE, "red")
test_nnet(50, 0.01, 300, TRUE, "tomato")
test_nnet(25, 0.01, 100, TRUE, "blue")
test_nnet(25, 0.01, 300, TRUE, "purple")
test_nnet(50, 0.001, 100, TRUE, "green")
test_nnet(50, 0.001, 300, TRUE, "turquoise")
test_nnet(25, 0.001, 100, TRUE, "grey")
test_nnet(25, 0.001, 300, TRUE, "black")

# Support vector machines
test_svm("linear", FALSE, "red")
test_svm("polynomial", TRUE, "blue")
test_svm("radial", TRUE, "green")
test_svm("sigmoid", TRUE, "orange")

# Naive Bayes
test_nb(0, FALSE, FALSE, "red")
test_nb(20, FALSE, TRUE, "blue")
test_nb(0, TRUE, TRUE, "green")
test_nb(20, TRUE, TRUE, "orange")


#------------------------------------------------------#
# Prédiction sur l'ensemble à définir avec Naive Bayes #
#------------------------------------------------------#

#Chargement des données test
projet_test <- read.csv("Data Projet New.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)

#Apprentissage
nv_bayes <- naive_bayes(RESPONSE~., projet, laplace = 20, usekernel = FALSE)


#------------------------------------------------------#
# Application de Naive Bayes à l'ensemble de test      #
#------------------------------------------------------#

#Prediction classe
nv_bayes_class <- predict(nv_bayes, projet_test, type="class")

#Probabilites pour chaque prediction
nv_bayes_prob <- predict(nv_bayes, projet_test, type="prob")

#------------------------------------------------------#
# Enregistrement prédictions                           #
#------------------------------------------------------#

#Ajout des predictions sur la une nouvelle colonne de projet_test
projet_test$PREDICTION <- nv_bayes_class

#Ajout des probabilites sur la derniere colonne de projet_test
projet_test$PROBABILITE_OUI <- nv_bayes_prob[,2]

#Ajout des probabilites sur la derniere colonne de projet_test
projet_test$PROBABILITE_NON <- nv_bayes_prob[,1]
#Enregistrement du fichier de resultats
write.table(projet_test, file="PREVOT_Maxime_MARTIN_François.csv", sep = ",", dec = ".", row.names = F)