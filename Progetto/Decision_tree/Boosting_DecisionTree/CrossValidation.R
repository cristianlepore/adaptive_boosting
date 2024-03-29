# CROSS VALIDAZIONE -------------------------------------------------------

# RICHIAMO FILES ESTERNI --------------------------------------------------
source("DecisionTree.R")

# IMPELEMENTING A WEAK LEARNER - Decision Stumps --------------------------

# DICHIARAZIONE DELLE FUNZIONI --------------------------------------------

# Rimpiazzo i valori delle etichette con -1 o +1.
replaceLabel <- function(y) {
  for(i in 1:nrow(y)) {
    if(y[i] == 2)
      y[i] <- -1
    else
      y[i] <- +1
  }
  return(y)
}

# Creo la Confusion Matrix.
confusionMatrix <- function(H, y_test) {
  # Prendo gli esempi corretti.
  classified <- which(H*y_test>0)
  misclassified <- which(H*y_test<0)
  
  TP <- length(which(y_test[classified]>0))
  TN <- length(which(y_test[classified]<0))
  
  FP <- length(which(H[misclassified]>0))
  FN <- length(which(H[misclassified]<0))
  
  return(list(
    true_positive = TP,
    true_negative = TN,
    false_positive = FP,
    false_negative = FN )
  )
}

# Preparo il grafico per recall/precision.
recall_precision <- function(y_test, H) {
  # Inizializzo i valori che mi serviranno.
  recallList <- c()
  precisionList <- c()
  truePositive <- 0
  
  list <- confusionMatrix(H, y_test)
  total_truePositive <- list$true_positive
  total_falseNegative <- list$false_negative
  total_falsePositive <- list$false_positive

  # Itero su tutti gli elementi Y del test set.
  for(i in 1:nrow(y_test)) {
    label <- y_test[1:i]
    predittore <- H[1:i]
    
    list <- confusionMatrix(predittore, label)
    
    TP <- list$true_positive + list$true_negative
    if(truePositive == TP)
      next
    else if(truePositive < TP) {
      truePositive <- TP
      recall <- list$true_positive/(total_truePositive + total_falseNegative)
      precision <- truePositive/(truePositive + list$false_positive)
      recallList <- c(recallList, recall)
      precisionList <- c(precisionList, precision)
    }
  }
  
  # Ritorno la lista con RecallList e PrecisionList.
  return(list(
              lista_recall = recallList,
              lista_precision = precisionList)
         )
}

# Classifico le istanze.
classify_singleX <- function(stump, singleX) {
  # # Controllo se � un nodo foglia.
  if(stump$isLeaf) {
    return (stump$class)
  }
  
  # Se non � un nodo foglia richiamo iterattivamente la funzione.
  if(singleX[stump$colNumber] > stump$threshold) {
    classify_singleX(stump$right, singleX)
  } else {
    classify_singleX(stump$left, singleX)
  }
}

# Classifico le istanze del testset.
classify_testSet <- function(adaBoost, test, y_test) {
  H <- rep(0, length(y_test))
  
  for(t in 1:length(adaBoost$committee)) {
    # Itero su tutti gli elementi del test.
    y <- c()
    for (i in 1:nrow(test)) {
      y <- c(y, classify_singleX(adaBoost$committee[[t]], test[i,]))
    }
    H <- H + (adaBoost$weight[t] * y)
  }
  return(sign(H))
}

# Ritorno l'errore sui tutta la cross validazione.
K_folds <- function(k, data, depth, T) {
  # Creo 10 folds.
  folds <- cut(seq(1,nrow(data)), breaks=k, labels=FALSE)
  
  # Inizilizzo l'errore a zero.
  er <- 0
  # Usato per calcolare la media del'errore di cross validazione.
  avg <- 0
  
  # Divido i dati i Testset e Trainingset.
  for(i in 1:k) {
    # Segmento i dati.
    validationIdx <- which(folds==i, arr.ind=TRUE)
    validationset <- data[validationIdx, ]
    trainingset <- data[-validationIdx, ]
    
    # Trainingset senza la colonna delle classi.
    training <- as.matrix(trainingset[,-10])
    validation <- as.matrix(validationset[,-10])
    
    # Colonna delle etichette e la prendo come vettore per comodit�.
    y_training <- as.matrix(trainingset[,10])
    y_validation <- as.matrix(validationset[,10])
    
    # Sostituisco le etichette.
    y_training <- replaceLabel(y_training)
    y_validation <- replaceLabel(y_validation)
    # Numero di righe del dataset.
    numberOfRow_training <- nrow(y_training)
    
    # Ritorno una lista con i membri del comitato ed il classificatore finale di AdaBoost.
    adaBoost <- AdaBoost(training, numberOfRow_training, y_training, depth, T)
    
    # Ritorno l'etichetta degli elementi del testset secondo l'albero di decisione.
    H <- classify_testSet(adaBoost, validation, y_validation)
    
    # Calcolo l'errore di validazione.
    # er <- k/length(y_validation) * length(which(H*y_validation<0))x\
    # er <- length(which(H*y_validation<0))
    # Gli ritorno la Cross Entropy.
    prob <- 1/length(y_validation)
    er <- er + (-1*(length(which(H*y_validation<0))*(prob)*log2(prob)))
    # avg <- avg + length(which(H*y_validation<0))*100/length(y_validation)
  }
  
  # Medio tutti gli errori dei validation set per ottenere l'errore di Cross-validazione.
  average_CVerror <- 1/k*er
  # avg_error <- 1/k * length(which(H*y_validation<0))
  
  return(average_CVerror)
}


# CODICE MAIN -------------------------------------------------------------

# Definisco il numero di folds che divider� il mio dataset. Deve essere almeno >=2
k <- 10

# Mischio le righe del dataset.
# dataset <- dataset[sample(nrow(dataset)),]

# Divido il dataset in training e test set.
train <- floor(0.85* nrow(dataset))
# trainIdx <- sample(seq_len(nrow(dataset)), size = train)
trainingset <- dataset[1:train, ]
y_train <- as.matrix(trainingset[,10])

testset <- dataset[(train+1):nrow(dataset), ]
y_test <- as.matrix(testset[,10])
y_test <- replaceLabel(y_test)
y_train <- replaceLabel(y_train)


# Inizializzo il grafico.
CV_error <- c()
depths <- c()
training_error <- c()
test_error <- c()
CV_error <- c()
training_error <- c()
test_error <- c()

# Scelgo la profondit� dell'albero.
for(T in 1:1) {
  for(depth in 1:1) {
    # Cross-Validazione.
    er <- K_folds(k, trainingset, depth, T)
    
    depths <- c(depths, T)
    CV_error <- c(CV_error, er)
    
    # *********** -------------------------------------------------------------
    # TRAINING ERROR ----------------------------------------------------------
    # Ritesto su tutto il Trainingset + validationset.
    adaBoost <- AdaBoost(as.matrix(trainingset[,-10]), nrow(trainingset), y_train, depth, T)
    # Ritorno l'etichetta degli elementi del testset secondo l'albero di decisione.
    H <- classify_testSet(adaBoost, trainingset[,-10], y_train)
    # Esprimo il risultato come Cross Entropy.
    prob <- 1/length(y_train)
    training_error <- c(training_error, -length(which(H*y_train<0))*(prob)*log2(prob))
    # Esprimo il risultato come percentuale.
    # vector_training_error <- c(vector_training_error, length(which(H*y_train<0))*100/length(y_train))
    
    # *********** -------------------------------------------------------------
    # TEST ERROR --------------------------------------------------------------
    # H <- classify_testSet(adaBoost, testset[,-10], y_test)
    # list <- confusionMatrix(H, y_test)
    # print(list)
    
    # Creo il grafico di Recall/Precision.
    # list <- recall_precision(y_test, H)
    # print(list)
    
    # prob=1/length(y_test)
    # test_error <- c(test_error, -length(which(H*y_test<0))*(prob)*log2(prob))
    # Esprimo il risultato come percentuale.
    # vector_test_error <- c(vector_test_error, length(which(H*y_test<0))*100/length(y_test))
  }
}

# *********** -------------------------------------------------------------
# TEST ERROR --------------------------------------------------------------
H <- classify_testSet(adaBoost, trainingset[,-10], y_train)
# list <- confusionMatrix(H, y_test)
# print(list)

# Creo il grafico di Recall/Precision.
list <- recall_precision(y_train, H)


# PLOTTO IL GRAFICO DI RECALL/PRECISION -----------------------------------
# Creo il grafico per recall/precision
plot(list$lista_recall, list$lista_precision,
     ylim=range(c(list$lista_recall,list$lista_precision)),
     type = "l",
     col = "blue",
     lwd = 1,
     xlab = "Recall",
     ylab = "Precision",
     main = "Recall/Precision"
)

legend("bottomright", legend=c("Decision Stumps", "Alberi di decisione", "AdaBoost"),
       col=c("blue", "green", "red"), lty=1:1, cex=1)


# Scelgo la profondit� dell'albero.
for(T in 1:1) {
  for(depth in 4:4) {
    # Cross-Validazione.
    er <- K_folds(k, trainingset, depth, T)
    
    depths <- c(depths, T)
    CV_error <- c(CV_error, er)
    
    # *********** -------------------------------------------------------------
    # TRAINING ERROR ----------------------------------------------------------
    # Ritesto su tutto il Trainingset + validationset.
    adaBoost <- AdaBoost(as.matrix(trainingset[,-10]), nrow(trainingset), y_train, depth, T)
    # Ritorno l'etichetta degli elementi del testset secondo l'albero di decisione.
    H <- classify_testSet(adaBoost, trainingset[,-10], y_train)
    # Esprimo il risultato come Cross Entropy.
    prob <- 1/length(y_train)
    training_error <- c(training_error, -length(which(H*y_train<0))*(prob)*log2(prob))
    # Esprimo il risultato come percentuale.
    # vector_training_error <- c(vector_training_error, length(which(H*y_train<0))*100/length(y_train))
    
    # *********** -------------------------------------------------------------
    # TEST ERROR --------------------------------------------------------------
    # H <- classify_testSet(adaBoost, testset[,-10], y_test)
    # list <- confusionMatrix(H, y_test)
    # print(list)
    
    # Creo il grafico di Recall/Precision.
    # list <- recall_precision(y_test, H)
    # print(list)
    
    # prob=1/length(y_test)
    # test_error <- c(test_error, -length(which(H*y_test<0))*(prob)*log2(prob))
    # Esprimo il risultato come percentuale.
    # vector_test_error <- c(vector_test_error, length(which(H*y_test<0))*100/length(y_test))
  }
}

# *********** -------------------------------------------------------------
# TEST ERROR --------------------------------------------------------------
H <- classify_testSet(adaBoost, trainingset[,-10], y_train)
# list <- confusionMatrix(H, y_test)
# print(list)

# Creo il grafico di Recall/Precision.
list <- recall_precision(y_train, H)


# PLOTTO IL GRAFICO DI RECALL/PRECISION -----------------------------------
# Creo il grafico per recall/precision
lines(list$lista_recall, list$lista_precision,
     ylim=range(c(list$lista_recall,list$lista_precision)),
     type = "l",
     col = "green",
     lwd = 1,
     xlab = "Numero di weak learners",
     ylab = "Cross-Entropy",
     main = "Training performance"
)


# Scelgo la profondit� dell'albero.
for(T in 6:6) {
  for(depth in 4:4) {
    # Cross-Validazione.
    er <- K_folds(k, trainingset, depth, T)
    
    depths <- c(depths, T)
    CV_error <- c(CV_error, er)
    
    # *********** -------------------------------------------------------------
    # TRAINING ERROR ----------------------------------------------------------
    # Ritesto su tutto il Trainingset + validationset.
    adaBoost <- AdaBoost(as.matrix(trainingset[,-10]), nrow(trainingset), y_train, depth, T)
    # Ritorno l'etichetta degli elementi del testset secondo l'albero di decisione.
    H <- classify_testSet(adaBoost, trainingset[,-10], y_train)
    # Esprimo il risultato come Cross Entropy.
    prob <- 1/length(y_train)
    training_error <- c(training_error, -length(which(H*y_train<0))*(prob)*log2(prob))
    # Esprimo il risultato come percentuale.
    # vector_training_error <- c(vector_training_error, length(which(H*y_train<0))*100/length(y_train))
    
    # *********** -------------------------------------------------------------
    # TEST ERROR --------------------------------------------------------------
    # H <- classify_testSet(adaBoost, testset[,-10], y_test)
    # list <- confusionMatrix(H, y_test)
    # print(list)
    
    # Creo il grafico di Recall/Precision.
    # list <- recall_precision(y_test, H)
    # print(list)
    
    # prob=1/length(y_test)
    # test_error <- c(test_error, -length(which(H*y_test<0))*(prob)*log2(prob))
    # Esprimo il risultato come percentuale.
    # vector_test_error <- c(vector_test_error, length(which(H*y_test<0))*100/length(y_test))
  }
}

# *********** -------------------------------------------------------------
# TEST ERROR --------------------------------------------------------------
H <- classify_testSet(adaBoost, trainingset[,-10], y_train)
# list <- confusionMatrix(H, y_test)
# print(list)

# Creo il grafico di Recall/Precision.
list <- recall_precision(y_train, H)


# PLOTTO IL GRAFICO DI RECALL/PRECISION -----------------------------------
# Creo il grafico per recall/precision
lines(list$lista_recall, list$lista_precision,
      ylim=range(c(list$lista_recall,list$lista_precision)),
      type = "l",
      col = "red",
      lwd = 1,
      xlab = "Numero di weak learners",
      ylab = "Cross-Entropy",
      main = "Training performance"
)



# CARICO I GRAFICI --------------------------------------------------------
# Creo il grafico test error
# plot(depths, test_error,
#      ylim=range(c(test_error,training_error)),
#      type = "l",
#      col = "red",
#      lwd = 3,
#      xlab = "Numero di weak learners",
#      ylab = "Cross-Entropy",
#      main = "Training performance"
# )

# legend("topright", legend=c("Test", "Train"),
#        col=c("red", "blue"), lty=1:1, cex=1)

# Creo il grafico training error
# lines(depths, training_error,
#       ylim=range(c(test_error,training_error)),
#       type = "l",
#       col = "blue",
#       lwd = 3
# )

# Creo il grafico cross validazione
# lines(depths, CV_error,
#       ylim=range(c(test_error,training_error,CV_error)),
#       type = "l",
#       col = "green",
#       lwd = 2
# )
