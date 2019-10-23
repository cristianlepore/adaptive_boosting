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

# Classifico le istanze.
classify_singleX <- function(stump, singleX) {
  # # Controllo se è un nodo foglia.
  if(stump$isLeaf) {
    return (stump$class)
  }
  
  # Se non è un nodo foglia richiamo iterattivamente la funzione.
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
    # H <- H + (adaBoost$weight[t] * y)
  }
  
  return(sign(y))
}

# Ritorno l'errore sui tutta la cross validazione.
K_folds <- function(k, data, depth) {
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
    
    # Colonna delle etichette e la prendo come vettore per comodità.
    y_training <- as.matrix(trainingset[,10])
    y_validation <- as.matrix(validationset[,10])
    
    # Sostituisco le etichette.
    y_training <- replaceLabel(y_training)
    y_validation <- replaceLabel(y_validation)
    # Numero di righe del dataset.
    numberOfRow_training <- nrow(y_training)
    
    # Ritorno una lista con i membri del comitato ed il classificatore finale di AdaBoost.
    adaBoost <- AdaBoost(training, numberOfRow_training, y_training, depth, 1)
    
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

# Definisco il numero di folds che dividerà il mio dataset. Deve essere almeno >=2
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

# Scelgo la profondità dell'albero.
for(depth in 1:1) {   #Utilizzo solo due layer di profondità dell'albero.
  # Cross-Validazione.
  er <- K_folds(k, trainingset, depth)
  depths <- c(depths, depth)
  CV_error <- c(CV_error, er)
  
  # *********** -------------------------------------------------------------
  # TRAINING ERROR ----------------------------------------------------------
  # Ritesto su tutto il Trainingset + validationset.
  adaBoost <- AdaBoost(as.matrix(trainingset[,-10]), nrow(trainingset), y_train, depth, 1)
  # Ritorno l'etichetta degli elementi del testset secondo l'albero di decisione.
  H <- classify_testSet(adaBoost, trainingset[,-10], y_train)
  # Esprimo il risultato come Cross Entropy.
  prob <- 1/length(y_train)
  training_error <- c(training_error, -length(which(H*y_train<0))*(prob)*log2(prob))
  # Esprimo il risultato come percentuale.
  # vector_training_error <- c(vector_training_error, length(which(H*y_train<0))*100/length(y_train))
  
  # *********** -------------------------------------------------------------
  # TEST ERROR --------------------------------------------------------------
  H <- classify_testSet(adaBoost, testset[,-10], y_test)
  prob=1/length(y_test)
  test_error <- c(test_error, -length(which(H*y_test<0))*(prob)*log2(prob))
  # Esprimo il risultato come percentuale.
  # vector_test_error <- c(vector_test_error, length(which(H*y_test<0))*100/length(y_test))
}


# CREO I GRAFICI ----------------------------------------------------------

# Creo il grafico test error
# plot(depths, test_error,
#      ylim=range(c(test_error,training_error,CV_error)),
#      type = "l",
#      col = "red",
#      lwd = 3,
#      xlab = "Profondità dell'albero",
#      ylab = "Cross-Entropy",
#      main = "Training performance"
# )
# 
# legend("topright", legend=c("Test", "Train", "Validazione"),
#        col=c("red", "blue", "green"), lty=1:1, cex=1)
# 
# # Creo il grafico training error
# lines(depths, training_error,
#       ylim=range(c(test_error,training_error,CV_error)),
#       type = "l",
#       col = "blue",
#       lwd = 3
# )
# 
# # Creo il grafico cross validazione
# lines(depths, CV_error,
#       ylim=range(c(test_error,training_error,CV_error)),
#       type = "l",
#       col = "green",
#       lwd = 2
# )

training_error <- c()
guadagno <- c()
for(c in 1:9) {
  adaBoost <- AdaBoost(as.matrix(trainingset[,-10]), nrow(trainingset), y_train, 1, c)
  # Ritorno l'etichetta degli elementi del testset secondo l'albero di decisione.
  H <- classify_testSet(adaBoost, trainingset[,-10], y_train)
  # Esprimo il risultato come Cross Entropy.
  prob <- 1/length(y_train)
  training_error <- c(training_error, -length(which(H*y_train<0))*(prob)*log2(prob))
  guadagno <- c(guadagno, adaBoost$committee[[1]]$gain)
}

counts <- rbind(training_error, guadagno)

# GRAFICI SUI DIVERSI GAIN ------------------------------------------------
# Creo il grafico test error
barplot(counts,
     type = "bar",
     col=c("blue","red"),     
     xlab = "Attributi"
)
legend("bottomright",
       c("Cross-Entropy","Guadagno"),
       fill = c("blue","red")
)