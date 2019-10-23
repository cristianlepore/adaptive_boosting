# CROSS VALIDAZIONE -------------------------------------------------------

# RICHIAMO FILES ESTERNI --------------------------------------------------
source("DecisionStump.R")
# source("DecisionStump_InformationGain.R")

# DICHIARAZIONE DELLE VARIABILI -------------------------------------------
# Definisco il numero di folds che dividerà il mio dataset. Deve essere almeno >=2
k <- 10

# DICHIARAZIONE DELLE FUNZIONI --------------------------------------------


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

# Calcolo l'errore sui dati di test.
validation_error <- function(adaBoost, test, y_validation, k) {
  H <- rep(0, length(y_validation))
  
  # Itero su tutti i membri del comitato.
  for(i in 1:length(adaBoost$committee)) {
    # Prendo la colonna ed la soglia di ogni singolo membro del comitato.
    colNumber = adaBoost$committee[[i]]$colNumber
    threshold = adaBoost$committee[[i]]$threshold
    
    # Creo le foglie dell'albero (destra e sinistra).
    leftIdx  <- which(as.numeric(test[,colNumber]) <= threshold)
    rightIdx <- which(as.numeric(test[,colNumber]) >  threshold)
    
    h <- numeric(length(test[, colNumber]))
    h[leftIdx] <- adaBoost$committee[[i]]$left$class
    h[rightIdx] <- adaBoost$committee[[i]]$right$class
    
    # Ricreo il mio classificatore finale F per il validation_set.  
    H <- H + (adaBoost$weight[i] * h)
  }
  
  # Prendo solo il segno del classificatore finale.
  H <- sign(H)
  
  # Calcolo l'errore di validazione.
  er <- k/length(y_validation) * length(which(H*y_validation<0))
  
  return(er)
}

# Ritorno l'errore sui tutta la cross validazione.
K_folds <- function(k, data) {
  # Creo 10 folds.
  folds <- cut(seq(1,nrow(trainingset)), breaks=k, labels=FALSE)

  # Inizilizzo l'errore a zero.
  er <- 0
  
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

    # Rimpiazzo le etichette.
    y_training <- replaceLabel(y_training)
    y_validation <- replaceLabel(y_validation)
    # Numero di righe del trainingset.
    numberOfRow_training <- nrow(y_training)
    
    # Ritorno una lista con i membri del comitato ed il classificatore finale di AdaBoost.
    adaBoost <- AdaBoost(training, numberOfRow_training, y_training)
    
    # Errore sul singolo validation set.
    er <- er + validation_error(adaBoost, validation, y_validation, k)
  }
  
  # Medio tutti gli errori dei validation set per ottenere l'errore di Cross-validazione.
  average_error <- 1/k * er
  
  return(average_error)
}


# CODICE MAIN -------------------------------------------------------------

# Mischio le righe del dataset.
# dataset <- dataset[sample(nrow(dataset)),]

# Divido il dataset in training e test set.
train <- floor(0.85 * nrow(dataset))
trainIdx <- sample(seq_len(nrow(dataset)), size = train)
trainingset <- dataset[trainIdx, ]
y_train <- as.matrix(trainingset[,10])

testset <- dataset[-trainIdx, ]
y_test <- as.matrix(testset[,10])
y_test <- replaceLabel(y)

# Cross-Validazione.
er <- K_folds(k, trainingset)

print(er)