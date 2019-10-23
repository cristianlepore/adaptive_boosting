# CROSS VALIDAZIONE -------------------------------------------------------

# RICHIAMO FILES ESTERNI --------------------------------------------------
source("Perceptron.R")
source("Grafic.R")
# source("DecisionStump_InformationGain.R")

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
calculate_error <- function(w, data, y) {
  numElem <- length(y)
  error <- 0
  gap2 <- rep(0, numElem)
  
  # Itero su tutti gli elementi del dataset.
  for (i in 1:numElem) {
    # Se y*w*X è negativo, faccio un aggiornameto del modello.
    if (y[i] * Classify(data[i,], w) <= 0) {
      error <- error + 1
    }
    # Creo l'istogamma dell'errore.
    label <- Classify(data[i,], w)
    
    # Se predizione ed etichetta reale sono uguali, non aggiungo nulla.
    if(label == y[i])
      gap2[i] <- gap2[i] + 0
    # se predizione ed etichetta reale sono negative, aggiungo -1.
    else if(label-y[i] < 0)
      gap2[i] <- gap2[i] - 1
    # se predizione ed etichetta reale sono positive, aggiungo -1.
    else if(label-y[i] > 0)
      gap2[i] <- gap2[i] + 1
  }
  
  # Calcolo l'errore di validazione.
  # er <- k/length(y_validation) * error
  
  return(error)
}

# Ritorno l'errore sui tutta la cross validazione.
K_folds <- function(k, data, T) {
  # Creo 10 folds.
  # folds <- cut(seq(1,nrow(trainingset)), breaks=k, labels=FALSE)
  
  # Inizilizzo l'errore a zero.
  er <- 0
  
  # Divido i dati i Testset e Trainingset.
  # for(i in 1:1) {
  # Segmento i dati.
  # validationIdx <- which(folds==i, arr.ind=TRUE)
  # validationset <- data[validationIdx, ]
  # trainingset <- data[-validationIdx, ]
  
  # Trainingset senza la colonna delle classi.
  training <- as.matrix(data[,-10])
  # validation <- as.matrix(validationset[,-10])
  
  # Colonna delle etichette e la prendo come vettore per comodità.
  y_training <- as.matrix(data[,10])
  # y_validation <- as.matrix(validationset[,10])
  
  # Rimpiazzo le etichette.
  y_training <- replaceLabel(y_training)
  # y_validation <- replaceLabel(y_validation)
  # Numero di righe del trainingset.
  # numberOfRow_training <- nrow(y_training)
  
  # Ritorno il classificatore dopo che faccio girare il percettrone.
  list <- boost(training, y_training, T)
  
  # Errore sul singolo validation set.
  # er <- calculate_error(w, validation, y_validation, k)
  # }
  
  # Medio tutti gli errori dei validation set per ottenere l'errore di Cross-validazione.
  # average_error <- 1/k * er
  
  return(list)
}


# CODICE MAIN -------------------------------------------------------------
# Definisco il numero di folds che dividerà il mio dataset. Deve essere almeno >=2
k <- 10
# Membri del comitato.
T <- 10

# Mischio le righe del dataset.
dataset <- dataset[sample(nrow(dataset)),]

# Divido il dataset in training e test set.
train <- floor(0.85* nrow(dataset))
# trainIdx <- sample(seq_len(nrow(dataset)), size = train)
trainingset <- dataset[1:train, ]

# Aggiungo la colonna degli id al trainingset
# id <- c(1:nrow(trainingset))
# trainingset <- cbind(trainingset, id)

y_train <- as.matrix(trainingset[,10])
testset <- dataset[(train+1):nrow(dataset), ]
y_test <- as.matrix(testset[,10])
y_test <- replaceLabel(y_test)
testset <- as.matrix(testset[,-10])
y_train <- replaceLabel(y_train)

errors <- c()
for(t in 1:T) {
  # Cross-Validazione.
  list <- K_folds(k, trainingset, T)
  errors <- c(errors, calculate_error(list$model, cbind(testset, 1), y_test))
}

# Costruisco il grafo.
# result <- create_graph(list$gap)

# Verifico sul testset.
# gap2 <- calculate_error(list$model, testset, y_test)

# result2 <- create_graph(gap2)

# ISTOGRAMMA DELL'ERRORE --------------------------------------------------

# Prendo solo una piccola parte casuale degli esempi del traningset.
# gap <- gap[sample(0.06*length(gap))]

# counts <- rbind(result, result2)
# Creo il grafico test error
# barplot(counts,
        # ylim=range(c(result, result2)),
#         type = "bar",
#         col=c("blue","red"),
#         lwd = 1,
#         xlab = "Errore (output-y)",
#         ylab = "Istanze",
#         main = "Istogramma dell'errore"
# )

# legend("topright", legend=c("Train", "Test"),
#        col=c("blue", "red"), lwd=1:1, cex=1)