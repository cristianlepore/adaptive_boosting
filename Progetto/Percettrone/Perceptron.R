#DEFINIZIONE DELLE FUNZIONI. ---------------------------------------------

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

# Il classificatore ritorno il segno di W trasposto * X.
Classify <- function(singleX, w) {
  return(sign(as.numeric(singleX) %*% as.numeric(w)))
}

# Implemento il percettrone.
perceptron <- function(data, y) {
  # Definisco il vettore iniziale dei pesi tutti a zero.
  w <- rep(0, 9)
  # w <- matrix(w,1,ncol(data))
  # w <- c(runif(ncol(data),0,0.0002))
  
  # Numero di righe del dataset.
  numElem <- nrow(data)
  
  # Definisco questa variabile che mi servirà.
  misclassfied <- TRUE
  epochs <- 20
  j <- 0
  gap <- rep(0, numElem)
  
  # Itero finchè l'iperpiano non raggiunge la convergenza.
  while (j<epochs) {
    # Mischio i dati del trainingset ad ogni epoca. Non è obbligatorio farlo.
    idx <- sample(nrow(data))
    data <- data[idx,]
    y <- y[idx]
    
    # Setto la variabile inizialmente a FALSE.
    misclassfied <- FALSE
    
    # Itero su tutti gli elementi del dataset.
    for (i in 1:numElem) {
      
      # Se y*w*X è negativo, faccio un aggiornameto del modello.
      if (y[i] * Classify(data[i,], w) <= 0) {
        w <- w + y[i] * data[i,]
        misclassfied <- TRUE
      }
      
      # Creo l'istogamma dell'errore.
      label <- Classify(data[i,], w)
      
      # Se predizione ed etichetta reale sono uguali, non aggiungo nulla.
      if(label == y[i])
        gap[i] <- gap[i] + 0
      # se predizione ed etichetta reale sono negative, aggiungo -1.
      else if(label-y[i] < 0)
        gap[i] <- gap[i] - 1
      # se predizione ed etichetta reale sono positive, aggiungo -1.
      else if(label-y[i] > 0)
        gap[i] <- gap[i] + 1
      gap
    }
    
    # Se raggiungo la convergenza, mi fermo.
    if(misclassfied == FALSE) {
      print("converge")
      break
    }
    j <- j+1
  }
  
  # Istogramma dell'errore.
  gap <- gap/epochs
  # Ritorno il modello.
  return(list(
    model = w ,
    gap = gap))
}


# # Dati da usare.
# data <- dataset[,-10]
# # Etichette.
# y <- as.matrix(dataset[ ,10])
# y <- replaceLabel(y)
# 
# perceptron(data, y)