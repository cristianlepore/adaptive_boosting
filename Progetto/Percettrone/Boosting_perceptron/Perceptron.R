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

# Classifico tutto il mio dataset.
Classify_data <- function(data, w) {
  # Inizializzo il vettore a zero.
  y <- c()
  
  # Itero sui dati del dataset.
  for(i in 1:nrow(data)) {
    if(Classify(data[i,], w) <= 0)
      y <- c(y, -1)
    else
      y <- c(y, +1)
  }
  
  # Ritorno il vettore che ho creato di etichette calcolate dal membro del comitato.
  return(y)
}

# Implemento il percettrone.
perceptron <- function(data, y) {
  # Definisco il vettore iniziale dei pesi tutti a zero.
  w <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, -2 )

  # Numero di righe del dataset.
  numElem <- nrow(data)
  
  # Definisco questa variabile che mi servirà.
  epochs <- 20
  j <- 0
  gap <- rep(0, numElem)

  # Itero finchè l'iperpiano non raggiunge la convergenza.
  while (j<epochs) {
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
      label <- Classify(data[i, ], w)
      
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
    if(misclassfied == FALSE)
      break
    j <- j+1
  }

  # Ritorno il modello.
  # Istogramma dell'errore.
  gap <- gap/epochs
  # Ritorno il modello.
  return(list(
    model = w,
    gap = gap))
}

# Implemento il boosting con percettrone.
boost <- function(data, y, T) {
  # Aggiungo una coordinata.
  data <- cbind(data, 1)

  # Numero di righe di data.
  numberOfRow <- nrow(data)
  # Vettore dei pesi. Inizialmente sono tutti ad [1/m, 1/m, 1/m, ... ].
  D <- rep(1/numberOfRow, numberOfRow)
  # Numero di membri del comitato.
  # T <- 1
  # Vettore con i pesi dei classificatori.
  A <- rep(0, T)
  # Classificatore finale. Inizialmente a zero.
  F <- rep(0, ncol(data))
  gamma <- 0
  
  # Itero su tutti i membri del comitato.
  for(t in 1:T) {
    # sorted <- sort(D, decreasing = TRUE, index.return = TRUE)
    # data <- data[sorted$ix,]
    # D <- D[sorted$ix]
    # y <- y[sorted$ix]
    
    # Estraggo i dati basandomi sulla nuova distribuzione di probabilità e  riordino il data (dataset).
    num.to.sample <- numberOfRow
    sampleIdx <- sample(1:numberOfRow, size=num.to.sample, replace=FALSE, prob=D)
    data <- data[sampleIdx,]
    D <- D[sampleIdx]
    y <- y[sampleIdx]
    
    # Ritorno il percettrone calcolato del t-esimo membro del comitato.
    list <- perceptron(data, y)
    w <- list$model
    
    # Classifico i miei esempi.
    h <- Classify_data(data, w)
    
    # Indice degli esempi sbagliati.
    idx <- which(h*y<0)

    # Calcolo epsilon
    epsilon <- sum(D[idx])
    # Se epsilon_i è uguale a 0.5 termino.
    if(!(epsilon > 0.5+gamma || epsilon < 0.5-gamma)) {
      # Esco dal boosting.
      break;
    }
    
    # Peso di votazione del singolo membro del comitato.
    A[t] <- (1/2)*log(((1-epsilon)/epsilon))

    # Aggiorno i pesi e normalizzo.
    D[idx] <- D[idx] * exp(-y[idx]*A[t]*h[idx])
    D <- D/sum(D)
    
    # Genero il classificatore finale dato dalla combinazione lineare dei precedenti.
    F <- F + (A[t]*w)
  }
  
  # Ritorno il classificatore finale dato dalla combinazione di tutti i classificatori base.
  return(list(
              model = F,
              gap = list$gap)
         )
}


# CODICE MAIN -------------------------------------------------------------

# Dati da usare.
# data <- dataset[,-10]
# Etichette.
# y <- as.matrix(dataset[ ,10])
# y <- replaceLabel(y)

# boost(data, y)




# sorted <- sort(D, decreasing = TRUE, index.return = TRUE)
# data <- data[sorted$ix,]
