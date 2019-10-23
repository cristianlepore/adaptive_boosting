# IMPELEMENTING A WEAK LEARNER - Decision Stumps --------------------------

# DICHIARAZIONE DELLE FUNZIONI --------------------------------------------

# Assegno l'etichetta alle foglie.
weightedMode <- function(y, w) {
  negative <- which(y==unique(y)[1])
  positive <- which(y!=unique(y)[1])
  if(sum(w[negative]) > sum(w[positive])) {
    return (unique(y)[1])
  } else {
    return (unique(y)[2])
  }
}

# Classifico le istanze.
classify <- function(stump, column) {
  # Inizializzo il vettore a zero.
  y <- c()
  for (i in 1:length(column)) {
    # Ritorno un vettore con -1 o +1 come etichette della foglia.
    if(as.numeric(column[i]) > stump$threshold) {
      y <- c(y, stump$right$class)
    }
    else
      y <- c(y, stump$left$class)
  }
  return(y)
}

# Restituisce l'entropia del dataset (solo ultima colonna).
entropy <- function(w, y) {
  # Prendo la posizione delle istanze con etichetta 2 e 4.
  posIdx <- which(y==unique(y)[1])
  negIdx <- which(y!=unique(y)[1])
  
  # Calcolo la probabilità delle etichette positive e negative.
  posProb <- sum(w[posIdx])/sum(w)
  negProb <- sum(w[negIdx])/sum(w)
  
  # Setto l'entropia inizialmente a zero.
  entropy <- 0
  
  # Verifiche se l'entropia positiva o negativa è = 0.
  # Calcolo l'entropia del dataset.
  if(posProb > 0) {
    entropy <- entropy + posProb*log2(posProb)
  }
  if(negProb > 0) {
    entropy <- entropy + negProb*log2(negProb)
  }
  entropy <- -entropy
  
  # Ritorno l'entropia del dataset.
  return (entropy)
}

# Restituisce la colonna con guadagno massimo.
gain <- function(data, datasetEntropy, w, columnNumber, y) {
  # Inizializzo i valori che mi serviranno.
  bestColNumber <- 1
  bestSep <- 1
  bestGain <- -100
  
  # Prendo l'intera colonna.
  column <- data[,columnNumber]
  # Numero di elementi in una colonna.
  N <- length(column)
  # Ordino gli elementi della colonna e ci accedo con $ix.
  sorted <- sort(column, index.return = TRUE)
  
  # Cerco il valore di soglia migliore confrontando direttamente tutte le entropie.
  for (sepN in 1:(N-1)) {
    # Se i valori sono uguali, allora salto.
    if(sorted$x[sepN] == sorted$x[sepN+1])
      next
    
    # Calcolo la soglia confrontando tutte le entropie.
    weightedSum <- (sum(w[sorted$ix[1:sepN]])/sum(w)) * entropy(w[sorted$ix[1:sepN]], y[sorted$ix[1:sepN]]) +
      (sum(w[sorted$ix[(sepN+1):N]])/sum(w)) * entropy(w[sorted$ix[(sepN+1):N]], y[sorted$ix[(sepN+1):N]])
    
    # Questa è la formula per calcolare il guadagno.
    gain <- datasetEntropy - weightedSum
    
    # Memorizzo i parametri della colonna con entropia massima e della soglia.
    if (gain > bestGain) {
      bestGain <- gain
      bestColNumber <- columnNumber
      # Calcolo il valore della soglia.
      bestSep <- sorted$x[sepN] + ((sorted$x[sepN+1] - sorted$x[sepN]) / 2)
    }
  }
  # Ritorno la lista dei valori che mi interessano.
  return(list(colNumber=bestColNumber, threshold=bestSep, gain=bestGain))
}

# Ritorna il primo livello dell'albero.
column_stump <- function(data, datasetEntropy, w, y, depth, colNumber) {
  # Al primo giro salto questo passaggio.
  if(depth == 0) {
    # Calcolo l'etichetta che va per la maggiore nella foglia.
    mValue <- weightedMode(y, w)
    # Ritorno la lista di prima più la loro etichetta.
    return (list(data, w, y, class=mValue))
  } else {
    # Calcolo il guadagno e la soglia con il metodo dell'information gain.
    partition <- gain(data, datasetEntropy, w, colNumber, y)
    
    # Creo le foglie dell'albero (destra e sinistra).
    leftIdx  <- which(as.numeric(data[,partition$colNumber]) <= partition$threshold)
    rightIdx <- which(as.numeric(data[,partition$colNumber]) >  partition$threshold)
    
    # Decremento il contatore.
    depth <- depth-1

    # Richiamo iterattivamente la stessa funzione e gli passo la classe.
    return (list(left  = column_stump(data[leftIdx,  partition$colNumber], datasetEntropy, w[leftIdx],  y[leftIdx],  depth),
                 right = column_stump(data[rightIdx, partition$colNumber], datasetEntropy, w[rightIdx], y[rightIdx], depth),
                 colNumber = partition$colNumber,
                 threshold = partition$threshold))
  }
}

# Estraggo il prossimo learner.
boost <- function(data, datasetEntropy, w, y) {
  # Numero di membri del comitato.
  T <- 5
  # Vettore con i pesi dei classificatori.
  A <- rep(0, T)
  # t è il t-esimo membro del comitato scelto.
  B <- list()
  F <- rep(0, length(y))
  gamma <- 0

  for(t in 1:T) {
    absPreviuosEpsilon <- 0
    exit <- TRUE
    # Itero su tutte le colonne del dataset alla ricerca della colonna con epsilon più distante da 0.5.
    for (colNumber in 1:ncol(data)) {
      # Creo il ceppo dell'albero.
      stump <- column_stump(data, datasetEntropy, w, y, 1, colNumber)

      # Classifico le istanze.
      h <- classify(stump, data[ ,stump$colNumber])
      # Indice degli esempi sbagliati.
      idx <- which(h*y<0)
      
      # Calcolo epsilon
      epsilon <- sum(w[idx])
      if(epsilon != 0.5 && abs(0.5-epsilon) > absPreviuosEpsilon && (epsilon > 0.5+gamma || epsilon < 0.5-gamma)) {
        absPreviuosEpsilon <- abs(epsilon-0.5)
        # Memorizzo il migliore nel vettore B.
        B[[t]] <- stump
        learner <- h
        index <- idx
        exit <- FALSE
      } 
    }
    if(exit)
      break;

    # Peso di votazione del singolo membro del comitato.
    A[t] <- (1/2)*log(((1-epsilon)/epsilon))
    # Aggiorno i pesi.
    w[index] <- w[index] * exp(-y[index]*A[t]*learner[index])
    w <- w/sum(w)
    # Genero il classificatore finale dato dalla combinazione lineare dei precedenti.
    F <- F + (A[t]*learner)
  }
  
  # Ritorno il classificatore finale.
  return(list(committee = B, classifier = sign(F), weight = A))
}

AdaBoost <- function(data, numberOfRow, y) {
  # Vettore dei pesi. Inizialmente sono tutti ad [1/m, 1/m, 1/m, ... ].
  w <- rep(1/numberOfRow, numberOfRow)
  
  # Calcolo l'entropia del dataset.
  datasetEntropy <- entropy(w, y)
  
  # Learner.
  adaBoost <- boost(data, datasetEntropy, w, y)
  
  F <- adaBoost$classifier
  
  return(adaBoost)
}