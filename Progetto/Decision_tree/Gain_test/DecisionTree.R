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
classify_dtree_singleX <- function(stump, singleX) {
  # Controllo se è un nodo foglia.
  if(stump$isLeaf) {
    return (stump$class)
  }
  # Se non è un nodo foglia richiamo iterattivamente la funzione.
  if(singleX[stump$colNumber] > stump$threshold) {
    return (classify_dtree_singleX(stump$right, singleX))
  } else {
    return (classify_dtree_singleX(stump$left, singleX))
  }
}

# Classifico le istanze dell'albero complessivo.
classify_dtree <- function(stump, data) {
  y <- c()
  for (i in 1:nrow(data)) {
    y <- c(y, classify_dtree_singleX(stump, data[i,]))
  }
  y
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
gain <- function(data, datasetEntropy, w, y, c) {
  # Inizializzo i valori che mi serviranno.
  bestColNumber <- 1
  bestSep <- 1
  bestGain <- -100
  
  # Itero su tutte le colonne del dataset (tranne l'ultima) alla ricerca del guadagno migliore.
  # for(colNumber in 1:9) {
    # Scelgo un numero casuale tra le colonne 1 e 9.
    # colNumber <- sample(1:ncol(data), 1)
  colNumber <- c

    # Scelgo una colonna casualmente per fare un test.
    # colNumber <- sample(1:ncol(data), 1)
    # Prendo l'intera colonna.
    column <- data[,colNumber]
    # Numero di elementi in una colonna.
    N <- length(column)
    # Ordino gli elementi della colonna e ci accedo con $ix.
    sorted <- sort(column, index.return = TRUE)

    # Cerco il valore di soglia migliore confrontando direttamente tutte le entropie.
    for (sepN in 1:(N-1)) {
      # Faccio un controllo se la lunghezza della colonna è zero allora esco.
      if(length(sorted$x) == 0)
        next
      
      # Se i valori sono uguali, allora ritorno subito all'inizio del ciclo.
      if(sorted$x[sepN] == sorted$x[sepN+1])
        next
      
      # Calcolo la soglia confrontando tutte le entropie.
      weightedSum <- (sum(w[sorted$ix[1:sepN]])/sum(w)) * entropy(w[sorted$ix[1:sepN]], y[sorted$ix[1:sepN]]) +
        (sum(w[sorted$ix[(sepN+1):N]])/sum(w)) * entropy(w[sorted$ix[(sepN+1):N]], y[sorted$ix[(sepN+1):N]])
      
      # Questa è la formula per calcolare il guadagno.
      gain <- datasetEntropy - weightedSum

      # Memorizzo i parametri della colonna con entropia massima e della soglia.
      # if (gain > bestGain) {
        bestGain <- gain
        bestColNumber <- colNumber
        # Calcolo il valore della soglia.
        bestSep <- sorted$x[sepN] + ((sorted$x[sepN+1] - sorted$x[sepN]) / 2)
      # }
    }
  # }
  # Ritorno la lista dei valori che mi interessano.
  return(list(colNumber=bestColNumber, threshold=bestSep, gain=bestGain))
}

# Ritorna il primo livello dell'albero.
column_stump <- function(data, datasetEntropy, w, y, depth, colNumber, c) {
  # Al primo giro salto questo passaggio.
  if(length(unique(y))==1 || depth == 0) {
    # Calcolo l'etichetta che va per la maggiore nella foglia.
    mValue <- weightedMode(y, w)
    # Ritorno la lista di prima più la loro etichetta.
    return (list(isLeaf=TRUE, class=mValue))
  } else {
    # Calcolo il guadagno e la soglia con il metodo dell'information gain.
    partition <- gain(data, datasetEntropy, w, y, c)
    
    # Creo le foglie dell'albero (destra e sinistra).
    leftIdx  <- which(as.numeric(data[,partition$colNumber]) <= partition$threshold)
    rightIdx <- which(as.numeric(data[,partition$colNumber]) >  partition$threshold)
    
    # Richiamo iterattivamente la stessa funzione e gli passo la classe.
    return (list(isLeaf=FALSE,
                 left  = column_stump(data[leftIdx,  ], datasetEntropy, w[leftIdx],  y[leftIdx],  depth-1),
                 right = column_stump(data[rightIdx, ], datasetEntropy, w[rightIdx], y[rightIdx], depth-1),
                 colNumber = partition$colNumber,
                 threshold = partition$threshold,
                 gain = partition$gain)
            )
  }
}

# Estraggo il prossimo learner.
boost <- function(data, datasetEntropy, w, y, depth, c) {
  # Numero di membri del comitato.
  T <- 1
  # Vettore con i pesi dei classificatori.
  A <- rep(0, T)
  # t è il t-esimo membro del comitato scelto.
  B <- list()
  F <- rep(0, length(y))
  gamma <- 0
  # depth <- 4
  
  for(t in 1:T) {
    # Creo il ceppo dell'albero.
    stump <- column_stump(data, datasetEntropy, w, y, depth, colNumber, c)
    
    # Classifico le istanze.
    h <- classify_dtree(stump, data)
    # Indice degli esempi sbagliati.
    idx <- which(h*y<0)
    
    # Calcolo epsilon
    # epsilon <- sum(w[idx])
    # if(epsilon > 0.5+gamma || epsilon < 0.5-gamma) {
    # Memorizzo il migliore nel vettore B.
    B[[t]] <- stump
    # learner <- h
    # index <- idx
    # }
    # Controllo se epsilon è uguale a 0.5
    # else if (epsilon < 0.5+gamma && epsilon > 0.5-gamma) { 
    # break;
    # }
    
    # Peso di votazione del singolo membro del comitato.
    # A[t] <- (1/2)*log(((1-epsilon)/epsilon))
    # Aggiorno i pesi.
    # w[index] <- w[index] * exp(-y[index]*A[t]*learner[index])
    # w <- w/sum(w)
    # Genero il classificatore finale dato dalla combinazione lineare dei precedenti.
    # F <- F + (A[t]*learner)
  }
  
  # Ritorno il classificatore finale.
  return(list(committee = B, classifier = sign(F), weight = A))
}

AdaBoost <- function(data, numberOfRow, y, depth, c) {
  # Vettore dei pesi. Inizialmente sono tutti ad [1/m, 1/m, 1/m, ... ].
  w <- rep(1/numberOfRow, numberOfRow)
  
  # Calcolo l'entropia del dataset.
  datasetEntropy <- entropy(w, y)
  
  # Learner.
  adaBoost <- boost(data, datasetEntropy, w, y, depth, c)
  
  # F <- adaBoost$classifier
  
  return(adaBoost)
}