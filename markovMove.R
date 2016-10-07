probabilityFromNormalDistribution <- function(value, mean, std) {
  interval = 0.005
  lowcut = value - interval
  highcut = value + interval
  
  return(pnorm(highcut, mean = mean, sd = std) - pnorm(lowcut, mean = mean, sd = std))
}

computeProbability <- function(waterhole, observations, previousProbabilities) {
  #returns PROPORTIONAL probability that Croc is in a given waterhole
  #waterhole = 'the number of the waterhole'
  #observations = c('salinity reading from Croc', 'phosphate reading from Croc', 'nitrogen reading from Croc', 'position of tourist 1', 'position of tourist 2', 'position of player')
  #previousProbabilities = 'the vector returned by computeProbabilities at the previous time point
  
  #Computing the emission probilities for the provided set of observations
  #The different observations are independent
  #emission = vector(mode="double",length = 40)
  #for (i in 1:40) {
    
  #}
  
  return(waterhole)
}

computeProbabilities <- function(observations, previousProbabilities) {
  #returns the vector of probabilities that Croc is in each waterhole
  #observations = c('salinity reading from Croc', 'phosphate reading from Croc', 'nitrogen reading from Croc', 'position of tourist 1', 'position of tourist 2', 'position of player')
  #previousProbabilities = 'the vector returned by computeProbabilities at the previous time point
  
  #If this is the first turn
  if (length(previousProbabilities) != 40) {
    previousProbabilities = vector(mode="double", length=40)
    possibleWaterholes = 0
    for (waterhole in 1:40) {
      if (observations[[4]] == waterhole || observations[[5]] == waterhole) {
        previousProbabilities[waterhole] = 0 #0 if a tourist is there
        next()
      }
      previousProbabilities[waterhole] = 1 #1/watherholes without a tourist if no tourist
      possibleWaterholes = possibleWaterholes + 1
    }
    previousProbabilities = sapply(previousProbabilities, function(x) x/possibleWaterholes)
  }
  
  probas = vector(mode="double", length=40)
  for (waterhole in 1:40) {
    #compute the proportional probability for each waterhole
    probas[waterhole] = computeProbability(waterhole, observations, previousProbabilities)
  }
  
  totalProba = sum(probas)
  probas = sapply(probas, function(x) x/totalProba)
  
  return(probas)
}

markovMove <- function(moveInfo, readings, positions, edges, probs) {
  #moveInfo = list(moves = c('the two moves to make'), mem = list('any info we want to store')
  #readings = c('salinity reading from Croc', 'phosphate reading from Croc', 'nitrogen reading from Croc')
  #positions = c('position of tourist 1', 'position of tourist 2', 'position of player')
  #edges = matrix('edges paths between waterholes, first column is one extremity of the edge, second column is the other extremity, every row is one edge')
  #probs = list(salinity = matrix('mean and standard deviation of readings for salinity, each row is a waterhole, first column is mean, second column is standard deviation'), phosphate = matrix('mean and standard deviation of readings for phosphate, each row is a waterhole, first column is mean, second column is standard deviation'), nitrogen = matrix('mean and standard deviation of readings for nitrogen, each row is a waterhole, first column is mean, second column is standard deviation'))

  #print(moveInfo)
  #print(readings)
  #print(positions)
  #print(edges)
  #print(probs)
  
  #Merging the two observations vectors
  observations = vector(mode="double", length=6)
  for (i in 1:3) {
    observations[i] = readings[[i]]
    observations[i + 3] = positions[[i]]
  }
  #Getting the previous probabilities
  if (length(moveInfo["mem"]) == 0) {
    moveInfo["mem"]["previousProbabilities"] = 0
  }
  previousProbabilities = moveInfo["mem"]["previousProbabilities"]
  #Computing the probabilities of Croc being in each waterhole
  probas = computeProbabilities(observations, previousProbabilities)
  ################################
  #END OF PROBABILITY CALCULATION#
  ################################
  
  
  
  moveInfo[['moves']] = c(0, 0)
  return(moveInfo)
}