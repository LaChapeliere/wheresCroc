computeProbability <- function(waterhole, observations, previousProbabilities) {
  #returns PROPORTIONAL probability that Croc is in a given waterhole
  #waterhole = 'the number of the waterhole'
  #observations = c('salinity reading from Croc', 'phosphate reading from Croc', 'nitrogen reading from Croc', 'position of tourist 1', 'position of tourist 2', 'position of player')
  #previousProbabilities = 'the vector returned by computeProbabilities at the previous time point
  
  #return(waterhole)
}

computeProbabilities <- function(observations, previousProbabilities) {
  #returns the vector of probabilities that Croc is in each waterhole
  #observations = c('salinity reading from Croc', 'phosphate reading from Croc', 'nitrogen reading from Croc', 'position of tourist 1', 'position of tourist 2', 'position of player')
  #previousProbabilities = 'the vector returned by computeProbabilities at the previous time point
  
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
  
  moveInfo[['moves']] = c(0, 0)
  return(moveInfo)
}