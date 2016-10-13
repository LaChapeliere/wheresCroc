probabilityFromNormalDistribution <- function(value, mean, std) {
  interval = 0.005
  lowcut = value - interval
  highcut = value + interval
  
  return(pnorm(highcut, mean = mean, sd = std) - pnorm(lowcut, mean = mean, sd = std))
}

computeProbability <- function(waterhole, observations, previousProbabilities, probs, neighbors) {
  #returns PROPORTIONAL probability that Croc is in a given waterhole
  #waterhole = 'the number of the waterhole'
  #observations = c('salinity reading from Croc', 'phosphate reading from Croc', 'nitrogen reading from Croc', 'position of tourist 1', 'position of tourist 2', 'position of player')
  #previousProbabilities = 'the vector returned by computeProbabilities at the previous time point
  #probs = list(salinity = matrix('mean and standard deviation of readings for salinity, each row is a waterhole, first column is mean, second column is standard deviation'), phosphate = matrix('mean and standard deviation of readings for phosphate, each row is a waterhole, first column is mean, second column is standard deviation'), nitrogen = matrix('mean and standard deviation of readings for nitrogen, each row is a waterhole, first column is mean, second column is standard deviation'))
  #neighbors = 'definition of the network of waterhole by list of lists of neighbors
  
  #Computing the emission probability for the provided set of observations
  #The different observations are independent
  emission = probabilityFromNormalDistribution(observations[1], probs[["salinity"]][waterhole,1], probs[["salinity"]][waterhole,2])
  emission = emission * probabilityFromNormalDistribution(observations[2], probs[["phosphate"]][waterhole,1], probs[["phosphate"]][waterhole,2])
  emission = emission * probabilityFromNormalDistribution(observations[3], probs[["nitrogen"]][waterhole,1], probs[["nitrogen"]][waterhole,2])

  #Compute the probability of Croc reaching the waterhole considering what we know of the previous state
  moving = 0
  for (n in length(neighbors[waterhole])) {
    neighbor = neighbors[[waterhole]][n]
    moving = moving + (1.0 / length(neighbors[[neighbor]])) * previousProbabilities[neighbor]
  }
  
  return(emission * moving)
}

computeProbabilities <- function(observations, previousProbabilities, probs, neighbors) {
  #returns the vector of probabilities that Croc is in each waterhole
  #observations = c('salinity reading from Croc', 'phosphate reading from Croc', 'nitrogen reading from Croc', 'position of tourist 1', 'position of tourist 2', 'position of player')
  #previousProbabilities = 'the vector returned by computeProbabilities at the previous time point
  #probs = list(salinity = matrix('mean and standard deviation of readings for salinity, each row is a waterhole, first column is mean, second column is standard deviation'), phosphate = matrix('mean and standard deviation of readings for phosphate, each row is a waterhole, first column is mean, second column is standard deviation'), nitrogen = matrix('mean and standard deviation of readings for nitrogen, each row is a waterhole, first column is mean, second column is standard deviation'))
  #neighbors = 'definition of the network of waterhole by list of lists of neighbors
  
  #If this is the first turn
  if (sum(previousProbabilities) == 0) {
    previousProbabilities = vector(mode="double", length=40)
    possibleWaterholes = 0
    for (waterhole in 1:40) {
      if (!is.na(observations[[4]]) && !is.na(observations[[5]])) {
        if (observations[[4]] == waterhole || observations[[5]] == waterhole) {
          previousProbabilities[waterhole] = 0 #0 if a tourist is there
          next()
        }
      }
      previousProbabilities[waterhole] = 1 #1/watherholes without a tourist if no tourist
      possibleWaterholes = possibleWaterholes + 1
    }
    previousProbabilities = sapply(previousProbabilities, function(x) x/possibleWaterholes)
  }
  
  probas = vector(mode="double", length=40)
  #if a tourist was eaten this turn
  if (!is.nan(observations[[4]]) || observations[[4]] < 0) {
    croc = -observations[[4]]
    for (waterhole in 1:40) {
      probas[waterhole] = 0
    }
    probas[croc] = 1
  }
  else if (!is.nan(observations[[5]]) || observations[[5]] < 0) {
    croc = -observations[[5]]
    for (waterhole in 1:40) {
      probas[waterhole] = 0
    }
    probas[croc] = 1
  }
  #Else
  else {
    for (waterhole in 1:40) {
      #compute the proportional probability for each waterhole
      probas[waterhole] = computeProbability(waterhole, observations, previousProbabilities, probs, neighbors)
    }
  }
  
  totalProba = sum(probas)
  probas = sapply(probas, function(x) x/totalProba)
  
  return(probas)
}

makeMove <- function(probas, positions, edges) {
  #probas: list of possibilities for each hole
  #positions: croc, backpacker 1, bacpacker 2, ranger
  #edges: relation of one hole to another
  
  #find max value of probability
  maxProb = 0
  maxs = 0
  for (i in 1:40) {
    print(maxProb)
    print(probas[i])
    if(maxProb < probas[i]){
      maxProb = i
      maxs = probas[i]
    }
  }
  
  #find path to maxProb
  #tmp = null
  vec = as.vector(t(edges))
  megraph = make_graph(vec, directed=FALSE)
  tmp = graph.bfs(megraph, root=positions[3], neimode='all', order=TRUE, father=TRUE)
  
  #print(vec)
  #print(megraph)
  print (probas)
  print(tmp)
  print(maxProb)
  print(maxs)
  goal = match(c(maxProb), tmp$order)
  #print("goal")
  #print(goal)
  #if(goal >= 2){
   # move = c(tmp$order[2], tmp$order[3])
    #probas[tmp$order[2]] = 0
    #probas[tmp$order[3]] = 0
  #}else if(goal == 1){
   # move = c(tmp$order[2], 0)
  #  probas[tmp$order[2]] = 0
  if(goal > 1){
    print(tmp$order[2])
    #getOptions(positions[3],edges)
    #move = c(0,0)
    move = c(tmp$order[2], 0)
    probas[tmp$order[2]] = 0
    #probas[tmp$order[3]] = 0
  }else{
    move = c(0,0)
    probas[positions[3]] = 0
  }
  #print(move)
    
  return(move)
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
  options=getOptions(positions[3],edges)
  print("Move 1 options (plus 0 for search):")
  #print(options)
  print(positions)
  ###############################
  #PRETREATEMENT OF NETWORK INFO#
  ###############################
  #neighbors is a list of list containing the neighbors for each waterhole
  neighbors = list()
  #Each waterhole is its own neighbor
  for (i in 1:40) {
    neighbors[i] = list(i)
  }
  for (edgeNum in 1:nrow(edges)) {
    firstPoint = edges[edgeNum,1]
    secondPoint = edges[edgeNum,2]
    neighbors[[firstPoint]][length(neighbors[[firstPoint]]) + 1] = secondPoint
    neighbors[[secondPoint]][length(neighbors[[secondPoint]]) + 1] <- firstPoint
  }
  ###################################
  #END PRETREATEMENT OF NETWORK INFO#
  ###################################
  
  
  #########################
  #PROBABILITY CALCULATION#
  #########################
  #Merging the two observations vectors
  observations = vector(mode="double", length=6)
  for (i in 1:3) {
    observations[i] = readings[[i]]
    observations[i + 3] = positions[[i]]
  }
  #Getting the previous probabilities
  if (length(moveInfo[["mem"]]) == 0) {
    moveInfo[["mem"]][["previousProbabilities"]] = vector(mode="double", length=40)
  }
  previousProbabilities = moveInfo[["mem"]][["previousProbabilities"]]
  #Computing the probabilities of Croc being in each waterhole
  probas = computeProbabilities(observations, previousProbabilities, probs, neighbors)
  ################################
  #END OF PROBABILITY CALCULATION#
  ################################
  
  #To Michael and Asa: Don't forget to ajust the probas if you check a waterhole, to pass the vector to the next turn
  #print(probas)
  #print(probas[2])
  moveInfo[['moves']] = makeMove(probas, positions, edges)
  #moveInfo[['moves']] = c(0, 0)
  moveInfo[['mem']][["previousProbabilities"]] = probas
  return(moveInfo)
}