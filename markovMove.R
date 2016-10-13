probabilityFromNormalDistribution <- function(value, mean, std) {
  interval = 0.5
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
      print(waterhole)
      if (!is.na(observations[[4]]) && !is.na(observations[[5]])) {
        if (observations[[4]] == waterhole || observations[[5]] == waterhole) {
          previousProbabilities[waterhole] = 0 #0 if a tourist is there
          next()
        }
      }
      previousProbabilities[waterhole] = 1 #1/watherholes without a tourist if no tourist
      possibleWaterholes = possibleWaterholes + 1
    }
    print(previousProbabilities)
    previousProbabilities = sapply(previousProbabilities, function(x) x/possibleWaterholes)
  }
  
  probas = vector(mode="double", length=40)
  #if a tourist was eaten this turn
  if (!is.nan(observations[[4]]) && observations[[4]] < 0) {
    croc = -observations[[4]]
    for (waterhole in 1:40) {
      probas[waterhole] = 0
    }
    probas[croc] = 1
  }
  else if (!is.nan(observations[[5]]) && observations[[5]] < 0) {
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
  #probas: list of probabilities for each hole
  #positions: backpacker 1, bacpacker 2, ranger
  #edges: relation of one hole to another
  
  #find max value of probability
  maxPos = 1
  maxVal = 0
  minVal = 1
  for (i in 1:40) {
    if(!is.nan(probas[i]) && maxVal < probas[i]){
      maxPos = i
      maxVal = probas[i]
    }
    if(!is.nan(probas[i]) && minVal > probas[i] && probas[i] != 0)
      minVal = probas[i]
  }
  
  print (probas)
  print(maxPos)
  print(maxVal)
  print(minVal)
  
  if(minVal == maxVal){
    nmove = sample(getOptions(positions[3],edges),1)
    move = c(nmove,0)
    probas[nmove] = 0
    print(nmove)
    print("random")
  }else{
    #find path to maxProb
    #tmp = null
    vec = as.vector(t(edges))
    megraph = make_graph(vec, directed=FALSE)
    tmp = graph.bfs(megraph, root=positions[3], neimode='all', order=TRUE, father=TRUE)
    
    #print(vec)
    #print(megraph)
    #print(tmp$order)
    #print(tmp$father)
    #print(maxPos)
    #print(maxVal)
    goal = match(c(maxPos), tmp$order)
    #print("goal")
    #print(goal)
    #h <- graph(rbind(tmp$order, tmp$father[tmp$order])[,-1], directed=FALSE )
    #print(rbind(tmp$order, tmp$father[tmp$order]))
    #print(tmp$father[goal])
    path = c()
    j = 0
    #while(positions[3] != tmp$father[goal]){
    # path[j] = tmp$father[goal]
    #goal = match(c(path[j]), tmp$order)
    #}
    #print(path)
    
    if(goal > 1){
      move = c(as_ids(tmp$order[2]), 0)
      probas[as_ids(tmp$order[2])] = 0
    }else{
      move = c(0,0)
      probas[positions[3]] = 0
    }  
    print("path")
  }
  print(move)
  
  return(list("move"=move, "probas"=probas))
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
  #print(positions)
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
  moveResult = makeMove(probas, positions, edges)
  moveInfo[['moves']] = moveResult$move
  #moveInfo[['moves']] = c(0, 0)
  moveInfo[['mem']][["previousProbabilities"]] = moveResult$probas
  return(moveInfo)
}