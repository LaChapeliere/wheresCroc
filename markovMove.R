#Our main function is named 'markovMove'

#Set the flagcheck to 0, use counter the turn
flagCheck <<- 0

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
  if (sum(previousProbabilities) == 0 || !is.nan(sum(previousProbabilities))) {
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
  if (!is.na(observations[[4]]) && observations[[4]] < 0) {
    croc = -observations[[4]]
    for (waterhole in 1:40) {
      probas[waterhole] = 0
    }
    probas[croc] = 1
  }
  else if (!is.na(observations[[5]]) && observations[[5]] < 0) {
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

#BFS-based Movement
bfsBased <- function(probas, positions, edges) {
  #probas = 'vector holding the probability distribution of Croc's position in the network'
  #positions = c('position of tourist 1', 'position of tourist 2', 'position of player')
  #edges = matrix('edges paths between waterholes, first column is one extremity of the edge, second column is the other extremity, every row is one edge')
  
  #find max value of probability
  maxPos = 1
  maxVal = 0
  for (i in 1:40) {
    if(!is.na(probas[i]) && maxVal < probas[i]){
      maxPos = i
      maxVal = probas[i]
    }
  }
  
  #make path to all nodes using bfs
  vec = as.vector(t(edges))
  megraph = make_graph(vec, directed=FALSE)
  tmp = graph.bfs(megraph, root=positions[3], neimode='all', order=TRUE, father=TRUE, dist=TRUE)
  
  #find the highest probability position in bfs
  goal = match(c(maxPos), tmp$order)
  pathFound = c()
  
  #if the goal is the same as ranger's position, check the waterhole
  if(as_ids(tmp$order[goal]) == positions[3]){
    move = c(0,0)
    probas[positions[3]] = 0
  }else{
    #traceback from the goal to ranger
    comp = as_ids(tmp$father[tmp$order[goal]])
    pathFound[1] = maxPos
    j = 2
    while(positions[3] != comp){
      pathFound[j] = comp
      temp = match(c(comp), tmp$order)
      comp = as_ids(tmp$father[tmp$order[temp]])
      j = j + 1
    }
    
    if(length(pathFound) >= 2){
      #if the path to the goal has more than 2 or more of nodes
      move = c(pathFound[j-1], pathFound[j-2])
    }else if(length(pathFound) == 1){
      #if ranger can access goal directly
      move = c(pathFound[j-1],0)
      probas[pathFound[j-1]] = 0
    } 
  }
  return(list("move"=move, "probas"=probas))
}

#Local Search Movement
makeMoveLocalSearch <- function(probas, positions, edges) {
  #probas = 'vector holding the probability distribution of Croc's position in the network'
  #positions = c('position of tourist 1', 'position of tourist 2', 'position of player')
  #edges = matrix('edges paths between waterholes, first column is one extremity of the edge, second column is the other extremity, every row is one edge')
  
  #find max value of probability
  
  #Check if the probas not NAN
  if(all(!is.nan(probas)))
  {
    maxProb = 0
    maxProb2 = 0
    tempData = 100
    flag = 0
    
    options=getOptions(positions[3],edges)
    
    ######Move for the First
    for(i in 1:length(options))
    {
      #Find the best movement for ranger from the Option ranger have it.
      if(probas[options[i]] != 0 && !is.nan(probas[options[i]]))
      {
        if(maxProb < probas[options[i]])
          maxProb = options[i]
        flag = flag + 1
      }
    }
    # If all the value of probabilty of Ranger option movement become 0
    # find the highest probability in the plots
    if(flag == 0)
    {
      highProbabs = 0
      for(j in 1:40)
      {
        if(highProbabs < probas[j] && j != positions[3])
          highProbabs = j
      }
      
      #Find the fastest way from the option ranger has to the high probability waterhole
      tempI = 0
      for(i in 1:length(options))
      {
        if(options[i] != positions[3])
        {
          tempValue = abs(highProbabs-options[i])
          
          if(tempValue < tempData)
          {
            tempData = tempValue
            tempI = i
          }
        }
      }
      maxProb = options[tempI]
      
    }
    
    ########Move for the second
    flag2 = 0
    tempData = 100
    # get the option movement from the first movement
    optionsStep2 = getOptions(maxProb,edges)
    
    #Find the best movement for ranger from the Option ranger have it.
    for(i in 1:length(optionsStep2))
    {
      if(probas[optionsStep2[i]] != 0 && !is.nan(optionsStep2[options[i]]))
      {
        if(maxProb2 < probas[optionsStep2[i]])
          maxProb2 = optionsStep2[i]
        flag2 = flag2 + 1
      }
    }
    
    # If all the value of probabilty of Ranger option movement become 0
    # find the highest probability in the plots
    if(flag2 == 0)
    {
      highProbabs = 0
      for(j in 1:40)
      {
        if(highProbabs < probas[j] && j != positions[3])
          highProbabs = j
      }
      
      #Find the fastest way from the option ranger has to the high probability waterhole
      tempI = 0
      for(i in 1:length(optionsStep2))
      {
        if(optionsStep2[i] != maxProb)
        {
          tempValue = abs(highProbabs-optionsStep2[i])
          
          if(tempValue < tempData)
          {
            tempData = tempValue
            tempI = i
          }
        }
      }
      
      maxProb2 = optionsStep2[tempI]
      
    }
    
    #Do counter for different movement for each turn
    flagCheck <<- flagCheck + 1
    
    if(flagCheck %% 2 == 0)
    {
      move = c(maxProb,0)
      probas[positions[3]] = 0
    }
    else
    {
      move = c(maxProb,maxProb2)
    }
    
  }
  else
  {
    move = c(0,0)
  }
  
  return(list("move"=move,"probas"=probas))
}

#use this function to run the solution
markovMove <- function(moveInfo, readings, positions, edges, probs) {
  #moveInfo = list(moves = c('the two moves to make'), mem = list('any info we want to store')
  #readings = c('salinity reading from Croc', 'phosphate reading from Croc', 'nitrogen reading from Croc')
  #positions = c('position of tourist 1', 'position of tourist 2', 'position of player')
  #edges = matrix('edges paths between waterholes, first column is one extremity of the edge, second column is the other extremity, every row is one edge')
  #probs = list(salinity = matrix('mean and standard deviation of readings for salinity, each row is a waterhole, first column is mean, second column is standard deviation'), phosphate = matrix('mean and standard deviation of readings for phosphate, each row is a waterhole, first column is mean, second column is standard deviation'), nitrogen = matrix('mean and standard deviation of readings for nitrogen, each row is a waterhole, first column is mean, second column is standard deviation'))

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
  
  
  #################
  #ACTION DECISION#
  #################
  #BFS-based Move
  moveResult = bfsBased(probas, positions, edges)
  
  #Local Search Move
  #moveResult = makeMoveLocalSearch(probas, positions, edges)
  ########################
  #END OF ACTION DECISION#
  ########################
  
  moveInfo[['moves']] = moveResult$move
  moveInfo[['mem']][["previousProbabilities"]] = moveResult$probas
  return(moveInfo)
}