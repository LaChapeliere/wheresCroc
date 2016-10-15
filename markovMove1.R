probabilityFromNormalDistribution <- function(value, mean, std) {
  interval = 0.5
  lowcut = value - interval
  highcut = value + interval
  
  return(pnorm(highcut, mean = mean, sd = std) - pnorm(lowcut, mean = mean, sd = std))
}
flagCheck <<- 0
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
  print("prev Prob")
  print(sum(previousProbabilities))
  if (sum(previousProbabilities) == 0 || !is.nan(sum(previousProbabilities))){
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


makeMove <- function(probas, positions, edges) {
  #probas: list of possibilities for each hole
  #positions: croc, backpacker 1, bacpacker 2, ranger
  #edges: relation of one hole to another
  
  #find max value of probability
  if(all(!is.nan(probas)))
  {
      maxProb = 0
      maxProb2 = 0
      tempData = 100
      flag = 0
      # for (i in 1:40) {
      #   if(maxProb < probas[i])
      #     maxProb = i
      # }
      
      ##################################
      ###########My Trying COde#########
      ##################################
      options=getOptions(positions[3],edges)
      # print("Position")
      # print(positions[3])
      # print(options[3])
      # print("Length of Options")
      # print(length(options))
      
      # for(i in 1:length(options))
      # {
      #   print(options[i])
      #   print(probas[options[i]])
      # }
    
      
      # pCrocs = 0
      # tempData = 100
      # if(positions[2] < 0 || positions[3] < 0 || positions[1] < 0 || !is.na(positions[1]) || !is.na(positions[2]) || !is.na(positions[3]) )
      # {
      #   for (i in 1:40) {
      #     if(pCrocs < probas[i])
      #       pCrocs = i
      #   }
      #   for(i in 1:length(options))
      #   {
      # 
      #     tempCal = pCrocs - options[i]
      #     if(abs(tempCal) < tempData)
      #       tempData = tempCal
      #   }
      #   maxProb = tempData
      # }
      # else
      # {
      ######Move for the First
      for(i in 1:length(options))
      {
        if(probas[options[i]] != 0 && !is.nan(probas[options[i]]))
        {
          if(maxProb < probas[options[i]])
            maxProb = options[i]
          flag = flag + 1
        }
      }
      if(flag == 0)
      {
        highProbabs = 0
        for(j in 1:40)
        {
          if(highProbabs < probas[j] && j != positions[3])
            highProbabs = j
        }
        # print("Options flag = 0")
        # print(options)
        tempI = 0
        for(i in 1:length(options))
        {
          if(options[i] != positions[3])
          {
             tempValue = abs(highProbabs-options[i])
             # print("Flag = 0")
             # print(tempValue)
             # print("Temp Data")
             # print(tempData)
             if(tempValue < tempData)
             {
               tempData = tempValue
               tempI = i
             }
          }
        }
        maxProb = options[tempI]
        
      }
      # }
      # print(probas)
      # print("MaxProb")
      # print(maxProb)
      
      ########Move for the second
      flag2 = 0
      tempData = 100
      optionsStep2 = getOptions(maxProb,edges)
      for(i in 1:length(optionsStep2))
      {
        if(probas[optionsStep2[i]] != 0 && !is.nan(optionsStep2[options[i]]))
        {
          if(maxProb2 < probas[optionsStep2[i]])
            maxProb2 = optionsStep2[i]
          flag2 = flag2 + 1
        }
      }
      if(flag2 == 0)
      {
        highProbabs = 0
        for(j in 1:40)
        {
          if(highProbabs < probas[j] && j != positions[3])
            highProbabs = j
        }
        # print("Options flag2 = 0")
        # print(optionsStep2)
        tempI = 0
        for(i in 1:length(optionsStep2))
        {
          if(optionsStep2[i] != maxProb)
          {
            tempValue = abs(highProbabs-optionsStep2[i])
            # print("Flag2 = 0")
            # print(tempValue)
            # print("Temp Data2")
            # print(tempData)
            if(tempValue < tempData)
            {
              tempData = tempValue
              tempI = i
            }
          }
        }
        # print("Optimal Options2")
        # print(optionsStep2[tempI])
        maxProb2 = optionsStep2[tempI]
        
      }
      # }
      # print(probas)
      # print("MaxProb2")
      # print(maxProb2)
      
      
      # optionStep2=getOptions(maxProb,edges)
      # print(maxProb)
      # print("Options2")
      # for(i in 1:length(optionStep2))
      # {
      #   print(optionStep2[i])
      #   print(probas[optionStep2[i]])
      # }
      # pCrocs = 0
      # tempData = 100
      # 
      # print(positions[1])
      # print(positions[3])
      # if(positions[2] < 0 || positions[3] < 0 || positions[1] < 0 || !is.na(positions[1]) || !is.na(positions[2]) || !is.na(positions[3]))
      # {
      #   for (i in 1:40) {
      #     if(pCrocs < probas[i])
      #       pCrocs = i
      #   }
      #   for(i in 1:length(optionStep2))
      #   {
      # 
      #     tempCal = pCrocs - optionStep2[i]
      #     if(abs(tempCal) < tempData)
      #       tempData = tempCal
      #   }
      #   maxProb2 = tempData
      # }
      # else
      # {
      #   for(i in length(optionStep2))
      #     if(maxProb2 < probas[optionStep2[i]])
      #       maxProb2 = optionStep2[i]
      # }
      # 
      # print("Test1")
      # print(maxProb)
      # print("Test111")
      # print(optionStep2)
      # 
      # 
      # 
      # print("Test2")
      # print(maxProb2)
      
     
      # 
      # tempData = c(maxProb,maxProb2)
      # print(tmppath)vec = as.vector(t(edges))
      # megraph = make_graph(vec, directed=FALSE)
      # #tmp <- graph.bfs(megraph, root=positions[3], neimode='all', order=TRUE, father=TRUE)
      # 
      # 
      # tmppath = shortest_paths(megraph, positions[3], to = maxProb, mode ='all',
      #                weights = NULL, output = 'both',
      #                predecessors = FALSE, inbound.edges = FALSE)       
      # 
      # print("Test")
      # print("Test")
      # print(options)
      # print(options[1])
      #print(edges)
      #print(edges(1,1))
      
      #find path to maxProb
      flagCheck <<- flagCheck + 1
      # print("Flag")
      # print(flagCheck)
      if(flagCheck %% 2 == 0)
      {
        move = c(maxProb,0)
        probas[positions[3]] = 0
      }
      else
      {
        move = c(maxProb,maxProb2)
        # probas[maxProb]= 0;
        # probas[maxProb2]=0;
      }
      # probas[maxProb2]=0;
      # print("Move")
      # print(move)
  }
  else
  {
    print("Test11")
    move = c(0,0)
  }
  
  return(list("move"=move,"probas"=probas))
}

markovMove1 <- function(moveInfo, readings, positions, edges, probs) {
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