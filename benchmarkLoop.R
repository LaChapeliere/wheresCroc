benchmarkLoop <- function(n) {
  results = vector(mode="integer", length=n)
  for (i in 1:n) {
    results[n] = runWheresCroc(markovMove, pause = 0.1)
  }
  
  print("Results:")
  print(results)
  print("Mean:")
  print(mean(results))
  print("Standard deviation:")
  print(sd(results))
  
  return(results)
}