
# Loop to answer The Riddler for March 18, 2016. 
# http://fivethirtyeight.com/features/can-you-best-the-mysterious-man-in-the-trench-coat/

# Create sequence from 1 to 1000 
vals <- c(1:1000)

# Build dataframe for starting guesses, include row to be filled by expected outcome
guesses <- data.frame(vals)
guesses$expected <- 0

# Begin loop for each possible starting guess
for (j in 1:1000) {
  
  # Build dataframe for answers, include row for whether result was guessed
  results <- data.frame(vals)
  results$result <- 0
  
  # Begin loop for each possible answer
  for (i in 1:1000) {
    
    # Starting guess = j
    guess <- j
    
    # The minimum high number is upper bound
    minH <- 1000
    
    # The maximum low number is lower bound
    maxL <- 1
    
    for (g in 1:9) {
      # If the guess is high and lower than current upper bound, reset upper bound
      if (guess > i && guess < minH) minH <- guess
      # If the guess is low but higher than the current lower bound, reset lower bound
      if (guess < i && guess > maxL) maxL <- guess
      # If the guess is correct insert 1 in result dataframe and exit loop
      if (guess == i) {results[i,]$result <- 1 ; break}
      # The next guess is the midpoint between the lower and upper bound, rounding up
      guess <- ceiling((maxL+minH)/2)
    }
  }
  
  # The expected result for a starting guess is the average earnings over all answers
  guesses[j,]$expected <- sum(results[which(results$result==1),1])/nrow(results)
}

# The best starting point is the starting guess with the highest expected result
answer <- guesses[which(guesses$expected == max(guesses$expected)),]


png(file="expectedresults.png", bg="white")
plot(guesses,type="l",ylab = "expected earnings ($)", xlab = "starting guess ($)")
dev.off()
