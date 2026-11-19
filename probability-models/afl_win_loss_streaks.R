#############################################
# Date: 19.08.2024
# Author: Chong Jun Hao
#############################################

# Using the data in port.adelaide.csv, write some R code to find the
# frequency with which PA won/lost a game after it won/lost its previous game.

# Read the data
pa <- read.csv("port.adelaide.csv", header = TRUE)

# Find the number of games
n <- nrow(pa)

# Initialize the counters
lost_lost <- 0
lost_won <- 0
won_lost <- 0
won_won <- 0

# Loop through the data
for (i in 2:n)
{
  if (pa$game.result[i - 1] == 1)
  {
    if (pa$game.result[i] == 1)
    {
      won_won <- won_won + 1
    }
    else
    {
      won_lost <- won_lost + 1
    }
  }
  else
  {
    if (pa$game.result[i] == 1)
    {
      lost_won <- lost_won + 1
    }
    else
    {
      lost_lost <- lost_lost + 1
    }
  }
}

# Display the results 
cat("Port Adelaide lost after it lost", lost_lost, "games\n")
cat("Port Adelaide lost after it won", lost_won, "games\n")
cat("Port Adelaide won after it lost", won_lost, "games\n")
cat("Port Adelaide won after it won", won_won, "games\n")