# Create a function to find all possible solutions
find_weight_estimations <- function(total_weight) {
  # Initialize an empty list to store solutions
  solutions <- list()
  
  # Iterate through possible values of x from 0 to total_weight
  for (x in 0:total_weight) {
    # Calculate y based on the first equation
    y <- total_weight - x
    
    # Store the solution in the list
    solutions[[length(solutions) + 1]] <- c(x, y)
  }
  
  return(solutions)
}

# Total weight observed in both measurements
total_weight <- 6

# Find all possible weight estimations
possible_solutions <- find_weight_estimations(total_weight)

# Print all possible solutions
for (i in 1:length(possible_solutions)) {
  cat("Solution", i, " - Estimated weights (A, B):", 
      possible_solutions[[i]][1], ",", possible_solutions[[i]][2], "\n")
}

