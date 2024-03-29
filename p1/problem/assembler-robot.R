# =======================================================================
# Group Name:CibCom
# Students: Erik B. Terres, Aritz Zugazaga
# =======================================================================

initialize.problem() <- function(filename = "../data/assembler-robot-1.txt") {
  problem <- list()
  
  problem$name <- paste0("assembler-robot-[", filename, "]")
  
  N <- readLines(file, n=4)
  M <- strsplit(N, ";")
  print(M)
  cords <- strsplit(M[[1]],",")
  cordsN <- as.numeric(cords[[1]])
  grid <- matrix(0, nrow=cordsN[[1]],ncol=cordsN[[2]], byrow=TRUE)
  cordsR <- strsplit(M[[2]],",")
  cordsRN <- as.numeric(cordsR[[1]])
  grid[cordsRN[[1]],cordsRN[[2]]] <- -1
  print(M)
  print(grid)
  # problem$state_initial     <- <INSERT CODE HERE>
  # problem$state_final       <- <INSERT CODE HERE>
  # problem$actions_possible  <- <INSERT CODE HERE>
  
  # You can add additional attributes
  problem$rows              <- rows
  problem$columns           <- columns
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  where <- which(state == 0, arr.ind = TRUE)
  row <- where[1]
  col <- where[2]
  
  if (action == "Up") {
    return(row != 1)
  }
  
  if (action == "Down") {
    return(row != problem$rows)
  }
  
  if (action == "Left") {
    return(col != 1)
  }
  
  if (action == "Right") {
    return(col != problem$columns)
  }
  
  return(FALSE)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  # <INSERT YOUR CODE HERE TO MODIFY CURRENT STATE>
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  result <- FALSE # Default value is FALSE.
  
  # <INSERT YOUR CODE HERE TO CHECK WHETHER A STATE IS FINAL OR NOT> 
  
  return(result)
}

# Transforms a state into a string
to.string = function (state, problem) {
  
  # <INSERT YOUR CODE HERE TO GENERATE A STRING THAT REPRESENTS THE STATE> 
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE> 
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
	return(1) # Default value is 1.
}