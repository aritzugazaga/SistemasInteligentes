hill.climbing.search = function(problem,
                                max_iterations = 1000,
                                count_print = 100, 
                                trace = FALSE) {
  
  name_method      <- paste0("Hill Climbing Search")
  state_initial    <- problem$state_initial
  actions_possible <- problem$actions_possible
  
  # Get Start time
  start_time       <- Sys.time()
  
  node_current <- list(parent = c(),
                       state = state_initial,
                       actions = c(),
                       depth = 1,
                       cost = get.cost(state = state_initial, problem = problem),
                       evaluation = get.evaluation(state_initial, problem))
  
  count <- 1
  end_reason <- 0
  
  #Initialization of information for further analysis
  report <- data.frame(iteration = numeric(),
                       nodes_frontier = numeric(),
                       depth_of_expanded = numeric(),
                       nodes_added_frontier = numeric())
  
  #Perform "max_iterations" iterations of the expansion process of the first node in the frontier list
  while (count <= max_iterations) {
    # Print a search trace for each "count_print" iteration
    if (count %% count_print == 0) {
      print(paste0("Iteration: ", count, ", Current node=", node_current$cost, " / needed=", problem$needed_slices), quote = FALSE)
    }
    
    #If "trace" is on, the information of current node is displayed
    if (trace) {
      print(paste0("Current node=", node_current$cost, " / needed=", problem$needed_slices), quote = FALSE)
      to.string(state = node_current$state, problem = problem)
    }
    
    # Current node is expanded
    sucessor_nodes <- local.expand.node(node_current, actions_possible, problem)
    # Successor nodes are sorted ascending order of the evaluation function
    sucessor_nodes <- sucessor_nodes[order(sapply(sucessor_nodes,function (x) x$evaluation))]
    
    # Select best successor
    node_best_successor <- sucessor_nodes[[1]]
    
    # The best successor is better than current node
    if (node_best_successor$evaluation <= node_current$evaluation) {
      # Current node is updated
      node_current <- node_best_successor
      
      #If "trace" is on, the information of the new current node is displayed
      if (trace){
        print(paste0("New current node=", node_current$cost, " / needed=", problem$needed_slices), quote = FALSE)
        to.string(state = node_current$state, problem = problem)
      }
    # Local best found
    } else {
      # Algorithm stops because a local best has been found
      end_reason <- "Local_Best"
      
      #Add of information for further analysis
      report <- rbind(report, data.frame(iteration = count,
                                         nodes_frontier = 1,
                                         depth_of_expanded = node_current$depth,
                                         nodes_added_frontier = 1))
      
      break
    }
    
    #Add of information for further analysis
    report <- rbind(report, data.frame(iteration = count,
                                       nodes_frontier = 1,
                                       depth_of_expanded = node_current$depth,
                                       nodes_added_frontier = 1))
    count <- count + 1
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name    <- name_method
  result$runtime <- end_time - start_time
  
  # Print final result
  if (end_reason == "Local_Best") {
    print("Local best found!!", quote = FALSE)
  } else {
    print("Maximum iterations reached", quote = FALSE)
  }
  
  to.string(state = node_current$state, problem = problem)
  
  result$state_final <- node_current
  result$report      <- report
  
  return(result)
}