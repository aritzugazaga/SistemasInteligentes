local.beam.search = function(problem,
                             max_iterations = 100,
                             count_print = 100,
                             trace = FALSE,
                             beams,
                             filename){
  
  name_method      <- paste0("Local Beam Search")
  state_initial    <- problem$state_initial
  actions_possible <- problem$actions_possible
  
  # Get Start time
  start_time       <- Sys.time()
  
  node_current <- c() #Lo cambiamos a una lista de nodos en funcion del numero de beams
  sucessor_nodes <- c() #Vector para tomar los K mejores estados
  
  limit <- 1
  
  #Generar k estados aleatorios 
  while(limit <= beams){
    state_initial    <- problem$state_initial #Estado inicial aleatorio
    actions_possible <- problem$actions_possible #Posibles acciones 
    node_current[[limit]] <- list(parent = c(),
                                  state = state_initial,
                                  actions = c(),
                                  depth = 1,
                                  cost = get.cost(state = state_initial, problem = problem),
                                  evaluation = get.evaluation(state_initial, problem))
    
    problem <- initialize.problem(filename = filename) #Inicializar problema para generar distintos estados
    
    limit <- limit + 1
    
  }
  
  
  
  #Initialization of information for further analysis
  report <- data.frame(iteration = numeric(),
                       nodes_frontier = numeric(),
                       depth_of_expanded = numeric(),
                       nodes_added_frontier = numeric())
  
  #Ahora tenemos que hacer que en cada una de las iteraciones se comporte igual que el algoritmo Hill Climbing
  #Perform "max_iterations" iterations of the expansion process of the first node in the frontier list
  
  count <- 1
  end_reason <- 0
  limit2 <- 1
  
  while (count <= max_iterations) {
    #Dentro del bucle que nos proporciona el Hill Climbing meteremos otro para los nodos actaules y secuesores de cada beam
    while(limit2 <= beams){
      # Print a search trace for each "count_print" iteration
      if (count %% count_print == 0) {
        print(paste0("Iteration: ", count, ", Current node=", node_current[[limit2]]$cost, " / needed=", problem$needed_slices), quote = FALSE)
      }
      
      #If "trace" is on, the information of current node is displayed
      if (trace) {
        print(paste0("Current node=", node_current[[limit2]]$cost, " / needed=", problem$needed_slices), quote = FALSE)
        to.string(state = node_current$state, problem = problem)
      }
      
      # Current node is expanded
      sucessor_nodes[[limit2]] <- local.expand.node(node_current[[limit2]], actions_possible, problem)
      # Successor nodes are sorted ascending order of the evaluation function
      sucessor_nodes[[limit2]] <- sucessor_nodes[[limit2]][order(sapply(sucessor_nodes[[limit2]],function (x) x$evaluation))]
      # Select best successor
      node_current[[limit2]] <- sucessor_nodes[[limit2]][[1]]
      
      # The best successor is better than current node
      if(node_current[[limit2]]$evaluation == 0){
        report <- rbind(report, data.frame(iteration = count,
                                           nodes_frontier = 1,
                                           depth_of_expanded = node_current[[1]]$depth,
                                           nodes_added_frontier = 1))
        count <- max_iterations 
        break
      }
      
      
      limit2 <- limit2 + 1
    }
    
    #Add of information for further analysis
    report <- rbind(report, data.frame(iteration = count,
                                       nodes_frontier = 1,
                                       depth_of_expanded = node_current[[1]]$depth,
                                       nodes_added_frontier = 1))
    count <- count + 1
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name    <- name_method
  result$runtime <- end_time - start_time
  
  node_current <- node_current[order(sapply(node_current,function (x) x$evaluation))]
  
  
  to.string(state = node_current[[1]]$state, problem = problem)
  
  result$state_final <- node_current[[1]]
  result$report      <- report
  
  return(result)
}