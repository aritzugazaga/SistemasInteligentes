random.restart.hill.climbing = function(problem, 
                                        iterations,
                                        max_iterations = 1000,
                                        count_print = 100, 
                                        trace = FALSE) {
  
  limit <- 0
  iteraciones <- 0
  
  name_method      <- paste0("Random Restart Hill Climbing")
  state_initial    <- problem$state_initial
  actions_possible <- problem$actions_possible
  
  # Get Start time
  start_time       <- Sys.time()
  
  count <- 1
  end_reason <- 0
  
  mejor_resultado <- list(parent = c(),
                          state = state_initial,
                          actions = c(),
                          depth = 1,
                          cost = get.cost(state = state_initial, problem = problem),
                          evaluation = get.evaluation(state_initial, problem))
  
  report <- data.frame(iteration = numeric(),
                       nodes_frontier = numeric(),
                       depth_of_expanded = numeric(),
                       nodes_added_frontier = numeric())
  
  #Ejecuciones = iteraciones pasadas por parametro
  while (limit <= iterations) {
    
    report <- rbind(report, data.frame(iteration = iteraciones,
                                       nodes_frontier = 1,
                                       depth_of_expanded = mejor_resultado$depth,
                                       nodes_added_frontier = 1))
    
    problem <- initialize.problem(filename = filename)
    
    #Funcion Hill Climbing Search
    resultado <- hill.climbing.search(problem,
                                      max_iterations = max_iterations,
                                      count_print = count_print, 
                                      trace = trace)
    
    #Si el resultado es el mejor se actualiza
    if(resultado$state_final$evaluation <= mejor_resultado$evaluation){
      mejor_resultado <- resultado$state_final
    }
    
    #Si mejor_resultado es el estado final hacemos break
    if((is.final.state(state = mejor_resultado$state, final_state = resultado$state_final, problem = problem))){
      break 
    }
    else{
      limit <- limit + 1
    }
    
    
  }
  
  
  end_time <- Sys.time()
  
  result <- list()
  result$name        <- paste0("Random Restart Hill Climbing: ")
  result$runtime     <- end_time - start_time
  result$state_final <- mejor_resultado
  result$report      <- report
  
  
  return(result)
}