#' Dijkstra algorithm.
#'
#' \code{dijkstra(graph, init_node)} returns  shortest paths between nodes in a
#' graph.
#'
#'
#' For a given source node in the graph, the algorithm finds the shortest path
#' between that node and every other. The Dijkstra algorithm uses labels that
#' are positive integers or real numbers, which are totally ordered. Let the
#' node at which we are starting be called the initial node. Let the distance of
#' node Y be the distance from the initial node to Y. Dijkstra's algorithm will
#' assign some initial distance values and will try to improve them step by
#' step.
#'
#' \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @param graph  data.frame with three variables (v1, v2 and w) that contains
#'   the edges of the graph (fromv1 to v2) with the weight of the edge (w).
#' @param init_node Numeric scalar indicating the initial node.
#' @return If all inputs are integer, then the output will be an integer.
#' @export


dijkstra <- function(graph, init_node){
  
  # First argument must be a data frame with three columns.
  stopifnot(is.data.frame(graph) && length(graph) == 3 & names(graph) == c("v1", "v2", "w"))
  
  # The second argument must be a numeric scalar.
  stopifnot(init_node %in% graph$v1)
  
  # Creating a vector consisting the graph nodes.
  nodes <- graph[[1]][!duplicated(graph[[1]])]
  
  # Creating a vector called "dist" to store the nodes paths. 
  dist <- numeric(length(nodes))
  
  # Setting initial values for distance vector. 0 assigned to init_node and Inf to the others.
  for(i in seq_along(nodes)){
    if(nodes[i] == init_node){dist[i] = 0}else{
      dist[i] = Inf
    }
  }
  
  # Assigning names to each element of "dist.
  names(dist) <- nodes
  
  # Creating visitted & unvisitted nodes
  current_node <- init_node
  visit_nodes <- c(current_node)
  unvisit_nodes <- setdiff(nodes, visit_nodes)
  
  
  # Looking for minimum distance.
  
  repeat{
  for (i in seq_along(graph[[1]])) {
    if(graph[[i,1]] == current_node){
      if (graph[[i,2]] %in% unvisit_nodes){
        if((dist[graph[[i,2]]] > dist[current_node] + graph[[i,3]])){
           dist[graph[[i,2]]] <- dist[current_node] + graph[[i,3]]
        }
      }
    }
  }
    
    # Selecting the shortest paths.
    min_dist = min(dist[unvisit_nodes])
    
    # Changing the current node value to the nodes with the minimum path.
    current_node <- names(which(dist == min_dist))
    
    # Storing the value of min_dist into the vector "dist".
    dist[current_node] <- min_dist
    
    # Considering the current node as a visited position.
    visit_nodes <- c(visit_nodes, current_node)
    
    # Updating the unvisitd nodes.
    unvisit_nodes <- setdiff(nodes, visit_nodes)
    
    # Breaking the loop as the nodes are totally visited.
    if(length(unvisit_nodes) == 0){break}
  }
  names(dist) <- NULL
  return(dist)
  
}