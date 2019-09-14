
dijkstra <- function(graph, init_node){
  

  stopifnot(is.data.frame(graph) && length(graph) == 3 & names(graph) == c("v1", "v2", "w"))
  

  stopifnot(init_node %in% graph$v1)
  

  nodes <- graph[[1]][!duplicated(graph[[1]])]
  

  dist <- numeric(length(nodes))
  

  for(i in seq_along(nodes)){
    if(nodes[i] == init_node){dist[i] = 0}else{
      dist[i] = Inf
    }
  }
  

  names(dist) <- nodes
  

  current_node <- init_node
  visit_nodes <- c(current_node)
  unvisit_nodes <- setdiff(nodes, visit_nodes)
  

  
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
    

    min_dist = min(dist[unvisit_nodes])
    

    current_node <- names(which(dist == min_dist))
    

    dist[current_node] <- min_dist
    

    visit_nodes <- c(visit_nodes, current_node)
    

    unvisit_nodes <- setdiff(nodes, visit_nodes)
    

    if(length(unvisit_nodes) == 0){break}
  }
  names(dist) <- NULL
  return(dist)
  
}