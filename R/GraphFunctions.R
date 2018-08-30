
#' A Graph Function
#' 
#' This function returns a graph object.
#' @param nodes A vector of nodes
#' @param weighted TRUE if the graph  is weighted or FALSE. Defaults to FALSE.
#' @param directed TRUE if graph is directed. Defaults to FALSE.
#' @return A graph object
graph <- function(nodes, weighted = FALSE, directed = FALSE) {
  graphObj <- list(weighted = weighted,
                  directed = directed,
                  nodes = c())
  
  if (!is.vector(nodes)) stop("Nodes must be a vector.")
  
  for (i in 1:length(nodes)) {
    graphObj$nodes[[i]] <- nodes[i]
  }
  
  names(graphObj$nodes) <- nodes
  
  graphObj$edges <- matrix(-1, nrow = length(graphObj$nodes), ncol = length(graphObj$nodes), 
                           dimnames = list(names(graphObj$nodes), names(graphObj$nodes)))
  
  class(graphObj) <- "graph"
  return(graphObj)
}

hasEdge <- function(obj, ...) UseMethod("hasEdge")
hasEdge.default <- function(obj, ...) paste("Method for class not found:", class(obj))

#' A Graph Function
#' 
#' This function allows you to check for if an edge exists between 2 nodes in the graph.
#' @param node1 The first node
#' @param node2 The second node
#' @return TRUE if the edge exists or FALSE otherwise
hasEdge.graph <- function(graph, node1, node2) {
  if (!node1 %in% graph$nodes) return(FALSE)
  if (!node2 %in% graph$nodes) return(FALSE)
  return(graph$edges[node1, node2] != -1)
}

addEdge <- function(obj, ...) UseMethod("addEdge")
addEdge.default <- function(obj, ...) paste("Method for class not found:", class(obj))

#' A Graph Function
#' 
#' This function allows you to add a new edge to an existing graph. If weighted graph,
#' will override previous edge.
#' @param node1 The first node
#' @param node2 The second node
#' @param weight Defaults to 1 (used for non-weighted graph)
#' @return the new graph with the added edge
addEdge.graph <- function(graph, node1, node2, weight = 1) {
  graph$edges[node1, node2] <- weight
  
  if (!graph$directed) graph$edges[node2, node1] <- weight
  
  return(graph)
}

deleteEdge <- function(obj, ...) UseMethod("deleteEdge")
deleteEdge.default <- function(obj, ...) paste("Method for class not found:", class(obj))

#' A Graph Function
#' 
#' This function deletes an edge.
#' @param node1 The first node
#' @param node2 The second node
#' @return TRUE if the node was deleted or FALSE if nothing was done
deleteEdge.graph <- function(obj, node1, node2) {
  return(FALSE)
}