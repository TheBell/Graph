
#' A Graph Function
#' 
#' This function returns a graph object.
#' @param nodes A vector of nodes
#' @param weighted TRUE if the graph  is weighted or FALSE. Defaults to FALSE.
#' @param directed TRUE if graph is directed. Defaults to FALSE.
#' @return A graph object
graph <- function(nodes, weighted=FALSE, directed=FALSE) {
  graphObj <- list(weighted=weighted,
                  directed=directed,
                  nodes=c())
  
  if (!is.vector(nodes)) stop("Nodes must be a vector.")
  
  for (i in 1:length(nodes)) {
    graphObj$nodes[[i]] <- nodes[i]
  }
  
  class(graphObj) <- "graph"
  return(graphObj)
}

hasEdge <- function(obj) UseMethod("hasEdge")
hasEdge.default <- function(obj) FALSE

#' A Graph Function
#' 
#' This function allows you to check for if an edge exists between 2 nodes in the graph.
#' @param node1 The first node
#' @param node2 The second node
#' @return TRUE if the edge exists or FALSE otherwise
hasEdge.graph <- function(node1, node2) {
  return(FALSE)
}

addEdge <- function(obj) UseMethod("addEdge")
addEdge.default <- function(obj) FALSE

#' A Graph Function
#' 
#' This function allows you to add a new edge to an existing graph.
#' Throws error if 1 or both nodes do not exist.
#' @param node1 The first node
#' @param node2 The second node
#' @param weight Defaults to 1 (used for non-weighted graph)
#' @param directed Defaults to FALSE for non-directed graph.
#' @return TRUE if the edge added or FALSE if edge already exists
addEdge.graph <- function(node1, node2, weight=1, directed=FALSE) {
  return(FALSE)
}

deleteEdge <- function(obj) UseMethod("deleteEdge")
deleteEdge.default <- function(obj) FALSE

#' A Graph Function
#' 
#' This function deletes an edge.
#' @param node1 The first node
#' @param node2 The second node
#' @return TRUE if the node was deleted or FALSE if nothing was done
deleteEdge.graph <- function(node1, node2) {
  return(FALSE)
}