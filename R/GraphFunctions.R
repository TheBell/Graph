
#' A Graph Function
#'
#' This function returns a graph object stored as an adjacency matrix.
#' @param nodes A vector of nodes
#' @param weighted TRUE if the graph  is weighted or FALSE. Defaults to FALSE.
#' @param directed TRUE if graph is directed. Defaults to FALSE.
#' @param useAdjMatrix TRUE if an adjacency matrix is used internally or FALSE to use a list
#' @return A graph object
graph <- function(nodes, weighted = FALSE, directed = FALSE, useAdjMatrix = TRUE) {
  if (useAdjMatrix) return(adjMatrixGraph(nodes, weighted, directed))
  return(adjListGraph(nodes, weighted, directed))
}

#' A Graph Function
#'
#' This function returns a graph object stored as an adjacency matrix.
#' @param nodes A vector of nodes
#' @param weighted TRUE if the graph  is weighted or FALSE. Defaults to FALSE.
#' @param directed TRUE if graph is directed. Defaults to FALSE.
#' @return A graph object
adjMatrixGraph <- function(nodes, weighted = FALSE, directed = FALSE) {
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

  class(graphObj) <- c("adjMatrixGraph", "graph")

  return(graphObj)
}


#' A Graph Function
#'
#' This function returns a graph object stored as an adjacency list.
#' @param nodes A vector of nodes
#' @param weighted TRUE if the graph  is weighted or FALSE. Defaults to FALSE.
#' @param directed TRUE if graph is directed. Defaults to FALSE.
#' @return A graph object
adjListGraph <- function(nodes, weighted = FALSE, directed = FALSE) {
  return(NULL)
}

hasEdge <- function(obj, ...) UseMethod("hasEdge")
hasEdge.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#'
#' This function allows you to check for if an edge exists between 2 nodes in the graph.
#' @param node1 The first node
#' @param node2 The second node
#' @return a list containing the graph object and the a boolean indicating if the graph contained the edge
hasEdge.adjMatrixGraph <- function(graph, node1, node2) {
  r <- list(graph = graph, result = NULL)
  if (!node1 %in% graph$nodes | !node2 %in% graph$nodes) r$result <- FALSE
  else r$result <- graph$edges[node1, node2] != -1
  return(r)
}

addEdge <- function(obj, ...) UseMethod("addEdge")
addEdge.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#'
#' This function allows you to add a new edge to an existing graph. If weighted graph,
#' will override previous edge.
#' @param node1 The first node
#' @param node2 The second node
#' @param weight Defaults to 1 (used for non-weighted graph)
#' @return the new graph with the added edge and a boolean indicating whether the operation was successful
addEdge.adjMatrixGraph <- function(graph, node1, node2, weight = 1) {
  r <- list(graph = graph, result = TRUE)

  if (!node1 %in% graph$nodes | !node2 %in% graph$nodes) {
    r$result <- FALSE
    return(r)
  }
  graph$edges[node1, node2] <- weight
  if (!graph$directed) graph$edges[node2, node1] <- weight

  r$graph = graph

  return(r)
}

deleteEdge <- function(obj, ...) UseMethod("deleteEdge")
deleteEdge.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#'
#' This function deletes an edge.
#' @param graph the graph obj
#' @param node1 The first node
#' @param node2 The second node
#' @return a list containing the new graph and whether an operation was performed
deleteEdge.adjMatrixGraph <- function(graph, node1, node2) {
  r <- list(graph = obj, result = FALSE)
  return(FALSE)
}
