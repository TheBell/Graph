# TODO: Remove possibility of loopsbacks and test
# TODO: Test isSparse and isDense


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

  graphObj$edges <- matrix(0, nrow = length(graphObj$nodes), ncol = length(graphObj$nodes),
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
  else r$result <- graph$edges[node1, node2] != 0
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
#' @return the new graph with the added edge and whether the operation was successful
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
  r <- list(graph = graph, result = TRUE)
  
  if (!node1 %in% graph$nodes | !node2 %in% graph$nodes) {
    r$result <- FALSE
    return(r)
  }
  
  if (graph$edges[node1, node2] == 0) {
    r$result <- FALSE
    return(r)
  }
  
  graph$edges[node1, node2] <- 0
  
  if (!graph$directed) {
    graph$edges[node2, node1] <- 0
  }
  
  r$graph <- graph
  
  return(r)
}

addVertex <- function(obj, ...) UseMethod("addVertex")
addVertex.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#' 
#' This function adds a vertex.
#' @param graph the graph obj
#' @param vertex the new vertex to add
#' @return a list containing the new graph and whether an operation was performed
addVertex.adjMatrixGraph <- function(graph, vertex) {
  r <- list(graph = graph, result = TRUE)
  
  if (vertex %in% graph$nodes) {
    r$result <-  FALSE
    return(r)
  }
  
  # Update list of nodes
  graph$nodes <- c(graph$nodes, vertex)
  names(graph$nodes)[length(graph$nodes)] <- vertex
  
  # Update matrix
  graph$edges <- rbind(cbind(graph$edges, 0), 0)  # add new row and col to matrix
  rownames(graph$edges) <- names(graph$nodes)
  colnames(graph$edges) <- rownames(graph$edges)
  
  r$graph = graph
  
  return(r)
}

deleteVertex <- function(obj, ...) UseMethod("deleteVertex")
deleteVertex.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#' 
#' This function deletes a vertex.
#' @param graph the graph obj
#' @param vertex the vertex to delete
#' @return a list containing the new graph and whether an operation was performed
deleteVertex.adjMatrixGraph <- function(graph, vertex) {
  r <- list(graph = graph, result = TRUE)
  
  if (!vertex %in% graph$nodes) {
    r$result <- FALSE
    return(r)
  }
  
  index <- which(graph$nodes == vertex)
  
  graph$nodes <- graph$nodes[-index]
  graph$edges <- graph$edges[-index, -index]
  
  r$graph <- graph
  
  return(r)
}

isSparse <- function(obj, ...) UseMethod("isSparse")
isSparse.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#' 
#' This functions checks if the graph is sparse.
#' @param graph the graph obj
#' @param level the cutoff level for a sparse graph, defaul is .15
#' @return a list containing the graph and whether the graph is sparse
isSparse.adjMatrixGraph <- function(graph, level = .15) {
  
  cutoff <- .15 * (length(graph$nodes)  ^ 2 - length(graph$nodes)) # Since no loop-back is allowed, subtract those possibilites
  
  if (graph$directed) {
    cutoff <- cutoff / 2
  }
  
  edges <- sum(graph$edges > 0)
  
  return(list(graph = graph, result = edges < cutoff))
}

isDense <- function(obj, ...) UseMethod("isDense")
isDense.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#' 
#' This functions checks if the graph is dense.
#' @param graph the graph obj
#' @param level the cutoff level for a dense graph, defaul is .85
#' @return a list containing the graph and whether the graph is dense
isDense.adjMatrixGraph <- function(graph, level = .15) {
  
  cutoff <- .85 * (length(graph$nodes)  ^ 2 - length(graph$nodes)) # Since no loop-back is allowed, subtract those possibilites
  
  if (graph$directed) {
    cutoff <- cutoff * 2
  }
  
  edges <- sum(graph$edges > 0)
  
  return(list(graph = graph, result = edges > cutoff)) 
}

countVertices <- function(obj, ...) UseMethod("countVertices")
countVertices.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#' 
#' This functions counts the vertices.
#' @param graph the graph obj
#' @return a list containing the graph and the number of vertices

countEdges <- function(obj, ...) UseMethod("countEdges")
countEdges.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

countEdges.graph <- function(graph) {
  return(list(graph = graph, result = length(graph$nodes)))
}

#' A Graph Function
#' 
#' This functions counts the edges.
#' @param graph the graph obj
#' @return a list containing the graph and the number of edges
countEdges.adjMatrixGraph <- function(graph) {
  
}

isConnected <- function(obj, ...) UseMethod("isConnected")
isConnected.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

isFullyConnected <- function(obj, ...) UseMethod("isFullyConnected")
isFullyConnected.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

