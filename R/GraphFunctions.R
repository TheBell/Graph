

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
    graphObj$nodes[i] <- nodes[i]
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
  graphObj <- list(weighted = weighted,
                   directed = directed,
                   nodes = c())

  if (!is.vector(nodes)) stop("Nodes must be a vector.")

  for (i in 1:length(nodes)) {
    graphObj$nodes[i] <- nodes[i]
  }

  names(graphObj$nodes) <- nodes

  if (length(nodes) < 1) {
    graphObj$edges <-  NULL
    return(graphObj)
  }

  edges <- as.list(graphObj$nodes)

  for (i in 1:length(edges)) {
    edges[[i]] <- pairlist(c(nodes[i], 0))
  }

  graphObj$edges <- edges

  class(graphObj) <- c("adjListGraph", "graph")

  return(graphObj)
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

#' A Graph Function
#'
#' This function allows you to check for if an edge exists between 2 nodes in the graph.
#' @param node1 The first node
#' @param node2 The second node
#' @return a list containing the graph object and the a boolean indicating if the graph contained the edge
hasEdge.adjListGraph <- function(graph, node1, node2) {
  r <- list(graph = graph, result = FALSE)
  if (!node1 %in% graph$nodes | !node2 %in% graph$nodes) return(r)
  else {
    edges <- graph$edges[[node1]]

    for (i in 1:length(edges)) {
      if (edges[[i]][1] == node2 & edges[[i]][2] > 0) {
        r$result = TRUE
        return(r)
      }
    }
  }
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

#' A Graph Function
#'
#' This function allows you to add a new edge to an existing graph. If weighted graph,
#' will override previous edge.
#' @param node1 The first node
#' @param node2 The second node
#' @param weight Defaults to 1 (used for non-weighted graph)
#' @return the new graph with the added edge and whether the operation was successful
addEdge.adjListGraph <- function(graph, node1, node2, weight = 1) {
  r <- list(graph = graph, result = TRUE)

  if (!node1 %in% graph$nodes | !node2 %in% graph$nodes) {
    r$result <- FALSE
    return(r)
  }

  graph$edges[[node1]] <- append(graph$edges[[node1]], list(c(node2, weight)))

  if (!graph$directed) graph$edges[[node2]] <- append(graph$edges[[node2]], list(c(node1, weight)))

  r$graph <- graph
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

#' A Graph Function
#'
#' This function deletes an edge.
#' @param graph the graph obj
#' @param node1 The first node
#' @param node2 The second node
#' @return a list containing the new graph and whether an operation was performed
deleteEdge.adjListGraph <- function(graph, node1, node2) {
  r <- list(graph = graph, result = TRUE)

  if (!node1 %in% graph$nodes | !node2 %in% graph$nodes) {
    r$result <- FALSE
    return(r)
  }

  if (!hasEdge(graph, node1, node2)$result) {
    r$result <- FALSE
    return(r)
  }

  if (node1 == node2) {
    r$result <- FALSE
    return(r)
  }

  edges <- graph$edges[[node1]]

  for (i in 2:length(edges)) {
    if (edges[[i]][1] == node2 & edges[[i]][2] != 0) {
      graph$edges[[node1]] <- graph$edges[[node1]][-i]
    }
  }

  if (!graph$directed) {
    edges <- graph$edges[[node2]]

    for (i in 2:length(edges)) {
      if (edges[[i]][1] == node1 & edges[[i]][2] != 0) {
        graph$edges[[node2]] <- graph$edges[[node2]][-i]
      }
    }
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

#' A Graph Function
#'
#' This function adds a vertex.
#' @param graph the graph obj
#' @param vertex the new vertex to add
#' @return a list containing the new graph and whether an operation was performed
addVertex.adjListGraph <- function(graph, vertex) {
  r <- list(graph = graph, result = TRUE)

  if (vertex %in% graph$nodes) {
    r$result <-  FALSE
    return(r)
  }

  graph$nodes <- c(graph$nodes, vertex)

  graph$edges[[vertex]] <- list(c(vertex, 0))

  r$graph <- graph

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

#' A Graph Function
#'
#' This function deletes a vertex.
#' @param graph the graph obj
#' @param vertex the vertex to delete
#' @return a list containing the new graph and whether an operation was performed
deleteVertex.adjListGraph <- function(graph, vertex) {
  r <- list(graph = graph, result = TRUE)

  if (!vertex %in% graph$nodes) {
    r$result <- FALSE
    return(r)
  }

  index <- which(graph$nodes == vertex)
  graph$nodes <- graph$nodes[-index]
  graph$edges <- graph$edges[-index] # Remove list of edges for vertex

  # Search through remaining vertices for edges
  for (i in 1:length(graph$edges)) {
    for (j in 1:length(graph$edges[[i]])) {
      if (graph$edges[[i]][j] == vertex) {
        graph$edges <- graph$edges[-i]
      }
    }
  }

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

#' A Graph Function
#'
#' This functions checks if the graph is sparse.
#' @param graph the graph obj
#' @param level the cutoff level for a sparse graph, defaul is .15
#' @return a list containing the graph and whether the graph is sparse
isSparse.adjListGraph <- function(graph, level = .15) {

  cutoff <- .15 * (length(graph$nodes)  ^ 2 - length(graph$nodes)) # Since no loop-back is allowed, subtract those possibilites

  if (graph$directed) {
    cutoff <- cutoff / 2
  }

  edges <- countEdges(graph)$result

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

#' A Graph Function
#'
#' This functions checks if the graph is dense.
#' @param graph the graph obj
#' @param level the cutoff level for a dense graph, defaul is .85
#' @return a list containing the graph and whether the graph is dense
isDense.adjListGraph <- function(graph, level = .15) {

  cutoff <- .85 * (length(graph$nodes)  ^ 2 - length(graph$nodes)) # Since no loop-back is allowed, subtract those possibilites

  if (graph$directed) {
    cutoff <- cutoff * 2
  }

  edges <- countEdges(graph)$result

  return(list(graph = graph, result = edges > cutoff))
}

countVertices <- function(obj, ...) UseMethod("countVertices")
countVertices.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#'
#' This functions counts the vertices.
#' @param graph the graph obj
#' @return a list containing the graph and the number of vertices
countVertices.graph <- function(graph) {
  return(list(graph = graph, result = length(graph$nodes)))
}

countEdges <- function(obj, ...) UseMethod("countEdges")
countEdges.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#'
#' This functions counts the edges.
#' @param graph the graph obj
#' @return a list containing the graph and the number of edges
countEdges.adjMatrixGraph <- function(graph) {
  return(list(graph = graph, result = sum(graph$edges > 0)))
}

#' A Graph Function
#'
#' This functions counts the edges.
#' @param graph the graph obj
#' @return a list containing the graph and the number of edges
countEdges.adjListGraph <- function(graph) {
  total <- 0

  vertices <- graph$edges

  for (i in 1:length(vertices)) {
    edges <- vertices[[i]]
    for (j in 1:length(edges)) {
      node <- edges[[j]]
      if (node[2] != 0) {
        total <- total + 1
      }
    }
  }
  return(list(graph = graph, result = total))
}

isConnected <- function(obj, ...) UseMethod("isConnected")
isConnected.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#'
#' This functions checks if the graph is connected.
#' @param graph the graph obj
#' @return a list containing the graph and whether the graph is connected
isConnected.adjMatrixGraph <- function(graph) {
  r <- list(graph = graph, result = TRUE)

  if (length(graph$nodes) <= 1) return(r)  # graph with 0 or 1 nodes

  visited <- c(graph$nodes[1])

  row <- graph$edges[1,]

  neighbors <- c()

  # Get neighbors for starting node
  for (i in 2:length(row)) {
    if (row[i] != -1) {
      neighbors <- c(neighbors, graph$nodes[i])
    }
  }

  while (length(neighbors) > 0) {
    current <- neighbors[1]
    visited <- c(visited, current)
    neighbors <- neighbors[-1]

    row <- graph$edges[current,]

    for (i in 1:length(row)) {

      if (row[i] == current) next

      node <- graph$nodes[i]
      if (row[i] != -1 & !(node %in% visited)) {
        neighbors <- c(neighbors, node)
      }
    }
  }


  if (length(visited) < length(graph$nodes)) r$result <- FALSE

  return(r)

}

#' A Graph Function
#'
#' This functions checks if the graph is connected.
#' @param graph the graph obj
#' @return a list containing the graph and whether the graph is connected
isConnected.adjListGraph <- function(graph) {
  r <- list(graph = graph, result = TRUE)

  if (length(graph$nodes) <= 1) return(r)  # graph with 0 or 1 nodes

  current <- graph$nodes[1]
  visited <- c(current)
  neighbors <- c()

  # Get starting neighbors
  edges <- graph$edges[[1]]
  for (i in 2:length(edges)) {
    if (edges[[i]][2] != 0) neighbors <- c(neighbors, edges[[i]][1])
  }

  while (length(neighbors) > 0) {
    current <- neighbors[1]
    visited <- c(visited, current)
    neighbors <- neighbors[-1]

    edges <- graph$edges[[current]]
    for (i in 2:length(edges)) {
      if (edges[[i]][2] != 0 & !(edges[[i]][1] %in% visited)) {
        neighbors <- c(neighbors, edges[[i]][1])
      }
    }
  }

  if (length(visited) < length(graph$nodes)) r$result <- FALSE

  return(r)
}

isFullyConnected <- function(obj, ...) UseMethod("isFullyConnected")
isFullyConnected.default <- function(obj, ...) print(paste("Method for class not found:", class(obj)))

#' A Graph Function
#'
#' This functions checks if the graph is fully connected.
#' @param graph the graph obj
#' @return a list containing the graph and whether the graph is fully connected
isFullyConnected.adjMatrixGraph <- function(graph) {
  r <- list(graph = graph, result = TRUE)

  for (i in 1:length(graph$nodes)) {
    for (j in 1:length(graph$nodes)) {
      if (i != j) {
        if (graph$edges[i, j] == 0) {
          r$result <- FALSE
          return(r)
        }
      }
    }
  }

  return(r)
}

#' A Graph Function
#'
#' This functions checks if the graph is fully connected.
#' @param graph the graph obj
#' @return a list containing the graph and whether the graph is fully connected
isFullyConnected.adjListGraph <- function(graph) {
  max_edges <- length(graph$edges) * (length(graph$edges) - 1)
  return(list(graph = graph, result = max_edges == countEdges(graph)$result))
}
