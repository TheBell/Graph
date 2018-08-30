# Utility functions for parsing test files

#' A function to parse the header of a graph
#' 
#' @param con A connection to a test file
#' @return a graph object with base attributes set
buildGraph <- function(con) {
  weighted <- FALSE
  directed <- FALSE
  
  # Read through header
  while (line != "begin") {
    if (length(line) == 0) break  # EOF
    
    line <- readLines(con, n = 1)
    
    if (grepl("^\\*.*$", line)) next  # skip comments
    
    if (grepl("^weighted", line)) weighted <- TRUE
    
    if (grepl("^directed", line)) directed <- TRUE
  }
  
  # Get Nodes
  line <- readLines(con, n = 1)  
  nodes <- unlist(strsplit(line, split = " "))

  return(graph(nodes, weighted = weighted, directed = directed))
}

#' A function to add edges from test file into graph
#' 
#' @param con A connection to a test file
#' @return a graph object with the edges added
addEdges <- function(con, graph) {
  while (TRUE) {
    line <- readLines(con, n = 1)
    
    if (length(line) == 0) break  # EOF
    if (line == "end") break
    
    weight <- 1  # Default for edges, if not specified
    
    edge <- unlist(strsplit(line, split = " "))
    
    if (length(edge) > 3) weight <- edge[3]

    graph <- addEdge(graph, edge[1], edge[2], weight)
  }
  
  return(graph)
}


#'A function to run the tests in the file
#'
#'@param obj the object to run the method on
#'@param methodStr a string with the name of the method
#'@param expected the expected output
#'@param ... parameters to pass to the method
runTest <- function(obj, methodStr, expected, ...) {
  
  fun <- get(methodStr)
  result <- fun(obj, ...)
  
  expect_equal(result, expected)
}
