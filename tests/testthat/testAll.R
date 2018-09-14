context("Run tests on all .txt files in directory")
library(Graph)

pathToGraph <- "C:/Programming/Algorithms/Graph"

# Load in utility test functions
source(paste(pathToGraph, "/tests/testthat/TestFunctions.R", sep=""))

setwd(paste(pathToGraph, "/tests/testthat/TestFiles", sep = ""))
files <- list.files(path=".", pattern = ".txt")

runTestFile <- function(file) {

  con <- file(file, "r")
  on.exit(close(con))

  testGraph <- buildGraph(con)
  testGraph <- addEdges(con, testGraph)

  # Run Tests
  while (TRUE) {
    lines <- readLines(con, n = 2)

    if (length(lines) <= 1) break  # EOF

    fun <- unlist(strsplit(lines[1], split = " "))

    result <- lines[2]

    if (result == "true") result <- TRUE
    if (result == "false") result <-  FALSE
    testGraph <- runTest(testGraph, fun[1], result, fun[-1])
  }

  # on.exit(close(con))
  return(TRUE)
}

# runTestFile(files[2])

sapply(files, FUN = runTestFile)
