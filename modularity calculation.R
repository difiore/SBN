library(tidyverse)
library(igraph)

kite <- graph(c(1,2, 1,3, 2,3, 2,4, 3,4, 1,5, 5,6, 5,7, 5,8, 6,7, 6,8, 7,8), n = 8, directed = FALSE)
plot.igraph(kite)
partition <- c(1,1,1,1,2,2,2,2) # vector of partitions
modularity(kite, partition)

calculate_mod <- function(g, partition) {
  mat <- as_adjacency_matrix(g)
  k <- degree(g) # vector of degrees
  m <- ecount(g)
  mod <- 0
  for (i in 1:length(k)){
    for (j in 1:length(k)){
      x <- mat[i, j] - (k[i] * k[j])/(2 * m)
      x <- ifelse (partition[i] == partition[j], x * 1, x * 0)
      x <- x / (2 * m)
      mod <- mod + x
    }
  }
  return(mod)
}

calculate_mod(kite, partition)

g <- read_graph("data/harry_potter.gml", format = "gml")
V(g)$shape <- if_else(V(g)$sex == "M", "square", "circle")
V(g)$color <- if_else(V(g)$sex == "M", "red", "green")
plot(g)
V(g)$group <- if_else(V(g)$sex == "M", 1, 2)

partition <- V(g)$group
calculate_mod(g, partition)
modularity(g, partition)
