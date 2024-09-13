library(tidyverse)
library(igraph)

f <- file.choose() # this will open a dialog box... then select the file "asoiaf-book1-edges.csv"
e <- read_csv(f, col_names = TRUE)
edges <- e
edges <- filter(edges, weight >= 50)
vertices <- data.frame(
  id = unique(c(edges$Source,edges$Target)),
  label = unique(c(edges$Source, edges$Target))
)
edges <- rename(edges, from = Source, to = Target, type = Type)
edges <- mutate(edges,
                width = weight / max(weight) * 20,
                label = weight,
                title = paste("Weight:", weight)
)

graph <- graph_from_data_frame(
  d = edges,
  vertices = vertices,
  directed = FALSE)
graph

library(visNetwork)
visIgraph(graph)

vngraph <- toVisNetworkData(graph, idToLabel = TRUE)
visNetwork(vngraph$nodes, vngraph$edges)

b <- betweenness(
  graph,
  directed = FALSE
)

b <- tibble(name=names(b),value=b)
