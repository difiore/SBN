library(igraph)
library(visNetwork)
g <- read_graph(file.choose(), format = "graphml")
plot(g)

V(g)$color <- as.numeric(as.factor(vertex_attr(g, "sex")))

plot(g)
plot_dd(g)

gVN <- toVisNetworkData(g)
visIgraph(g, idToLabel = TRUE)
visNetwork(nodes = gVN$nodes, edges = gVN$edges)
