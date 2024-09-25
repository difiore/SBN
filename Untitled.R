i <- rep()
output <- map(i, .f = my_function(12, 12))

my_function <- function(n, m) {
  g <- sample_gnm(n = n, m = m)
  diam <- diameter(g)
  deg <-mean(degree(g))
  plot <- ggraph(g, layout = layout_in_circle(g)) +
    geom_edge_link() +
    geom_node_point(size = 1, shape=21, fill="#A70042") +
    theme(legend.position = "none")
  trans <- transitivity(g, type = "global")
  return(a = list(graph = g, diameter = diam, mean_degree = deg, plot = plot, transitivity = trans))
}


a<- my_function(12, 12)
