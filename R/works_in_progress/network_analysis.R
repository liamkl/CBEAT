library(igraph)
library(dplyr)


G <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
G <- simplify(G, edge.attr.comb = list(weight = "sum", "ignore"))
G_red <- delete.edges(G, E(G)[weight < quantile(E(G)$weight, 0.90)])

colrs <- c("D" = "blue", "I" = "green", "R" = "red")
V(G_red)$color <- colrs[V(G_red)$party]
V(G_red)$size <- 4
V(G_red)$label <- ""
E(G_red)$arrow.size <- 0.1
E(G_red)$width <- E(G_red)$weight / 50
plot(G_red, layout = layout_with_kk)

degree.distribution(G_red) %>% hist()