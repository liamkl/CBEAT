library(igraph)

nodes <- nodes[!duplicated(nodes$bioguideId), ]
net <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
net <- simplify(net, edge.attr.comb = list(weight = "sum", "ignore"))

V(net)$size <- 8

V(net)$frame.color <- "white"

V(net)$color <- "orange"

V(net)$label <- ""
E(net)$width


mean(E(net)$weight)
quantile(E(net)$weight, 0.80)

cutoff <- mean(E(net)$weight)
net.sp <- delete_edges(net, E(net)[weight < cutoff])
plot(net.sp, layout = layout_in_circle)
