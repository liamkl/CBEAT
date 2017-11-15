library(igraph)
library(dplyr)

nodes <- nodes[!duplicated(nodes$bioguideId), ]

nodes.senate <- filter(nodes, chamber_id == 2)
edges.senate <- filter(edges, 
       from %in% nodes.senate$bioguideId,
       to %in% nodes.senate$bioguideId)

net.sen <- graph_from_data_frame(d = edges.senate, 
                                    vertices = nodes.senate,
                                    directed = TRUE)
net.sen <- simplify(net.sen, edge.attr.comb = list(weight = "sum", "ignore"))
net.senate <- delete.edges(net.sen, E(net.sen)[weight < quantile(E(net.sen)$weight, 0.90)])
colrs <- c("D" = "blue", "I" = "green", "R" = "red")
V(net.senate)$color <- colrs[V(net.senate)$party]
V(net.senate)$size <- 4
V(net.senate)$label <- ""
E(net.senate)$arrow.size <- 0.1
E(net.senate)$width <- E(net.senate)$weight / 50
plot(net.senate, layout = layout_with_kk)

degree.distribution(net.senate) %>% hist()
     

net.senate %>% V()
