library(igraph)
library(tidyverse)

nodes <- read.csv("./data/output_data/nodes.csv")
edges <- read.csv("./data/output_data/edges.csv")

G <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
G <- simplify(G, edge.attr.comb = list(weight = "sum", "ignore"))
G_red <- delete.edges(G, E(G)[weight < quantile(E(G)$weight, 0.90)])

colrs <- c("D" = "blue", "I" = "green", "R" = "red")
# V(G_red)$color <- colrs[V(G_red)$party]
# V(G_red)$size <- 4
# V(G_red)$label <- ""
# E(G_red)$arrow.size <- 0.1
# E(G_red)$width <- E(G_red)$weight / 50
# plot(G, layout = layout_with_kk)
betweenness(G) %>% View

# Calculate in and out degree, add to nodes DF
deg_out <- degree(G, mode = "out")
deg_in <- degree(G, mode = "in")
nodes <- nodes %>% cbind(deg_out, deg_in)

total_in <- strength(G, mode = "in", weights = E(G)$weight)
total_out <- strength(G, mode = "out", weights = E(G)$weight)
nodes <- nodes %>% cbind(total_in, total_out)


D <- induced_subgraph(G, (V(G)$party == "D") | V(G)$party == "I")
total_in.d <- data.frame(bioguideId = V(D)$name,
                           total_in.d = strength(D, mode = "in", weights = E(D)$weight))
total_out.d <- data.frame(bioguideId = V(D)$name,
                            total_out.d = strength(D, mode = "out", weights = E(D)$weight))
nodes <- nodes %>% left_join(total_in.d) %>% left_join(total_out.d)
  
  
R <- induced_subgraph(G, V(G)$party == "R")
total_in.r <- data.frame(bioguideId = V(R)$name,
                           total_in.r = strength(R, mode = "in", weights = E(R)$weight))
total_out.r <- data.frame(bioguideId = V(R)$name,
                            total_out.r = strength(R, mode = "out", weights = E(R)$weight))
nodes <- nodes %>% left_join(total_in.r) %>% left_join(total_out.r) %>%
  mutate(total_in_party = coalesce(total_in.d, total_in.r),
         total_out_party = coalesce(total_out.d, total_out.r)) %>%
  select(-total_in.d, -total_in.r, -total_out.d, -total_out.r)

nodes <- nodes %>% mutate(total_in_nonparty = total_in - total_in_party, 
                          total_out_nonparty = total_out - total_out_party) %>%
  filter(bioguideId != "K000148") # Filter out John Kerry

# 
write_csv(nodes, paste0(getwd(), "/data/output_data/nodes_proc.csv"))
