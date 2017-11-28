library(igraph)
library(tidyverse)
library(grDevices)

###################
## Data read-in/processing
###################
nodes <- read.csv("./data/output_data/nodes_proc.csv")
simp_edges <- read.csv("./data/output_data/simplified_edges.csv")


### Example bioguideId set here ### 
sen_ID <- "R000146" # Claire Mcaskill

# Filter edges to get ones we want
senator_edges <- simp_edges %>% 
  select(-X) %>%
  filter((from == sen_ID | to == sen_ID))
senator_edges <- senator_edges[order(senator_edges$weight,decreasing = TRUE)[1:15],]
# Then subset nodes
nodes_subset <- nodes %>%
  filter((bioguideId %in% senator_edges$to | bioguideId %in% senator_edges$from))
# Create graph
G <- graph_from_data_frame(d = senator_edges, vertices = nodes_subset, directed = TRUE)
###################
###################
## Colors
###################
alpha <- 0.8
red <- rgb(0.82, 0.305, 0.305, alpha = alpha)
blue <- rgb(0.258, 0.345, 0.803, alpha = alpha)
green <- rgb(0.270, 0.709, 0.396, alpha = alpha)
colrs <- c("D" = blue, "I" = green, "R" = red)
V(G)$color <- colrs[V(G)$party]
edge_colors <- 1 - 0.6 * (E(G)$weight / max(E(G)$weight))
E(G)$color <- rgb(edge_colors, edge_colors, edge_colors)
###################
###################
## Other graph parameters
###################
# Node labels
V(G)$label <- paste0(V(G)$last_name, ", ", V(G)$first_name, 
                     "\n (", V(G)$party, "-", V(G)$state, ")")
E(G)$width <- E(G)$weight
E(G)$arrow.size <- (E(G)$weight / max(E(G)$weight))
E(G)$curved <- 0.5 * (E(G)$weight / max(E(G)$weight))
L <- layout_with_gem(G)
L <- norm_coords(L, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
###################
###################
## Plot
###################
plot(G,
     vertex.label.family = "sans",
     vertex.label.font = 2,
     vertex.label.dist = 0,
     vertex.label.color = "white",
     vertex.label.cex = 0.95,
     vertex.size = 40,
     arrow.mode = 2,
     layout = L * 1, 
     rescale = FALSE)