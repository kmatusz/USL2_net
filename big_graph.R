library(tidyverse)
library(igraph)
library(visNetwork)

# Preparation ----
load("data/clean_datasets.Rdata")


# TODO: Add tags for artists

# Create igraph object
similar_artists <- similar_artists %>%
  drop_na() %>%
  filter(similar %in% general_info$name) 

general_info %>%
  select(-listeners) -> general_info


artist_graph <-
  igraph::graph_from_data_frame(similar_artists, vertices = general_info)

artist_graph %>% 
  as.undirected(mode = "collapse") -> artist_graph


# Create biggest connected subgraph ----
comp <- components(artist_graph)
biggest_subgraph <- delete_vertices(artist_graph, 
                                    V(artist_graph)[comp[["membership"]] != 1])
# Around 6618 items


# Prepare layout 

# Load the coordinates from file
load("data/coords_computed.Rdata")

# Extract tsne
coords_tsne <- coords_tsne$Y

#MDS nicest, but tsne provides good info about 100 best artists 
l <- norm_coords(coords_tsne, 
                 xmin = -2, 
                 xmax = 2, 
                 ymin = -1,
                 ymax = 1)

temp <- visNetwork::toVisNetworkData(biggest_subgraph, idToLabel = TRUE)
nodes <- temp$nodes
edges <- temp$edges

# Add options 
nodes$size <- c(rep(10, 100), rep(2, 6518))
nodes$label <- NA
potential_colors <- c("rgba(118, 120, 237, 0.15)", "rgba(255, 0, 0, 0.7)")
# nodes$color <- potential_colors[1]
nodes <- nodes %>% arrange(-playcount)
nodes$color <- c(rep(potential_colors[2], 100), rep(potential_colors[1], 6518))

visNetwork(nodes, edges, width = "100%", height = "600px",
           background = "black") %>%
  visNodes(font = list(size = 30)) %>%
  visEdges(width = 0.001) %>%
  visIgraphLayout(layout = "layout_randomly") -> temp_graph


temp_graph$x$nodes$x <- l[,1]
temp_graph$x$nodes$y <- l[,2]
temp_graph %>% visSave(file = "export_net.html")

