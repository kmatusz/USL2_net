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

# Sample visualisation ----

temp <- visNetwork::toVisNetworkData(artist_graph, idToLabel = TRUE)

nodes <- temp$nodes
edges <- temp$edges

visNetwork(nodes, edges)


# Create subgraph from neighbourhood of some artist ----
artist <- "Łąki Łan"
order <- 2 

neighborhood_graph <- make_ego_graph(artist_graph, 
               order = order, 
               nodes = V(artist_graph)[artist], 
               mode = c("all")) %>% .[[1]]
  
# Star layout
# plot(neighborhood_graph, layout = layout_as_star(graph = neighborhood_graph, 
#                                                  center = V(neighborhood_graph)[artist]))

# Convert to nodes and edges
temp <- visNetwork::toVisNetworkData(neighborhood_graph, idToLabel = TRUE)
nodes <- temp$nodes
edges <- temp$edges

# Add options 
nodes$title = sprintf("%s, </br> playcount: %s", nodes$label, nodes$playcount)
nodes$font.size <- 30
# nodes$size <- 30#log(nodes$playcount)
nodes$color <- ifelse(nodes$id == artist, "red", NA)
nodes$size <- ifelse(nodes$id == artist, 40, 30)

visNetwork(nodes, edges) %>%
  visNodes(font = list(size = 30)) %>%
  visEdges(width = 4)

# Compare layouts using different algos ----




# full graph analysis (shit) ----
# From calc_distances.R
load("data/coords_computed.Rdata")

# Convert to nodes and edges
temp <- visNetwork::toVisNetworkData(artist_graph, idToLabel = TRUE)
nodes <- temp$nodes
edges <- temp$edges

# Add options 
nodes$title = sprintf("%s, </br> playcount: %s", nodes$label, nodes$playcount)
# nodes$font.size <- 30
# nodes$size <- 30#log(nodes$playcount)
# nodes$color <- ifelse(nodes$id == artist, "red", NA)
nodes$size <- 10
nodes$label <- NA

visNetwork(nodes, edges) %>%
  visNodes(font = list(size = 30)) %>%
  visEdges(width = 0, hidden = T) %>%
  visIgraphLayout(layout = "layout_randomly") -> temp_graph

l <- norm_coords(coords_fr, xmin = -1, xmax = 1, ymin = -1)
l <- coords_lgl
temp_graph$x$nodes$x <- l[,1]
temp_graph$x$nodes$y <- l[,2]
temp_graph %>% visSave(file = "export_net.html")

# Nothings visible

# Select only biggest subgraph (without recalculating coords) ----
# Create visNetwork
temp <- visNetwork::toVisNetworkData(biggest_subgraph, idToLabel = TRUE)
nodes <- temp$nodes
edges <- temp$edges

# Add options 
nodes$title = sprintf("%s, </br> playcount: %s", nodes$label, nodes$playcount)
# nodes$font.size <- 30
# nodes$size <- 30#log(nodes$playcount)
# nodes$color <- ifelse(nodes$id == artist, "red", NA)
nodes$size <- 2
nodes$label <- NA
# nodes$color <- RColorBrewer::brewer.pal(5, "Set1")[kmeans(l, centers = 5)$cluster]
nodes$color <- "rgba(255, 0, 0, 0.1)"

visNetwork(nodes, edges) %>%
  visNodes(font = list(size = 30)) %>%
  visEdges(width = 0.001) %>%
  visIgraphLayout(layout = "layout_randomly") -> temp_graph



# Here change
coords <- layout_with_fr(biggest_subgraph, niter = 5000)
l <- norm_coords(coords_kk, 
                 xmin = -1, 
                 xmax = 1, 
                 ymin = -1,
                 ymax = 1)


temp_graph$x$nodes$x <- l[,1]
temp_graph$x$nodes$y <- l[,2]
temp_graph %>% visSave(file = "export_net.html")
# temp_graph

# Calc t-SNE

# Obtain distance matrix
dist_graph <- distances(biggest_subgraph)
dist_graph[is.infinite(dist_graph)] <- 1000000 # convert from infinity
dist_graph <- as.dist(dist_graph)

coords_tsne <- tsne::tsne(dist_graph, max_iter = 100, epoch = 1, whiten = F)
t <- Rtsne::Rtsne(dist_graph, is_distance = T, pca = F)

# COnnecting more vertices ----
biggest_subgraph_connected <- connect(biggest_subgraph, 3)


temp <- visNetwork::toVisNetworkData(biggest_subgraph_connected, idToLabel = TRUE)
nodes <- temp$nodes
edges <- temp$edges

# Add options 
nodes$title = sprintf("%s, </br> playcount: %s", nodes$label, nodes$playcount)
# nodes$font.size <- 30
# nodes$size <- 30#log(nodes$playcount)
# nodes$color <- ifelse(nodes$id == artist, "red", NA)
nodes$size <- 1
nodes$label <- NA

visNetwork(nodes, edges) %>%
  visNodes(font = list(size = 30)) %>%
  visEdges(width = 0.5) %>%
  visIgraphLayout(layout = "layout_randomly") -> temp_graph

coords_lgl <- layout_with_lgl(biggest_subgraph_connected)

l <- norm_coords(coords_lgl, 
                 xmin = -1, 
                 xmax = 1, 
                 ymin = -1,
                 ymax = 1)


temp_graph$x$nodes$x <- l[,1]
temp_graph$x$nodes$y <- l[,2]
temp_graph %>% visSave(file = "export_net.html")

# For creating sparser graph function connect
# tkplot(neighborhood_graph_1)
# make sparser graph
# neighborhood_graph_connected <- connect(neighborhood_graph, order = 2) 
