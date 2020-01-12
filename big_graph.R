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
load("data/coords_kk.Rdata")

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
nodes <- nodes %>% arrange(-playcount)
nodes$label <- NA
# nodes$size <- c(rep(10, 100), rep(2, 6518))
potential_colors <- RColorBrewer::brewer.pal(8, "Set1")
# nodes$color <- potential_colors[1]
# nodes$color <- c(rep(potential_colors[1], 100), rep(potential_colors[2], 6518))
nodes$group <- c(rep("top 100", 100), rep("rest", 6518))

visNetwork(nodes, edges, 
           width = "100%", 
           height = "600px",
           background = "black"
           ) %>%
  visNodes(font = list(size = 30)) %>%
  visEdges(width = 0.001) %>%
  visGroups(groupname = "top 100", color = potential_colors[1]) %>%
  visGroups(groupname = "rest", color = adjustcolor(potential_colors[2], alpha.f = 0.1)) %>%
  visIgraphLayout(layout = "layout_randomly") -> temp_graph


temp_graph$x$nodes$x <- l[,1]
temp_graph$x$nodes$y <- l[,2]
temp_graph %>% visSave(file = "export_net.html")



# For all layouts 
coords_fr <- norm_coords(coords_fr, xmin = -2, xmax = 2, ymin = -1, ymax = 1)
coords_lgl <- norm_coords(coords_lgl, xmin = -2, xmax = 2, ymin = -1, ymax = 1)
coords_rand <- norm_coords(coords_rand, xmin = -2, xmax = 2, ymin = -1, ymax = 1)
coords_mds <- norm_coords(coords_mds, xmin = -2, xmax = 2, ymin = -1, ymax = 1)
coords_tsne <- norm_coords(coords_tsne, xmin = -2, xmax = 2, ymin = -1, ymax = 1)
coords_kk <- norm_coords(coords_kk, xmin = -2, xmax = 2, ymin = -1, ymax = 1)


coords_list <- list(
  coords_fr,
  coords_lgl,
  coords_rand,
  coords_mds,
  coords_tsne,
  coords_kk
) 

types <- c("fr", "lgl", "rand", "mds", "tsne", "kk")
names(coords_list) <- types

for (i in names(coords_list)){
  message(i)
  temp_graph$x$nodes$x <- coords_list[[i]][,1]
  temp_graph$x$nodes$y <- coords_list[[i]][,2]
  temp_graph %>% visSave(file = paste0(i, "_plot.html"))
}


temp_graph$x$nodes$x <- l[,1]
temp_graph$x$nodes$y <- l[,2]
temp_graph %>% visSave(file = "export_net.html")



# k-means plot ----
coords_df <- as_tibble(coords_tsne)
set.seed(10)
km <- kmeans(coords_df, centers = 3)



temp <- visNetwork::toVisNetworkData(biggest_subgraph, idToLabel = TRUE)
nodes <- temp$nodes
edges <- temp$edges

# Add options 
nodes$group <- as.character(km$cluster)
nodes$label <- NA
# nodes$size <- c(rep(10, 100), rep(2, 6518))
potential_colors <- adjustcolor(RColorBrewer::brewer.pal(8, "Set1"), alpha.f = 0.1)
# nodes$color <- potential_colors[1]
# nodes$color <- c(rep(potential_colors[1], 100), rep(potential_colors[2], 6518))
# nodes <- nodes %>% arrange(-playcount)
nodes$group

visNetwork(nodes, edges, 
           width = "100%", 
           height = "600px",
           background = "black"
) %>%
  visNodes(font = list(size = 30)) %>%
  visEdges(width = 0.001) %>%
  visGroups(groupname = '1', color = potential_colors[2]) %>%
  visGroups(groupname = "2", color = potential_colors[5]) %>%
  visGroups(groupname = "3", color = potential_colors[7]) %>%
  visIgraphLayout(layout = "layout_randomly") -> temp_graph


temp_graph$x$nodes$x <- coords_tsne[,1]
temp_graph$x$nodes$y <- coords_tsne[,2]
temp_graph %>% visSave(file = "tsne_cluster.html")

