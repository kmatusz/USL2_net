message(Sys.time())

library(tidyverse)
library(igraph)
library(visNetwork)

load("data/clean_datasets.Rdata")


# Preparation

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

comp <- components(artist_graph)
biggest_subgraph <- delete_vertices(artist_graph, 
                                    V(artist_graph)[comp[["membership"]] != 1])

artist_graph <- biggest_subgraph

# Compute coordinates for various methods ----


# Fruchterman-Reingold (most popular)
set.seed(10)
message("fr")
message(Sys.time())
coords_fr <- layout_with_fr(artist_graph, niter = 5000)

#  Kamada-Kawai (other springs)
set.seed(10)
message("fr")
message(Sys.time())
coords_kk <- layout_with_kk(artist_graph)

# LGL (for large graphs)
set.seed(10)
message("lgl")
message(Sys.time())
coords_lgl <- layout_with_lgl(artist_graph)

# Random
set.seed(10)
message("rand")
message(Sys.time())
coords_rand <- layout_randomly(artist_graph)


# Obtain distance matrix
dist_graph <- distances(artist_graph)
dist_graph[is.infinite(dist_graph)] <- 1000000 # convert from infinity

dist_graph <- as.dist(dist_graph)

# calculate MDS
set.seed(10)
message("mds")
message(Sys.time())
coords_mds <- cmdscale(dist_graph)

# Calculate t-SNE
set.seed(10)
message("tsne")
message(Sys.time())
coords_tsne <- tsne::tsne(dist_graph, max_iter = 2)
message(Sys.time())

save(coords_fr, 
     coords_lgl, 
     coords_rand, 
     dist_graph, 
     coords_mds, 
     #coords_tsne, 
     file = "data/coords_computed.Rdata")

