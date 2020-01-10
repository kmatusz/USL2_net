
library('visNetwork') 
library(igraph)
library(tidyverse)
library(factoextra)

# Dragging one point (simulation) -----
nodes <- tribble(~id, ~color, ~label,
                1, NA, NA,
                2, NA, NA,
                3, "red", "Drag me",
                4, NA, NA
                )

edges <- tribble(~from, ~to,
                 1, 2,
                 2, 3,
                 1, 3,
                 3, 4
                 )

visNetwork(nodes, edges)


# LAyouts comparison
net <- sample_pa(100) 
V(net)$size <- 8
V(net)$frame.color <- NA
V(net)$color <- "red"
V(net)$label <- "" 
E(net)$arrow.mode <- 0

par(mfrow = c(1, 4))
plot(net, layout = layout_randomly, main = "Random")
plot(net, layout = layout_with_fr, main = "Fruchterman-Reingold")
plot(net, layout = layout_with_kk, main = "Kamanda-Kawai")
plot(net, layout = layout_with_lgl, main = "Large-Graph-Layout")


# Prepare the dataset (load etc)----

# Load the data
temp <- new.env()
load("data/clean_datasets.Rdata", envir = temp)
general_info <- temp$general_info
similar_artists <- temp$similar_artists

# Print the datasets
general_info %>% 
  arrange(-playcount) %>% 
  head()


similar_artists %>%
  head(10)


#' Create igraph object
 
# Remove artists not avaliable in the database
similar_artists <- similar_artists %>%
  drop_na() %>%
  filter(similar %in% general_info$name) 

general_info %>%
  select(-listeners) -> general_info

# Create graph
artist_graph <- graph_from_data_frame(similar_artists, 
                                vertices = general_info)

# Convert to undirected
artist_graph %>% 
  as.undirected(mode = "collapse") -> artist_graph

# Select biggest component of the graph
comp <- components(artist_graph)
biggest_subgraph <- delete_vertices(artist_graph, 
                                    V(artist_graph)[comp[["membership"]] != 1])

artist_graph <- biggest_subgraph

#' Here some stats about the dataset

#
# Computing the coordinates-----

# # Fruchterman-Reingold (most popular)
# set.seed(10)
# message("fr")
# message(Sys.time())
# coords_fr <- layout_with_fr(artist_graph, niter = 5000)
# 
# #  Kamada-Kawai (other springs)
# set.seed(10)
# message("fr")
# message(Sys.time())
# # coords_kk <- layout_with_kk(artist_graph)
# 
# # LGL (for large graphs)
# set.seed(10)
# message("lgl")
# message(Sys.time())
# coords_lgl <- layout_with_lgl(artist_graph, maxiter = 500)
# 
# # Random
# set.seed(10)
# message("rand")
# message(Sys.time())
# coords_rand <- layout_randomly(artist_graph)
# 
# 
# # Obtain distance matrix
# dist_graph <- distances(artist_graph)
# dist_graph[is.infinite(dist_graph)] <- 1000000 # convert from infinity
# 
# dist_graph <- as.dist(dist_graph)
# 
# # calculate MDS
# set.seed(10)
# message("mds")
# message(Sys.time())
# coords_mds <- cmdscale(dist_graph)
# 
# # Calculate t-SNE
# set.seed(10)
# message("tsne")
# message(Sys.time())
# coords_tsne <- Rtsne::Rtsne(dist_graph, is_distance = T, pca = F)
# message(Sys.time())
# 
# save(coords_fr, 
#      coords_lgl, 
#      coords_rand, 
#      dist_graph, 
#      coords_mds, 
#      #coords_tsne, 
#      file = "data/coords_computed.Rdata")




# Load the coordinates from file
load("data/coords_computed.Rdata")

# Extract tsne
coords_tsne <- coords_tsne$Y

graph_to_plot <- artist_graph
V(graph_to_plot)$label <- NA

# Quick plot ----
plot(graph_to_plot, 
     layout = coords_tsne, 
     vertex.color = "#6c34358c",
     vertex.size = 1,
     vertex.frame.color = NULL,
     margin = c(0,0,0,0),
     edge.color = "#0000008c",
     edge.width = 0.5
     )


# General k-means-----

# Najlepiej dla mds i 3 klastrów (i random, sic!)

coords_df <- as_tibble(coords_mds)
sil_plot <- fviz_nbclust(coords_df, kmeans, method = "silhouette")
# wss_plot <- fviz_nbclust(coords_df, kmeans, method = "wss", k.max = 20)
sil_plot
sil_plot$data
# wss_plot$data

km <- kmeans(coords_df, 3)
fviz_cluster(km, data = coords_df, geom = "point")
fviz_silhouette(eclust(km))

eclust(coords_df, "kmeans", 30) %>% fviz_silhouette()
 

