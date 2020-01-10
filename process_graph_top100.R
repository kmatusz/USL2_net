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


# Create subgraph of top 100 artists + neighbors ----
a = tibble(playcount = V(artist_graph)$playcount) 
a %>% arrange(-playcount) %>% 
  head(100) %>%
  .$playcount -> top_playcounts
top_artists <- V(artist_graph)[V(artist_graph)$playcount %in% top_playcounts]

ego(artist_graph, top_artists, order = 1) -> neighbor_list
  
neighbor_list %>% 
  reduce(c) -> neighbors_list_v

v_list <- c(neighbors_list_v, top_artists)

artist_graph

top_graph <- induced_subgraph(artist_graph, v_list)

unique(v_list$name)

V(artist_graph)[!(V(artist_graph)$name %in% v_list$name)]

top_graph <- delete_vertices(artist_graph, 
                                    V(artist_graph)[!(V(artist_graph)$name %in% v_list$name)])
# Around 6618 items


# Prepare layout 


temp <- visNetwork::toVisNetworkData(top_graph, idToLabel = TRUE)
nodes <- temp$nodes
edges <- temp$edges

# Add options 
nodes$size <- c(rep(10, 100), rep(2, 6518))
nodes$label <- NA
potential_colors <- c("top10" = "rgba(255, 0, 0, 1)", "others" = "rgba(255, 0, 0, 0.7)")
# nodes$color <- potential_colors[1]
nodes <- nodes %>% arrange(-playcount) %>%
  mutate(rank = row_number(),
         if_top10 = ifelse(rank<11, T, F))

nodes$color <- ifelse(nodes$if_top10, "red", NA)

visNetwork(nodes, edges, width = "100%") %>%
  visNodes(font = list(size = 30)) %>%
  visEdges(width = 1) %>%
  visIgraphLayout(layout = "layout_with_kk") -> temp_graph

coords <- layout_with_fr(top_graph)

l <- norm_coords(coords, 
                 xmin = -1, 
                 xmax = 1, 
                 ymin = -1,
                 ymax = 1)



temp_graph$x$nodes$x <- l[,1]
temp_graph$x$nodes$y <- l[,2]
temp_graph
# temp_graph %>% visSave(file = "export_net.html")

